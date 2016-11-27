/**
 * @file
 *
 * @brief benchmark for an equivalent solution based on mysql
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <chrono>
#include <iostream>
#include <sstream>
#include <thread>
#include <vector>

#include <kdbthread.hpp>
#include <kdbtimer.hpp>

#include <cppconn/driver.h>
#include <cppconn/exception.h>
#include <cppconn/prepared_statement.h>
#include <cppconn/resultset.h>
#include <cppconn/statement.h>

#include <model_configformat.hpp>
#include <model_entry.hpp>
#include <model_importedconfig.hpp>
#include <model_user.hpp>
#include <service.hpp>
#include <testdata.hpp>
#include <timer.hpp>

#define SQL_HOST "tcp://127.0.0.1:3306"
#define SQL_USER "root"
#define SQL_PASS "root"
#define SQL_DB "test_rest_backend"

namespace kdbrest
{

namespace benchmark
{

void clearDatabase (sql::Connection * con)
{
	sql::Statement * stmt;
	stmt = con->createStatement ();

	// drop tables
	stmt->execute ("SET GLOBAL max_allowed_packet=1073741824"); // 1gb of data can be inserted at once
	stmt->execute ("SET foreign_key_checks = 0");
	stmt->execute ("DROP TABLE IF EXISTS `tags`");
	stmt->execute ("DROP TABLE IF EXISTS `snippets`");
	stmt->execute ("DROP TABLE IF EXISTS `users`");
	stmt->execute ("SET foreign_key_checks = 1");

	// create them again
	std::string tbl_users =
		"CREATE TABLE `users` (`id` INT NOT NULL AUTO_INCREMENT, `name` VARCHAR(20) NOT NULL, "
		"`password` VARCHAR(255) NOT NULL, `email` VARCHAR(255) NOT NULL, `rank` INT NOT NULL DEFAULT '10', "
		"`createdat` INT NOT NULL, PRIMARY KEY (`id`)) ENGINE=InnoDB";
	std::string tbl_snippets =
		"CREATE TABLE `snippets` (`id` INT NOT NULL AUTO_INCREMENT, "
		"`author_id` INT NOT NULL, `organization` VARCHAR(255) NOT NULL, `application` VARCHAR(255) NOT NULL, "
		"`scope` VARCHAR(255) NOT NULL, `slug` VARCHAR(255) NOT NULL, `title` VARCHAR(255) NOT NULL, "
		"`description` TEXT NOT NULL, `configuration` TEXT NOT NULL, `plugin` VARCHAR(100) NOT NULL, "
		"`views` INT NOT NULL DEFAULT '0', `createdat` INT NOT NULL, PRIMARY KEY (`id`), "
		"FOREIGN KEY (`author_id`) REFERENCES `users`(`id`)) ENGINE=InnoDB";
	std::string tbl_tags =
		"CREATE TABLE `tags` (`id` INT NOT NULL AUTO_INCREMENT, `snippet_id` INT NOT NULL, "
		"`tag` VARCHAR(20) NOT NULL, PRIMARY KEY (`id`), "
		"FOREIGN KEY (`snippet_id`) REFERENCES `snippets`(`id`)) ENGINE=InnoDB";

	stmt->execute (tbl_users);
	stmt->execute (tbl_snippets);
	stmt->execute (tbl_tags);

	delete stmt;
}

void prepareTestData (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry)
{
	// prepare all statements
	sql::Statement * stmt;
	stmt = con->createStatement ();

	// start transaction
	stmt->execute ("START TRANSACTION");

	// create users
	std::string sql = "INSERT INTO `users` (`name`,`password`,`email`,`rank`,`createdat`) VALUES ";
	std::vector<model::User> users = createTestUsers (numUsers);
	for (auto & user : users)
	{
		sql += "('" + user.getUsername () + "','" + user.getPasswordHash () + "','" + user.getEmail () + "'," +
		       std::to_string (user.getRank ()) + "," + std::to_string (user.getCreatedAt ()) + "),";
	}
	stmt->execute (sql.substr (0, sql.size () - 1));

	// create entries
	sql = "INSERT INTO `snippets` (`author_id`,`organization`,`application`,`scope`,`slug`,`title`,`description`,"
	      "`configuration`,`plugin`,`views`,`createdat`) VALUES ";
	int fk_id = 1;
	std::vector<model::Entry> entries = createTestEntries (users[0], 1, 1);
	model::ConfigFormat configFormat = service::ConvertEngine::instance ().exportTo (entries[0].getUploadPlugin (), entries[0]);
	for (auto & user : users)
	{
		entries = createTestEntries (user, numEntriesPerUser, numTagsPerEntry);
		for (auto & entry : entries)
		{
			sql += "(" + std::to_string (fk_id) + ",'" + entry.getOrganization () + "','" + entry.getApplication () + "','" +
			       entry.getScope () + "','" + entry.getSlug () + "','" + entry.getTitle () + "','" + entry.getDescription () +
			       "','" + configFormat.getConfig () + "','" + entry.getUploadPlugin () + "'," +
			       std::to_string (entry.getViews ()) + "," + std::to_string (entry.getCreatedAt ()) + "),";
		}
		fk_id++;
	}
	stmt->execute (sql.substr (0, sql.size () - 1));

	// create tags for entry
	sql = "INSERT INTO `tags` (`snippet_id`, `tag`) VALUES ";
	fk_id = 1;
	for (auto & user : users)
	{
		for (auto & entry : entries)
		{
			for (auto & tag : entry.getTags ())
			{
				sql += "(" + std::to_string (fk_id) + ", '" + tag + "'),";
			}
			fk_id++;
		}
	}
	stmt->execute (sql.substr (0, sql.size () - 1));

	// commit transaction
	stmt->execute ("COMMIT");

	// free all resources
	delete stmt;
}

void addIndexes (sql::Connection * con)
{
	sql::Statement * stmt;

	stmt = con->createStatement ();

	stmt->execute ("ALTER TABLE `users` ADD UNIQUE INDEX `name` (`name`)");

	stmt->execute ("ALTER TABLE `snippets` ADD INDEX `organization` (`organization`)");
	stmt->execute ("ALTER TABLE `snippets` ADD INDEX `application` (`application`)");
	stmt->execute ("ALTER TABLE `snippets` ADD INDEX `scope` (`scope`)");
	stmt->execute ("ALTER TABLE `snippets` ADD INDEX `slug` (`slug`)");
	stmt->execute (
		"ALTER TABLE `snippets` ADD UNIQUE INDEX `full_key` (`organization`, `application`,"
		"`scope`, `slug`)");
	stmt->execute ("ALTER TABLE `snippets` ADD FULLTEXT INDEX `title` (`title`)");
	stmt->execute ("ALTER TABLE `snippets` ADD FULLTEXT INDEX `description` (`description`)");
	stmt->execute ("ALTER TABLE `snippets` ADD INDEX `createdat` (`createdat`)");

	stmt->execute ("ALTER TABLE `tags` ADD INDEX `tag` (`tag`)");

	delete stmt;
}

std::vector<model::Entry> rsToEntryVector (sql::ResultSet * rs)
{
	std::vector<model::Entry> result;

	while (rs->next ())
	{
		model::Entry entry (rs->getString ("organization").asStdString (), rs->getString ("application").asStdString (),
				    rs->getString ("scope").asStdString (), rs->getString ("slug").asStdString ());
		entry.setTitle (rs->getString ("title").asStdString ());
		entry.setDescription (rs->getString ("description").asStdString ());
		entry.setUploadPlugin (rs->getString ("plugin").asStdString ());
		model::ImportedConfig cfg = service::ConvertEngine::instance ().import (rs->getString ("configuration").asStdString (),
											entry.getUploadPlugin (), entry);
		auto subkeys = cfg.getKeySet ();
		entry.addSubkeys (subkeys.begin (), subkeys.end ());
		entry.setAuthor (rs->getString ("author").asStdString ());
		entry.setViews (rs->getInt ("views"));
		entry.setCreatedAt (rs->getInt ("createdat"));

		result.push_back (entry);
	}

	return result;
}

void printEntryVector (std::vector<model::Entry> entries, int indent = 0)
{
	std::string spaces = "";
	for (int i = 0; i < indent; i++)
	{
		spaces += " ";
	}
	std::cout << spaces << "-> Result: " << entries.size () << " matching entries found" << std::endl;
}

void benchmarkLookupSingleByKey (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry, int indexes)
{
	std::cout << "Benchmark: Lookup single entry by fully specified key\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear & create tables)" << std::endl;

	clearDatabase (con);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

	if (indexes == 1)
	{
		std::cout << "-> Creating indexes" << std::endl;

		addIndexes (con);
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int user_index = numUsers / 2;
	int entry_index = numEntriesPerUser / 2;
	std::string organization = "organization-" + std::to_string (entry_index);
	std::string application = "application-" + std::to_string (entry_index);
	std::string scope = "scope-" + std::to_string (entry_index);
	std::string slug = "slug-" + std::to_string (entry_index) + std::string ("-user-") + std::to_string (user_index);

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	sql::PreparedStatement * pstmt;
	sql::ResultSet * rs;
	pstmt = con->prepareStatement (
		"SELECT s.*,u.`name` AS `author` "
		"FROM `snippets` AS s "
		"INNER JOIN `users` AS u "
		"ON s.`author_id` = u.`id` "
		"WHERE `organization` = ? AND `application` = ? AND `scope` = ? AND `slug` = ?");
	pstmt->setString (1, organization);
	pstmt->setString (2, application);
	pstmt->setString (3, scope);
	pstmt->setString (4, slug);
	rs = pstmt->executeQuery ();
	std::vector<model::Entry> entries = rsToEntryVector (rs);

	// free resources
	delete rs;
	delete pstmt;

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkLookupMultipleByOrganization (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry, int indexes)
{
	std::cout << "Benchmark: Lookup multiple entries by organization\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear & create tables)" << std::endl;

	clearDatabase (con);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

	if (indexes == 1)
	{
		std::cout << "-> Creating indexes" << std::endl;

		addIndexes (con);
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int entry_index = numEntriesPerUser / 2;
	std::string organization = "organization-" + std::to_string (entry_index);

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	sql::PreparedStatement * pstmt;
	sql::ResultSet * rs;
	pstmt = con->prepareStatement (
		"SELECT s.*,u.`name` AS `author` "
		"FROM `snippets` AS s "
		"INNER JOIN `users` AS u "
		"ON s.`author_id` = u.`id` "
		"WHERE `organization` = ? "
		"ORDER BY s.`organization`, s.`application`, s.`scope`, s.`slug` ASC");
	pstmt->setString (1, organization);
	rs = pstmt->executeQuery ();
	std::vector<model::Entry> entries = rsToEntryVector (rs);

	// free resources
	delete rs;
	delete pstmt;

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkLookupMultipleByTag (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry, int indexes)
{
	std::cout << "Benchmark: Lookup multiple entries by tag\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear & create tables)" << std::endl;

	clearDatabase (con);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

	if (indexes == 1)
	{
		std::cout << "-> Creating indexes" << std::endl;

		addIndexes (con);
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int tag_index = numTagsPerEntry / 2;
	std::string tag = "tagged-" + std::to_string (tag_index);

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	sql::PreparedStatement * pstmt;
	sql::ResultSet * rs;
	pstmt = con->prepareStatement (
		"SELECT s.*,u.`name` AS `author` "
		"FROM `snippets` AS s "
		"INNER JOIN `users` AS u "
		"ON s.`author_id` = u.`id` "
		"INNER JOIN `tags` AS t "
		"ON t.`snippet_id` = s.`id` "
		"WHERE t.`tag` = ? "
		"ORDER BY s.`organization`, s.`application`, s.`scope`, s.`slug` ASC");
	pstmt->setString (1, tag);
	rs = pstmt->executeQuery ();
	std::vector<model::Entry> entries = rsToEntryVector (rs);

	// free resources
	delete rs;
	delete pstmt;

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkLookupMultipleByAuthor (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry, int indexes)
{
	std::cout << "Benchmark: Lookup multiple entries by author\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear & create tables)" << std::endl;

	clearDatabase (con);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

	if (indexes == 1)
	{
		std::cout << "-> Creating indexes" << std::endl;

		addIndexes (con);
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int user_index = numUsers / 2;
	std::string author = "user-" + std::to_string (user_index);

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	sql::PreparedStatement * pstmt;
	sql::ResultSet * rs;
	pstmt = con->prepareStatement (
		"SELECT s.*,u.`name` AS `author` "
		"FROM `snippets` AS s "
		"INNER JOIN `users` AS u "
		"ON s.`author_id` = u.`id` "
		"WHERE u.`name` = ? "
		"ORDER BY s.`organization`, s.`application`, s.`scope`, s.`slug` ASC");
	pstmt->setString (1, author);
	rs = pstmt->executeQuery ();
	std::vector<model::Entry> entries = rsToEntryVector (rs);

	// free resources
	delete rs;
	delete pstmt;

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkLookupMultipleByDescription (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry, int indexes)
{
	std::cout << "Benchmark: Lookup multiple entries by description\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear & create tables)" << std::endl;

	clearDatabase (con);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

	if (indexes == 1)
	{
		std::cout << "-> Creating indexes" << std::endl;

		addIndexes (con);
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int entry_index = numEntriesPerUser / 2;
	std::string description_search = "%description-" + std::to_string (entry_index) + "%";

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	sql::PreparedStatement * pstmt;
	sql::ResultSet * rs;
	pstmt = con->prepareStatement (
		"SELECT s.*,u.`name` AS `author` "
		"FROM `snippets` AS s "
		"INNER JOIN `users` AS u "
		"ON s.`author_id` = u.`id` "
		"WHERE s.`description` LIKE ? "
		"ORDER BY s.`organization`, s.`application`, s.`scope`, s.`slug` ASC");
	pstmt->setString (1, description_search);
	rs = pstmt->executeQuery ();
	std::vector<model::Entry> entries = rsToEntryVector (rs);

	// free resources
	delete rs;
	delete pstmt;

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkInsertData (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry, int indexes)
{
	std::cout << "Benchmark: Insert users and entries\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear & create tables)" << std::endl;

	clearDatabase (con);

	if (indexes == 1)
	{
		std::cout << "-> Creating indexes" << std::endl;

		addIndexes (con);
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	std::cout << "   -> Creating test data ("
		  << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry) << " rows)" << std::endl;

	// timer start
	Timer timer;
	timer.start ();

	// execute benchmark
	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

	// stop timer here
	timer.stop ();

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}
}
}


void printUsage (char * argv[])
{
	std::cerr << "Usage: " << argv[0] << " BENCHMARK USERS ENTRIES TAGS [INDEXES]" << std::endl;
	std::cerr << "  - BENCHMARK: one of key, keypart, tag, author, description, insert" << std::endl;
	std::cerr << "  - USERS: number of user records to create for the benchmark" << std::endl;
	std::cerr << "  - ENTRIES: number of entry records per user to create" << std::endl;
	std::cerr << "  - TAGS: number of tags per entry to create" << std::endl;
	std::cerr << "  - INDEXES: whether to use indexes or not (1 or 0)" << std::endl;
}

int main (int argc, char * argv[])
{
	int users, entries, tags, indexes;

	// parse cmd args
	if (argc < 5)
	{
		printUsage (argv);
		return 1;
	}
	std::istringstream iss_users (argv[2]);
	std::istringstream iss_entries (argv[3]);
	std::istringstream iss_tags (argv[4]);
	if (!(iss_users >> users) || !(iss_entries >> entries) || !(iss_tags >> tags))
	{
		printUsage (argv);
		return 1;
	}
	if (argc > 5)
	{
		std::istringstream iss_indexes (argv[5]);
		if (!(iss_indexes >> indexes) || (indexes != 0 && indexes != 1))
		{
			printUsage (argv);
			return 1;
		}
	}

	// load configuration
	cppcms::json::value config = kdbrest::service::ConfigEngine::instance ().loadApplicationConfiguration ();
	(void)kdbrest::Config::initializeConfiguration (config);

	// create mysql connection
	sql::Driver * driver;
	sql::Connection * con;
	driver = get_driver_instance ();
	con = driver->connect (SQL_HOST, SQL_USER, SQL_PASS);

	// select databasesql::Statement *stmt;
	sql::Statement * stmt;
	stmt = con->createStatement ();
	stmt->execute ("CREATE DATABASE IF NOT EXISTS " SQL_DB);
	stmt->execute ("USE " SQL_DB);
	delete stmt;

	// run benchmarks
	if (std::string (argv[1]).compare (0, sizeof ("key"), "key") == 0)
	{
		kdbrest::benchmark::benchmarkLookupSingleByKey (con, users, entries, tags, indexes);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("keypart"), "keypart") == 0)
	{
		kdbrest::benchmark::benchmarkLookupMultipleByOrganization (con, users, entries, tags, indexes);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("tag"), "tag") == 0)
	{
		kdbrest::benchmark::benchmarkLookupMultipleByTag (con, users, entries, tags, indexes);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("author"), "author") == 0)
	{
		kdbrest::benchmark::benchmarkLookupMultipleByAuthor (con, users, entries, tags, indexes);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("description"), "description") == 0)
	{
		kdbrest::benchmark::benchmarkLookupMultipleByDescription (con, users, entries, tags, indexes);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("insert"), "insert") == 0)
	{
		kdbrest::benchmark::benchmarkInsertData (con, users, entries, tags, indexes);
	}
	else
	{
		printUsage (argv);
		delete con;
		return 1;
	}

	// clean up mysql connection
	delete con;

	return 0;
}
