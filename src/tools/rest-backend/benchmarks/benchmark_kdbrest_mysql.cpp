/**
 * @file
 *
 * @brief
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

#include "../model_configformat.hpp"
#include "../model_entry.hpp"
#include "../model_importedconfig.hpp"
#include "../model_user.hpp"
#include "../service.hpp"
#include "testdata.hpp"
#include "timer.hpp"


#define SQL_HOST "tcp://127.0.0.1:3306"
#define SQL_USER "root"
#define SQL_PASS "root"
#define SQL_DB "test_rest_backend"


namespace kdbrest
{

namespace benchmark
{

void clearDatabase (sql::Connection * con, int indexes)
{
	sql::Statement * stmt;
	stmt = con->createStatement ();

	// drop tables
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
		"`views` INT NOT NULL DEFAULT '0', `createdat` INT NOT NULL, PRIMARY KEY (`id`), ";
	if (indexes == 1)
	{
		tbl_snippets +=
			"FULLTEXT (`organization`), FULLTEXT (`application`), FULLTEXT (`scope`), "
			"FULLTEXT (`slug`), FULLTEXT (`organization`, `application`, `scope`, `slug`), "
			"FULLTEXT (`title`), FULLTEXT (`description`), INDEX (`createdat`), ";
	}
	tbl_snippets += "FOREIGN KEY (`author_id`) REFERENCES `users`(`id`)) ENGINE=InnoDB";
	std::string tbl_tags =
		"CREATE TABLE `tags` (`id` INT NOT NULL AUTO_INCREMENT, `snippet_id` INT NOT NULL, "
		"`tag` VARCHAR(20) NOT NULL, PRIMARY KEY (`id`), ";
	if (indexes == 1)
	{
		tbl_tags += "INDEX (`tag`), ";
	}
	tbl_tags += "FOREIGN KEY (`snippet_id`) REFERENCES `snippets`(`id`)) ENGINE=InnoDB";
	stmt->execute (tbl_users);
	stmt->execute (tbl_snippets);
	stmt->execute (tbl_tags);

	delete stmt;
}

void prepareTestData (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry)
{
	// prepare all statements
	sql::Statement * stmt;
	sql::PreparedStatement * pstmt_user;
	sql::PreparedStatement * pstmt_entry;
	sql::PreparedStatement * pstmt_tag;
	sql::PreparedStatement * pstmt_last_insert_id;
	sql::ResultSet * res;
	stmt = con->createStatement ();
	pstmt_user = con->prepareStatement (
		"INSERT INTO `users` (`name`, `password`, `email`, `rank`, `createdat`) "
		"VALUES (?,?,?,?,?)");
	pstmt_entry = con->prepareStatement (
		"INSERT INTO `snippets` (`author_id`, `organization`, `application`, "
		"`scope`, `slug`, `title`, `description`, `configuration`, `plugin`, `views`, `createdat`) "
		"VALUES (?,?,?,?,?,?,?,?,?,?,?)");
	pstmt_tag = con->prepareStatement ("INSERT INTO `tags` (`snippet_id`, `tag`) VALUES (?,?)");
	pstmt_last_insert_id = con->prepareStatement ("SELECT LAST_INSERT_ID()");

	// start transaction
	stmt->execute ("START TRANSACTION");

	// create users
	std::vector<model::User> users = createTestUsers (numUsers);
	for (auto & user : users)
	{
		pstmt_user->setString (1, user.getUsername ());
		pstmt_user->setString (2, user.getPasswordHash ());
		pstmt_user->setString (3, user.getEmail ());
		pstmt_user->setInt (4, user.getRank ());
		pstmt_user->setInt (5, user.getCreatedAt ());
		pstmt_user->executeUpdate ();
		res = pstmt_last_insert_id->executeQuery ();
		res->next ();
		int author_id = res->getInt (1);
		delete res;

		// create entries for user
		std::vector<model::Entry> entries = createTestEntries (user, numEntriesPerUser, numTagsPerEntry);
		for (auto & entry : entries)
		{
			pstmt_entry->setInt (1, author_id);
			pstmt_entry->setString (2, entry.getOrganization ());
			pstmt_entry->setString (3, entry.getApplication ());
			pstmt_entry->setString (4, entry.getScope ());
			pstmt_entry->setString (5, entry.getSlug ());
			pstmt_entry->setString (6, entry.getTitle ());
			pstmt_entry->setString (7, entry.getDescription ());
			model::ConfigFormat configFormat = service::ConvertEngine::instance ().exportTo (entry.getUploadPlugin (), entry);
			pstmt_entry->setString (8, configFormat.getConfig ());
			pstmt_entry->setString (9, entry.getUploadPlugin ());
			pstmt_entry->setInt (10, entry.getViews ());
			pstmt_entry->setInt (11, entry.getCreatedAt ());
			pstmt_entry->executeUpdate ();
			res = pstmt_last_insert_id->executeQuery ();
			res->next ();
			int snippet_id = res->getInt (1);
			delete res;

			// create tags for entry
			for (auto & tag : entry.getTags ())
			{
				pstmt_tag->setInt (1, snippet_id);
				pstmt_tag->setString (2, tag);
				pstmt_tag->executeUpdate ();
			}
		}
	}

	// commit transaction
	stmt->execute ("COMMIT");

	// free all resources
	delete stmt;
	delete pstmt_user;
	delete pstmt_entry;
	delete pstmt_tag;
	delete pstmt_last_insert_id;
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
	std::cout << spaces << "-> Result:" << std::endl;
	for (auto & entry : entries)
	{
		std::cout << spaces << spaces << "-> " << entry.getPublicName () << std::endl;
	}
}

void benchmarkLookupSingleByKey (sql::Connection * con, int numUsers, int numEntriesPerUser, int numTagsPerEntry, int indexes)
{
	std::cout << "Benchmark: Lookup single entry by fully specified key\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear & create tables)" << std::endl;

	clearDatabase (con, indexes);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

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

	// stop timer here
	timer.stop ();

	// print found entries
	std::vector<model::Entry> entries = rsToEntryVector (rs);
	printEntryVector (entries, 3);

	// free resources
	delete rs;
	delete pstmt;

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

	clearDatabase (con, indexes);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

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
		"WHERE `organization` = ?"
		"ORDER BY s.`organization`, s.`application`, s.`scope`, s.`slug` ASC");
	pstmt->setString (1, organization);
	rs = pstmt->executeQuery ();

	// stop timer here
	timer.stop ();

	// print found entries
	std::vector<model::Entry> entries = rsToEntryVector (rs);
	printEntryVector (entries, 3);

	// free resources
	delete rs;
	delete pstmt;

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

	clearDatabase (con, indexes);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

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
		"WHERE t.`tag` = ?"
		"ORDER BY s.`organization`, s.`application`, s.`scope`, s.`slug` ASC");
	pstmt->setString (1, tag);
	rs = pstmt->executeQuery ();

	// stop timer here
	timer.stop ();

	// print found entries
	std::vector<model::Entry> entries = rsToEntryVector (rs);
	printEntryVector (entries, 3);

	// free resources
	delete rs;
	delete pstmt;

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

	clearDatabase (con, indexes);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

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

	// stop timer here
	timer.stop ();

	// print found entries
	std::vector<model::Entry> entries = rsToEntryVector (rs);
	printEntryVector (entries, 3);

	// free resources
	delete rs;
	delete pstmt;

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

	clearDatabase (con, indexes);

	std::cout << "-> Creating test data (" << (numUsers + numUsers * numEntriesPerUser + numUsers * numEntriesPerUser * numTagsPerEntry)
		  << " rows)" << std::endl;

	prepareTestData (con, numUsers, numEntriesPerUser, numTagsPerEntry);

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

	// stop timer here
	timer.stop ();

	// print found entries
	std::vector<model::Entry> entries = rsToEntryVector (rs);
	printEntryVector (entries, 3);

	// free resources
	delete rs;
	delete pstmt;

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

	clearDatabase (con, indexes);

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