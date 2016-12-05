/**
 * @file
 *
 * @brief benchmark for the rest service utilizing elektra
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <algorithm>
#include <chrono>
#include <iostream>
#include <sstream>
#include <thread>
#include <vector>

#include <kdbthread.hpp>
#include <kdbtimer.hpp>

#include <config.hpp>
#include <kdb_includes.hpp>
#include <model_entry.hpp>
#include <model_user.hpp>
#include <service.hpp>
#include <testdata.hpp>
#include <timer.hpp>

namespace kdbrest
{

namespace benchmark
{

void clearDatabase ()
{
	kdb::KDB kdb;
	kdb::KeySet ks;

	// snippets
	kdb.get (ks, kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs"));
	ks.clear ();
	kdb.set (ks, kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs"));
	// users
	kdb.get (ks, kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.users"));
	ks.clear ();
	kdb.set (ks, kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.users"));
}

void prepareTestData (int numUsers, int numEntriesPerUser, int numTagsPerEntry)
{
	kdb::KDB kdb;
	kdb::KeySet ks_users (numUsers, KS_END);
	kdb::KeySet ks_entries (numUsers * numEntriesPerUser, KS_END);

	kdb.get (ks_users, kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.users"));
	kdb.get (ks_entries, kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs"));

	// create users
	std::vector<model::User> users = createTestUsers (numUsers);
	for (auto & user : users)
	{
		// add user to db
		ks_users.append (user);
		ks_users.append (user.getSubkeys ());

		// create entries for user
		std::vector<model::Entry> entries = createTestEntries (user, numEntriesPerUser, numTagsPerEntry);
		for (auto & entry : entries)
		{
			// add entry to db
			ks_entries.append (entry);
			ks_entries.append (entry.getSubkeys ());
		}
	}

	kdb.set (ks_users, kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.users"));
	kdb.set (ks_entries, kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs"));
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

void benchmarkLookupSingleByKey (int numUsers, int numEntriesPerUser, int numTagsPerEntry, int cached)
{
	std::cout << "Benchmark: Lookup single entry by fully specified key\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear)" << std::endl;

	clearDatabase ();

	std::cout << "-> Creating test data (" << (numUsers * 4 + numUsers * numEntriesPerUser * 18) << " keys)" << std::endl;

	prepareTestData (numUsers, numEntriesPerUser, numTagsPerEntry);

	if (cached == 1)
	{
		std::cout << "-> Loading data into cache" << std::endl;
		(void)service::StorageEngine::instance ();
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int user_index = numUsers / 2;
	int entry_index = numEntriesPerUser / 2;
	std::string organization = "organization-" + std::to_string (entry_index);
	std::string application = "/application-" + std::to_string (entry_index);
	std::string scope = "/scope-" + std::to_string (entry_index);
	std::string slug = "/slug-" + std::to_string (entry_index) + std::string ("-user-") + std::to_string (user_index);
	std::string key = organization + application + scope + slug;

	std::vector<model::Entry> entries;

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	try
	{
		entries.push_back (service::StorageEngine::instance ().getEntry (key));
	}
	catch (kdbrest::exception::EntryNotFoundException & e)
	{
		// we do nothing here, just prevent abort of benchmark
	}

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkLookupMultipleByOrganization (int numUsers, int numEntriesPerUser, int numTagsPerEntry, int cached)
{
	std::cout << "Benchmark: Lookup multiple entries by organization\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear)" << std::endl;

	clearDatabase ();

	std::cout << "-> Creating test data (" << (numUsers * 4 + numUsers * numEntriesPerUser * 18) << " keys)" << std::endl;

	prepareTestData (numUsers, numEntriesPerUser, numTagsPerEntry);

	if (cached == 1)
	{
		std::cout << "-> Loading data into cache" << std::endl;
		(void)service::StorageEngine::instance ();
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int entry_index = numEntriesPerUser / 2;
	std::string organization = "organization-" + std::to_string (entry_index);

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	std::vector<model::Entry> entries = service::StorageEngine::instance ().getAllEntries ();
	service::SearchEngine::instance ().filterConfigurationsByName (entries, organization);
	std::sort (entries.begin (), entries.end (), model::Entry::less_than_key);

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkLookupMultipleByTag (int numUsers, int numEntriesPerUser, int numTagsPerEntry, int cached)
{
	std::cout << "Benchmark: Lookup multiple entries by tag\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear)" << std::endl;

	clearDatabase ();

	std::cout << "-> Creating test data (" << (numUsers * 4 + numUsers * numEntriesPerUser * 18) << " keys)" << std::endl;

	prepareTestData (numUsers, numEntriesPerUser, numTagsPerEntry);

	if (cached == 1)
	{
		std::cout << "-> Loading data into cache" << std::endl;
		(void)service::StorageEngine::instance ();
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int user_index = numUsers / 2;
	int tag_index = numTagsPerEntry / 2;
	std::string tag = "user-" + std::to_string (user_index) + "-" + std::to_string (tag_index);

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	std::vector<model::Entry> entries = service::StorageEngine::instance ().getAllEntries ();
	service::SearchEngine::instance ().findConfigurationsByFilter (entries, tag, "tags");
	std::sort (entries.begin (), entries.end (), model::Entry::less_than_key);

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkLookupMultipleByAuthor (int numUsers, int numEntriesPerUser, int numTagsPerEntry, int cached)
{
	std::cout << "Benchmark: Lookup multiple entries by author\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear)" << std::endl;

	clearDatabase ();

	std::cout << "-> Creating test data (" << (numUsers * 4 + numUsers * numEntriesPerUser * 18) << " keys)" << std::endl;

	prepareTestData (numUsers, numEntriesPerUser, numTagsPerEntry);

	if (cached == 1)
	{
		std::cout << "-> Loading data into cache" << std::endl;
		(void)service::StorageEngine::instance ();
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int user_index = numUsers / 2;
	std::string user = "user-" + std::to_string (user_index);

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	std::vector<model::Entry> entries = service::StorageEngine::instance ().getAllEntries ();
	service::SearchEngine::instance ().findConfigurationsByFilter (entries, user, "author");
	std::sort (entries.begin (), entries.end (), model::Entry::less_than_key);

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkLookupMultipleByDescription (int numUsers, int numEntriesPerUser, int numTagsPerEntry, int cached)
{
	std::cout << "Benchmark: Lookup multiple entries by description\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear)" << std::endl;

	clearDatabase ();

	std::cout << "-> Creating test data (" << (numUsers * 4 + numUsers * numEntriesPerUser * 18) << " keys)" << std::endl;

	prepareTestData (numUsers, numEntriesPerUser, numTagsPerEntry);

	if (cached == 1)
	{
		std::cout << "-> Loading data into cache" << std::endl;
		(void)service::StorageEngine::instance ();
	}

	std::cout << "-> Executing benchmark:" << std::endl;

	// decide what key to look for
	int entry_index = numEntriesPerUser / 2;
	std::string description_search = "description-" + std::to_string (entry_index);

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark
	std::vector<model::Entry> entries = service::StorageEngine::instance ().getAllEntries ();
	service::SearchEngine::instance ().findConfigurationsByFilter (entries, description_search, "description");
	std::sort (entries.begin (), entries.end (), model::Entry::less_than_key);

	// stop timer here
	timer.stop ();

	// print found entries
	printEntryVector (entries, 3);

	// print timer result
	timer.printStatistic (3);

	std::cout << std::endl;
}

void benchmarkInsertData (int numUsers, int numEntriesPerUser, int numTagsPerEntry)
{
	std::cout << "Benchmark: Insert users and entries\n"
		  << "          (" << numUsers << " users à " << numEntriesPerUser << " entries with " << numTagsPerEntry << " tags each)"
		  << std::endl;
	std::cout << "==============================================================" << std::endl;

	std::cout << "-> Refreshing database (clear)" << std::endl;

	clearDatabase ();

	std::cout << "-> Executing benchmark:" << std::endl;

	std::cout << "   -> Creating test data (" << (numUsers * 4 + numUsers * numEntriesPerUser * 18) << " keys)" << std::endl;

	// timer start
	Timer timer;
	timer.start ();

	// the stuff to benchmark (can't use prepareDate because it does not utilize service class)
	std::vector<model::User> users = createTestUsers (numUsers);
	for (auto & user : users)
	{
		// add user to db
		service::StorageEngine::instance ().createUser (user);

		// create entries for user
		std::vector<model::Entry> entries = createTestEntries (user, numEntriesPerUser, numTagsPerEntry);
		for (auto & entry : entries)
		{
			// add entry to db
			service::StorageEngine::instance ().createEntry (entry);
		}
	}

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
	std::cerr << "Usage: " << argv[0] << " BENCHMARK USERS ENTRIES TAGS [CACHED]" << std::endl;
	std::cerr << "  - BENCHMARK: one of key, keypart, tag, author, description, insert" << std::endl;
	std::cerr << "  - USERS: number of user records to create for the benchmark" << std::endl;
	std::cerr << "  - ENTRIES: number of entry records per user to create" << std::endl;
	std::cerr << "  - TAGS: number of tags per entry to create" << std::endl;
	std::cerr << "  - CACHED: whether to use in-memory caching or not (1 or 0)" << std::endl;
}

int main (int argc, char * argv[])
{
	int users, entries, tags, cached;

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
		std::istringstream iss_cached (argv[5]);
		if (!(iss_cached >> cached) || (cached != 0 && cached != 1))
		{
			printUsage (argv);
			return 1;
		}
	}

	// load configuration
	cppcms::json::value config = kdbrest::service::ConfigEngine::instance ().loadApplicationConfiguration ();
	(void)kdbrest::Config::instance ().initializeConfiguration (config);
	kdbrest::Config::instance ().setValue<std::string> ("kdb.path.configs", "dir/configs");
	kdbrest::Config::instance ().setValue<std::string> ("kdb.path.users", "dir/users");

	// run benchmarks
	if (std::string (argv[1]).compare (0, sizeof ("key"), "key") == 0)
	{
		kdbrest::benchmark::benchmarkLookupSingleByKey (users, entries, tags, cached);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("keypart"), "keypart") == 0)
	{
		kdbrest::benchmark::benchmarkLookupMultipleByOrganization (users, entries, tags, cached);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("tag"), "tag") == 0)
	{
		kdbrest::benchmark::benchmarkLookupMultipleByTag (users, entries, tags, cached);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("author"), "author") == 0)
	{
		kdbrest::benchmark::benchmarkLookupMultipleByAuthor (users, entries, tags, cached);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("description"), "description") == 0)
	{
		kdbrest::benchmark::benchmarkLookupMultipleByDescription (users, entries, tags, cached);
	}
	else if (std::string (argv[1]).compare (0, sizeof ("insert"), "insert") == 0)
	{
		kdbrest::benchmark::benchmarkInsertData (users, entries, tags);
	}
	else
	{
		printUsage (argv);
		return 1;
	}

	return 0;
}
