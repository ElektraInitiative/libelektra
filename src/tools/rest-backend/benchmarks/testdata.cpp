#include <ctime>
#include <random>
#include <string>

#include <testdata.hpp>

namespace kdbrest
{

namespace benchmark
{

std::vector<model::Entry> createTestEntries (model::User user, int num, int numTags)
{
	std::vector<model::Entry> result;

	int max = std::time (NULL);
	int min = user.getCreatedAt ();
	std::srand (max);

	for (int i = 0; i < num; i++)
	{
		std::string index = std::to_string (i);
		model::Entry entry (std::string (std::string ("organization-" + index) + std::string ("/application-" + index) +
						 std::string ("/scope-" + index) + std::string ("/slug-" + index) + std::string ("-") +
						 user.getUsername ()));
		entry.setAuthor (user.getUsername ());
		entry.setTitle (std::string ("Entry title for entry ") + index);
		entry.setDescription (
			std::string ("A sophisticated test requires a long description. Therefore we decided "
				     "to write a description that is longer than the other attributes. This doesnt mean that the "
				     "description makes sense, it only has to be long enough. For a unique identification, take the "
				     "value: description-") +
			index);
		entry.setUploadPlugin ("ini");
		entry.setViews (0);
		entry.setCreatedAt (static_cast<long> (std::rand () % (max - min + 1) + min));

		std::vector<std::string> tags;
		for (int j = 0; j < numTags; j++)
		{
			tags.push_back ("tagged-" + std::to_string (j));
		}
		entry.setTags (tags);

		kdb::KeySet ks (12, *kdb::Key (entry.getName () + "/object1", KEY_END),
				*kdb::Key (entry.getName () + "/object1/param1", KEY_VALUE, "value 1-1", KEY_END),
				*kdb::Key (entry.getName () + "/object1/param2", KEY_VALUE, "value 1-2", KEY_END),
				*kdb::Key (entry.getName () + "/object1/param3", KEY_VALUE, "value 1-3", KEY_END),
				*kdb::Key (entry.getName () + "/object2", KEY_END),
				*kdb::Key (entry.getName () + "/object2/param1", KEY_VALUE, "value 2-1", KEY_END),
				*kdb::Key (entry.getName () + "/object2/param2", KEY_VALUE, "value 2-2", KEY_END),
				*kdb::Key (entry.getName () + "/object2/param3", KEY_VALUE, "value 2-3", KEY_END),
				*kdb::Key (entry.getName () + "/array1/#0", KEY_VALUE, "value 3-0", KEY_END),
				*kdb::Key (entry.getName () + "/array1/#1", KEY_VALUE, "value 3-1", KEY_END),
				*kdb::Key (entry.getName () + "/array1/#2", KEY_VALUE, "value 3-2", KEY_END),
				*kdb::Key (entry.getName () + "/array1/#3", KEY_VALUE, "value 3-3", KEY_END), KS_END);
		entry.addSubkeys (ks.begin (), ks.end ());

		result.push_back (entry);
	}

	return result;
}

std::vector<model::User> createTestUsers (int num)
{
	std::vector<model::User> result;

	int max = std::time (NULL);
	int min = max - 31536000; // one year
	std::srand (max);

	for (int i = 0; i < num; i++)
	{
		model::User user (std::string ("user-") + std::to_string (i));
		user.setEmail (user.getUsername () + "@example.com");
		user.setRank (10);
		user.setCreatedAt (static_cast<long> (std::rand () % (max - min + 1) + min));
		user.setPasswordHash (user.getUsername () + user.getUsername () + user.getUsername () + user.getUsername ());

		result.push_back (user);
	}

	return result;
}
}
}
