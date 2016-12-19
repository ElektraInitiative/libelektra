/**
 * @file
 *
 * @brief implementation of the storage service class
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <regex>

#include <service.hpp>

namespace kdbrest
{

namespace service
{

/**
 * @brief Default constructor that pre-fetches the cache.
 */
StorageEngine::StorageEngine ()
{
	// pre fetch the entry cache
	this->loadAllEntries ();
	// pre-fetch the user cache
	this->loadAllUsers ();
}

/**
 * @brief Can be used to create an entry in the database.
 * 
 * Will add the entry and all subkeys to the database (configuration).
 * 
 * @param entry A custom Entry object holding information to store.
 * @return true if the entry was stored, false if something went wrong
 * @throws kdbrest::exception::EntryAlreadyExistsException in case an
 * entry with the given name does already exist in the key database.
 */
bool StorageEngine::createEntry (model::Entry & entry)
{
	using namespace kdb;

	// register exclusive access
	boost::unique_lock<boost::shared_mutex> lock (m_mutex_entryCache);

	std::vector<model::Entry> & entries = this->m_entryCache;
	for (auto & elem : entries)
	{
		if (elem.getName ().compare (entry.getName ()) == 0)
		{
			throw exception::EntryAlreadyExistsException ();
		}
	}

	KDB kdb;
	KeySet ks;
	kdb.get (ks, entry.getName ());

	Key k = ks.lookup (entry.getName ());
	if (k)
	{
		throw kdbrest::exception::EntryAlreadyExistsException ();
	}

	ks.append (entry);
	ks.append (entry.getSubkeys ());

	if (kdb.set (ks, entry.getName ()) >= 1)
	{
		entries.push_back (entry);
		return true;
	}
	else
	{
		return false;
	}
}

/**
 * @brief Allows for updating of a database entry.
 * 
 * Will renew the entry and all its subkeys (configuration).
 * 
 * @param entry A custom Entry object holding current information.
 * @return true if the entry was updated, false if not
 * @throw kdbrest::exception::EntryNotFoundException in case the entry
 * to update does not exist.
 */
bool StorageEngine::updateEntry (model::Entry & entry)
{
	using namespace kdb;

	// register exclusive access
	boost::unique_lock<boost::shared_mutex> lock (m_mutex_entryCache);

	bool found = false;
	std::vector<model::Entry> & entries = this->m_entryCache;
	unsigned int i = 0;
	while (i < entries.size ())
	{
		if (entries[i].getName ().compare (entry.getName ()) == 0)
		{
			found = true;
			break;
		}
		i++;
	}

	if (!found)
	{
		throw exception::EntryNotFoundException ();
	}

	KDB kdb;
	KeySet ks;
	kdb.get (ks, entry.getName ());

	Key k = ks.lookup (entry.getName ());
	if (!k)
	{
		throw kdbrest::exception::EntryNotFoundException ();
	}

	ks.cut (entry);
	ks.append (entry);
	ks.append (entry.getSubkeys ());

	if (kdb.set (ks, entry.getName ()) >= 1)
	{
		entries.erase (entries.begin () + i);
		entries.push_back (entry);
		return true;
	}
	else
	{
		return false;
	}
}

/**
 * @brief Allows for deleting of a database entry.
 * 
 * Will delete the entry iteself as well as all meta data and subkeys (configuration).
 * 
 * @param entry A custom Entry object that should be deleted.
 * @return true if the entry was deleted successfully, false otherwise
 * @throw kdbrest::exception::EntryNotFoundException in case the entry
 * to delete does not exist.
 */
bool StorageEngine::deleteEntry (model::Entry & entry)
{
	using namespace kdb;

	// register exclusive access
	boost::unique_lock<boost::shared_mutex> lock (m_mutex_entryCache);

	bool found = false;
	std::vector<model::Entry> & entries = this->m_entryCache;
	unsigned int i = 0;
	while (i < entries.size ())
	{
		if (entries[i].getName ().compare (entry.getName ()) == 0)
		{
			found = true;
			break;
		}
		i++;
	}

	if (!found)
	{
		throw exception::EntryNotFoundException ();
	}

	KDB kdb;
	KeySet ks;
	kdb.get (ks, entry.getName ());

	Key k = ks.lookup (entry.getName ());
	if (!k)
	{
		throw kdbrest::exception::EntryNotFoundException ();
	}

	ks.cut (entry);

	if (kdb.set (ks, entry.getName ()) >= 1)
	{
		entries.erase (entries.begin () + i);
		return true;
	}
	else
	{
		return false;
	}
}

/**
 * @brief Checks if an entry exists
 * 
 * Can be used to determine if an entry with the given name 
 * exists in the database or not.
 *
 * @param key A string containing the entry name
 * @return true if the entry exists, false otherwise
 */
bool StorageEngine::entryExists (const std::string & key)
{
	// register read access
	boost::shared_lock<boost::shared_mutex> lock (m_mutex_entryCache);

	for (auto & elem : this->getAllEntriesRef ())
	{
		if (elem.getPublicName ().compare (key) == 0) return true;
	}

	return false;
}

/**
 * @brief attempts to retrieve an entry from the database
 * 
 * Can be used to get an entry from the database,
 * base on a given name.
 *
 * @param key A string containing the key in elektra format
 * @return An Entry element containing all information
 * @throws kdbrest::exception::EntryNotFoundException in case the
 * requested entry could not be found
 */
model::Entry StorageEngine::getEntry (const std::string & key)
{
	// register read access
	boost::shared_lock<boost::shared_mutex> lock (m_mutex_entryCache);

	for (auto & elem : this->m_entryCache)
	{
		if (elem.getPublicName ().compare (key) == 0) return elem;
	}

	throw exception::EntryNotFoundException ();
}

/**
 * @brief retrieves all entries from the database
 * 
 * Fetches all entries from the database and returns them
 * as a vector. The database keys will be converted into Entry
 * objects with their configuration stored as sub keys.
 * 
 * Returned will be a copy of the entry list that is currently in the
 * cache.
 * 
 * @param force If the cache should be re-fetched.
 * @return A vector containing all entries of the database
 */
std::vector<model::Entry> StorageEngine::getAllEntries (bool force)
{
	if (force)
	{
		this->loadAllEntries ();
	}

	// register read access
	boost::shared_lock<boost::shared_mutex> lock (m_mutex_entryCache);

	return std::vector<model::Entry> (this->m_entryCache);
}

/**
 * @brief retireves all entries from the database
 * 
 * Fetches all entries from the database and returns them
 * as a vector. The database keys will be converted into Entry
 * objects with their configuration stored as sub keys.
 * 
 * Returned will be a reference to the entry list that is currently 
 * in the cache. This reference should be used carefully, because
 * the list is not thread-safe. Should be used for search operations
 * only.
 * 
 * @param force If the cache should be re-fetched.
 * @return A vector containing all entries of the database
 */
std::vector<model::Entry> & StorageEngine::getAllEntriesRef (bool force)
{
	if (force)
	{
		this->loadAllEntries ();
	}

	return this->m_entryCache;
}

/**
 * @brief Loads all entries in the database into the cache.
 */
void StorageEngine::loadAllEntries ()
{
	using namespace kdb;

	// register exclusive access
	boost::unique_lock<boost::shared_mutex> lock (m_mutex_entryCache);

	// flush cache
	this->m_entryCache.clear ();

	std::string parentKeyStr = Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
	std::regex regex (ELEKTRA_REST_ENTRY_SCHEMA_CONFIGS);

	KDB kdb;
	KeySet ks;
	kdb.get (ks, parentKeyStr);

	auto elem = ks.begin ();
	while (elem != ks.end ())
	{
		kdb::Key k = elem.get ();
		if (std::regex_match (k.getName ().erase (0, parentKeyStr.length () + 1), regex))
		{
			kdbrest::model::Entry entry = static_cast<kdbrest::model::Entry> (k);
			elem++; // the next element must be a sub-key of this element
			while (elem != ks.end () && elem.get ().isBelow (entry))
			{
				entry.addSubkey (elem.get ());
				elem++;
			}
			this->m_entryCache.push_back (entry);
			continue; // we don't have to increase manually anymore
		}
		elem++;
	}
}

/**
 * @brief Can be used to create an user entry in the database.
 * 
 * Will add the user and all subkeys to the database (additional information).
 * 
 * @param user A custom User object holding information to store.
 * @return true if the user was stored, false if something went wrong
 * @throws kdbrest::exception::UserAlreadyExistsException in case an
 * user with the given name does already exist in the key database.
 */
bool StorageEngine::createUser (model::User & user)
{
	using namespace kdb;

	// register exclusive access
	boost::unique_lock<boost::shared_mutex> lock (m_mutex_userCache);

	std::vector<model::User> & users = this->m_userCache;
	for (auto & elem : users)
	{
		if (elem.getName ().compare (user.getName ()) == 0)
		{
			throw exception::UserAlreadyExistsException ();
		}
	}

	KDB kdb;
	KeySet ks;
	kdb.get (ks, user.getName ());

	Key k = ks.lookup (user.getName ());
	if (k)
	{
		throw exception::UserAlreadyExistsException ();
	}

	ks.append (user);
	ks.append (user.getSubkeys ());

	if (kdb.set (ks, user.getName ()) >= 1)
	{
		users.push_back (user);
		return true;
	}
	else
	{
		return false;
	}
}

/**
 * @brief Allows for updating of an user entry.
 * 
 * Will renew the entry and all its subkeys (additional user information).
 * 
 * @param user A custom User object holding current information.
 * @return true if the user was updated, false if not
 * @throw kdbrest::exception::UserNotFoundException in case the user to
 * update does not exist.
 */
bool StorageEngine::updateUser (model::User & user)
{
	using namespace kdb;

	// register exclusive access
	boost::unique_lock<boost::shared_mutex> lock (m_mutex_userCache);

	bool found = false;
	std::vector<model::User> & users = this->m_userCache;
	unsigned int i = 0;
	while (i < users.size ())
	{
		if (users[i].getName ().compare (user.getName ()) == 0)
		{
			found = true;
			break;
		}
		i++;
	}

	if (!found)
	{
		throw exception::UserNotFoundException ();
	}

	KDB kdb;
	KeySet ks;
	kdb.get (ks, user.getName ());

	Key k = ks.lookup (user.getName ());
	if (!k)
	{
		throw kdbrest::exception::UserNotFoundException ();
	}

	ks.cut (user);
	ks.append (user);
	ks.append (user.getSubkeys ());

	if (kdb.set (ks, user.getName ()) >= 1)
	{
		users.erase (users.begin () + i);
		users.push_back (user);
		return true;
	}
	else
	{
		return false;
	}
}

/**
 * @brief Allows for deleting of an user entry.
 * 
 * Will delete the entry iteself as well as all subkeys (additional user information).
 * 
 * @param user A custom User object that should be deleted.
 * @return true if the user was deleted successfully, false otherwise
 * @throw kdbrest::exception::UserNotFoundException in case the user
 * to delete does not exist.
 */
bool StorageEngine::deleteUser (model::User & user)
{
	using namespace kdb;

	// register exclusive access
	boost::unique_lock<boost::shared_mutex> lock (m_mutex_userCache);

	bool found = false;
	std::vector<model::User> & users = this->m_userCache;
	unsigned int i = 0;
	while (i < users.size ())
	{
		if (users[i].getName ().compare (user.getName ()) == 0)
		{
			found = true;
			break;
		}
		i++;
	}

	if (!found)
	{
		throw exception::UserNotFoundException ();
	}

	KDB kdb;
	KeySet ks;
	kdb.get (ks, user.getName ());

	Key k = ks.lookup (user.getName ());
	if (!k)
	{
		throw kdbrest::exception::UserNotFoundException ();
	}

	ks.cut (user);

	if (kdb.set (ks, user.getName ()) >= 1)
	{
		users.erase (users.begin () + i);
		return true;
	}
	else
	{
		return false;
	}
}

/**
 * @brief checks if a user exists in the database
 * 
 * Can be used to determine if aa user with the given name 
 * exists in the database or not.
 *
 * @param username A string containing the user name
 * @return true if the user exists, false otherwise
 */
bool StorageEngine::userExists (const std::string & username)
{
	// register read access
	boost::shared_lock<boost::shared_mutex> lock (m_mutex_userCache);

	for (auto & elem : this->getAllUsersRef ())
	{
		if (elem.getUsername ().compare (username) == 0) return true;
	}

	return false;
}

/**
 * @brief retrieves a user from the database
 * 
 * Can be used to get a user from the database,
 * base on a given name.
 *
 * @param username A string containing the username
 * @return An User element containing all information
 * @throws kdbrest::exception::UserNotFoundException in case the
 * requested user could not be found
 */
model::User StorageEngine::getUser (const std::string & username)
{
	// register read access
	boost::shared_lock<boost::shared_mutex> lock (m_mutex_userCache);

	for (auto & elem : this->m_userCache)
	{
		if (elem.getUsername ().compare (username) == 0) return elem;
	}

	throw exception::UserNotFoundException ();
}

/**
 * @brief retrieves all users from the database
 * 
 * Fetches all users from the database and returns them
 * as a vector. The database keys will be converted into User
 * objects with their personal information stored as sub keys.
 * 
 * @param force whether to force a cache refresh
 * @return A vector containing all users of the database
 */
std::vector<model::User> StorageEngine::getAllUsers (bool force)
{
	if (force)
	{
		this->loadAllUsers ();
	}

	// register read access
	boost::shared_lock<boost::shared_mutex> lock (m_mutex_userCache);

	return std::vector<model::User> (this->m_userCache);
}

/**
 * @brief retrieves all users from the database
 * 
 * Fetches all users from the database and returns them
 * as a vector. The database keys will be converted into User
 * objects with their personal information stored as sub keys.
 * 
 * @note This function returns a reference to the internal cache.
 * Changes to the vector can therefore cause unexpected behavior!
 * 
 * @param force whether to force a cache refresh
 * @return A vector reference containing all users of the database
 */
std::vector<model::User> & StorageEngine::getAllUsersRef (bool force)
{
	if (force)
	{
		this->loadAllUsers ();
	}

	return this->m_userCache;
}

/**
 * @brief loads all users from the database into the cache
 */
void StorageEngine::loadAllUsers ()
{
	using namespace kdb;

	// register exclusive access
	boost::unique_lock<boost::shared_mutex> lock (m_mutex_userCache);

	// flush cache
	this->m_userCache.clear ();

	std::string parentKeyStr = Config::instance ().getConfig ().get<std::string> ("kdb.path.users");
	std::regex regex (ELEKTRA_REST_ENTRY_SCHEMA_USERS);

	KDB kdb;
	KeySet ks;
	kdb.get (ks, parentKeyStr);

	auto elem = ks.begin ();
	while (elem != ks.end ())
	{
		kdb::Key k = elem.get ();
		if (std::regex_match (k.getName ().erase (0, parentKeyStr.length () + 1), regex))
		{
			kdbrest::model::User user = static_cast<kdbrest::model::User> (k);
			elem++; // the next element must be a sub-key of this element
			while (elem != ks.end () && elem.get ().isBelow (user))
			{
				user.addSubkey (elem.get ());
				elem++;
			}
			this->m_userCache.push_back (user);
			continue; // we don't have to increase manually anymore
		}
		elem++;
	}
}

} // namespace service

} // namespace kdbrest
