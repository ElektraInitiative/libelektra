#include <kdb>
#include <iostream>

namespace kdb
{

/**
 * Constructs a class KDB.
 */
KDB::KDB ()
{
	handle = ckdb::kdbOpen();
}

/**
 * The destructor closes the database.
 */
KDB::~KDB ()
{
	ckdb::kdbClose(handle);
}

/**
 * Get all keys below parentKey inside returned.
 *
 * @param returned the keyset where the keys will be in
 * @param parentKey the parentKey of returned
 * @param options to change the behaviour which keys to fetch
 */
size_t KDB::get (KeySet & returned, const Key & parentKey, option_t options)
{
	ssize_t ret = ckdb::kdbGet (handle, returned.getKeySet(), parentKey.getKey(), options);
	if (ret == -1) throw KDBException();
	return ret;
}


size_t KDB::set (KeySet & returned, const Key & parentKey, option_t options)
{
	ssize_t ret = ckdb::kdbSet(handle, returned.getKeySet(), parentKey.getKey(), options);
	if (ret == -1) throw KDBException();
	return ret;
}


size_t KDB::get (KeySet & returned, const std::string &parentName, option_t options)
{
	ssize_t ret = ckdb::kdbGetByName (handle, returned.getKeySet(), parentName.c_str(), options);
	if (ret == -1) throw KDBException();
	return ret;
}

size_t KDB::get (KeySet & returned, const char * parentName, option_t options)
{
	ssize_t ret = ckdb::kdbGetByName (handle, returned.getKeySet(), parentName, options);
	if (ret == -1) throw KDBException();
	return ret;
}


void KDB::getString (const std::string &keyname, std::string value, size_t maxSize)
{
	char *c = new char[maxSize];
	ckdb::kdbGetString(handle, keyname.c_str(), c, maxSize);
	value = c;
	delete (c);
}

void KDB::setString (const std::string &keyname, const std::string &value)
{
	ckdb::kdbSetString(handle, keyname.c_str(), value.c_str());
}

void KDB::remove (const std::string &keyname)
{
	ckdb::kdbRemove(handle, keyname.c_str());
}

/**
 * Get a single key.
 *
 * @param toGet the key to get
 */
void KDB::get (Key & toGet)
{
	int ret = ckdb::kdbGetKey(handle, toGet.getKey());
	if (ret == -1) throw KDBException();
}

void KDB::set (const Key & toSet)
{
	int ret = ckdb::kdbSetKey(handle, toSet.getKey());
	if (ret == -1) throw KDBException();
}

} // end of namespace kdb

