#ifndef CPP_KDB_H
#define CPP_KDB_H

#include <string>
#include <key.hpp>
#include <keyset.hpp>

#include <kdb.h>


namespace kdb {

class KDBException : public std::exception
{
	const char* what() {return "KDB Exception";}
};

/**
 * Access to the key database
 */
class KDB
{
public:
	KDB ();
	KDB (Key &errorKey);
	~KDB ();

	void close(Key &errorKey);

	int get (KeySet & returned, Key &parentKey);
	int set (KeySet & returned, Key &parentKey);

	/*
	size_t get (KeySet & returned, const std::string &parentName, option_t options = KDB_O_NONE);
	size_t get (KeySet & returned, const char * parentName, option_t options = KDB_O_NONE);

	void get (Key & toGet);
	void set (const Key & toSet);


	void getString (const std::string &keyname, std::string value, size_t maxSize);
	void setString (const std::string &keyname, const std::string &value);
	void remove (const std::string &keyname);
	*/

protected:
	/**You may use the KDB in an inherited class*/
	ckdb::KDB* handle;
};

/**
 * Constructs a class KDB.
 *
 * @copydoc kdbOpen
 */
inline KDB::KDB ()
{
	Key errorKey;
	handle = ckdb::kdbOpen(*errorKey);
	if (!handle) throw errorKey;
}

/**
 * Constructs a class KDB.
 *
 * @copydoc kdbOpen
 */
inline KDB::KDB (Key &errorKey)
{
	handle = ckdb::kdbOpen(*errorKey);
	if (!handle) throw errorKey;
}

/**
 * The destructor closes the database.
 *
 * @copydoc kdbClose
 */
inline KDB::~KDB ()
{
	Key errorKey;
	close (errorKey);
}


/**
 * @brief manually close connection to key database
 *
 * @copydoc kdbClose
 *
 * @note in destructor errorKey information would get lost
 * @param errorKey the key where the warnings will be attached
 */
inline void KDB::close (Key &errorKey)
{
	ckdb::kdbClose(handle, errorKey.getKey());
	handle = 0;
}

/**
 * Get all keys below parentKey inside returned.
 *
 * @copydoc kdbGet
 *
 * @param returned the keyset where the keys will be in
 * @param parentKey the parentKey of returned
 * @param options to change the behaviour which keys to fetch
 */
inline int KDB::get (KeySet & returned, Key & parentKey)
{
	int ret = ckdb::kdbGet (handle, returned.getKeySet(), parentKey.getKey());
	if (ret == -1) throw parentKey;
	return ret;
}

/**
 * @copydoc kdbSet
 */
inline int KDB::set (KeySet & returned, Key & parentKey)
{
	int ret = ckdb::kdbSet(handle, returned.getKeySet(), parentKey.getKey());
	if (ret == -1) throw parentKey;
	return ret;
}

/*
inline size_t KDB::get (KeySet & returned, const std::string &parentName, option_t options)
{
	ssize_t ret = ckdb::kdbGetByName (handle, returned.getKeySet(), parentName.c_str(), options);
	if (ret == -1) throw KDBException();
	return ret;
}

inline size_t KDB::get (KeySet & returned, const char * parentName, option_t options)
{
	ssize_t ret = ckdb::kdbGetByName (handle, returned.getKeySet(), parentName, options);
	if (ret == -1) throw KDBException();
	return ret;
}

inline void KDB::getString (const std::string &keyname, std::string value, size_t maxSize)
{
	char *c = new char[maxSize];
	ckdb::kdbGetString(handle, keyname.c_str(), c, maxSize);
	value = c;
	delete (c);
}

inline void KDB::setString (const std::string &keyname, const std::string &value)
{
	ckdb::kdbSetString(handle, keyname.c_str(), value.c_str());
}

inline void KDB::remove (const std::string &keyname)
{
	ckdb::kdbRemove(handle, keyname.c_str());
}

inline void KDB::get (Key & toGet)
{
	int ret = ckdb::kdbGetKey(handle, toGet.getKey());
	if (ret == -1) throw KDBException();
}

inline void KDB::set (const Key & toSet)
{
	int ret = ckdb::kdbSetKey(handle, toSet.getKey());
	if (ret == -1) throw KDBException();
}

*/

} // end of namespace kdb

#endif

