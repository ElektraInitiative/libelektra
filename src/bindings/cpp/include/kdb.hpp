#ifndef CPP_KDB_H
#define CPP_KDB_H

#include <string>
#include <key.hpp>
#include <keyset.hpp>

#include <kdb.h>


namespace kdb {

/**
 * Access to the key database.
 *
 * TODO: currently Key is thrown as exception
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
	if (!handle)
	{
		throw errorKey;
	}
}

/**
 * Constructs a class KDB.
 *
 * @copydoc kdbOpen
 */
inline KDB::KDB (Key &errorKey)
{
	handle = ckdb::kdbOpen(*errorKey);
	if (!handle)
	{
		throw errorKey;
	}
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

} // end of namespace kdb

#endif

