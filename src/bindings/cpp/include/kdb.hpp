#ifndef CPP_KDB_H
#define CPP_KDB_H

#include <string>
#include <key.hpp>
#include <keyset.hpp>
#include <kdbexcept.hpp>

#include <kdb.h>


namespace kdb {

/**
 * @brief Access to the key database.
 *
 * @invariant the object holds an connection to the key database
 */
class KDB
{
public:
	KDB ();
	KDB (Key &errorKey);
	~KDB ();

	inline int get (KeySet & returned, Key &parentKey);
	inline int set (KeySet & returned, Key &parentKey);

private:
	ckdb::KDB* handle; ///< holds an kdb handle
};

/**
 * Constructs a class KDB.
 *
 * @throw KDBException if database could not be opened
 *
 * @copydoc kdbOpen
 */
inline KDB::KDB ()
{
	Key errorKey;
	handle = ckdb::kdbOpen(*errorKey);
	if (!handle)
	{
		throw KDBException(errorKey);
	}
}

/**
 * Constructs a class KDB.
 *
 * @throw KDBException if database could not be opened
 *
 * @copydoc kdbOpen
 */
inline KDB::KDB (Key &errorKey)
{
	handle = ckdb::kdbOpen(*errorKey);
	if (!handle)
	{
		throw kdb::KDBException(errorKey);
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
	if (ret == -1)
	{
		throw KDBException(parentKey);
	}
	return ret;
}

/**
 * @copydoc kdbSet
 */
inline int KDB::set (KeySet & returned, Key & parentKey)
{
	int ret = ckdb::kdbSet(handle, returned.getKeySet(), parentKey.getKey());
	if (ret == -1)
	{
		throw KDBException(parentKey);
	}
	return ret;
}

} // end of namespace kdb

#endif

