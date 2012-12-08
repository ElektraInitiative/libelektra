#ifndef ELEKTRA_KDB_HPP
#define ELEKTRA_KDB_HPP

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
	KDB (Key & errorKey);
	~KDB () throw();

	inline int get (KeySet & returned, std::string const & keyname);
	inline int get (KeySet & returned, Key & parentKey);
	inline int set (KeySet & returned, std::string const & keyname);
	inline int set (KeySet & returned, Key & parentKey);

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
inline KDB::~KDB () throw()
{
	Key errorKey;
	close (errorKey);
}

/**
 * @class doxygenKDBReturn
 * @brief
 *
 * @retval 0 if no key was updated
 * @retval 1 if user or system keys were updated
 * @retval 2 if user and system keys were updated
 */

/**
 * Get all keys below keyname inside returned.
 *
 * @copydoc kdbGet
 *
 * @include cpp_example_get.cpp
 *
 * @param returned the keyset where the keys will be in
 * @param keyname the root keyname which should be used to get keys below it
 *
 * @copydetails doxygenKDBReturn
 *
 * @throw KeyInvalidName if the keyname is invalid
 * @throw KDBException if there were problems with the database
 *
 * @see KDB::get (KeySet & returned, Key & parentKey)
 */
inline int KDB::get (KeySet & returned, std::string const & keyname)
{
	if (keyname.empty())
	{
		throw KeyInvalidName();
	}

	int ret=0;
	if (keyname[0] != '/')
	{
		Key parentKey (keyname.c_str(), KEY_END);
		ret += get (returned, parentKey);
	}
	else
	{
		std::string userKeyname = "user" + keyname;
		Key userParentKey (userKeyname.c_str(), KEY_END);
		ret += get (returned, userParentKey);

		std::string systemKeyname = "system" + keyname;
		Key systemParentKey (systemKeyname.c_str(), KEY_END);
		ret += get (returned, systemParentKey);
	}
	return ret;
}

/**
 * Get all keys below parentKey inside returned.
 *
 * @copydoc kdbGet
 *
 * @param returned the keyset where the keys will be in
 * @param parentKey the parentKey of returned
 *
 * @copydetails doxygenKDBReturn
 *
 * @throw KeyInvalidName if the keyname is invalid
 * @throw KDBException if there were problems with the database
 */
inline int KDB::get (KeySet & returned, Key & parentKey)
{
	if (!parentKey.isValid())
	{
		throw KeyInvalidName();
	}

	int ret = ckdb::kdbGet (handle, returned.getKeySet(), parentKey.getKey());
	if (ret == -1)
	{
		throw KDBException(parentKey);
	}
	return ret;
}

/**
 * Set all keys below keyname.
 *
 * @copydoc kdbSet
 *
 * @copydetails doxygenKDBReturn
 *
 * @param returned the keyset where the keys will be in
 * @param keyname the keyname below the names should be set
 *
 * @throw KeyInvalidName if the keyname is invalid
 * @throw KDBException if there were problems with the database
 */
inline int KDB::set (KeySet & returned, std::string const & keyname)
{
	if (keyname.empty())
	{
		throw KeyInvalidName();
	}

	int ret = 0;
	if (keyname[0] != '/')
	{
		Key parentKey (keyname.c_str(), KEY_END);
		ret += set (returned, parentKey);
	}
	else
	{
		std::string userKeyname = "user" + keyname;
		Key userParentKey (userKeyname.c_str(), KEY_END);
		ret += set (returned, userParentKey);

		std::string systemKeyname = "system" + keyname;
		Key systemParentKey (systemKeyname.c_str(), KEY_END);
		ret += set (returned, systemParentKey);
	}
	return ret;
}

/**
 * Set all keys below parentKey.
 *
 * @copydoc kdbSet
 *
 * @copydetails doxygenKDBReturn
 *
 * @param returned the keyset where the keys are passed to the user
 * @param parentKey the parentKey of returned
 *
 * @throw KeyInvalidName if the keyname is invalid
 * @throw KDBException if there were problems with the database
 */
inline int KDB::set (KeySet & returned, Key & parentKey)
{
	if (!parentKey.isValid())
	{
		throw KeyInvalidName();
	}

	int ret = ckdb::kdbSet(handle, returned.getKeySet(), parentKey.getKey());
	if (ret == -1)
	{
		throw KDBException(parentKey);
	}
	return ret;
}

} // end of namespace kdb

#endif

