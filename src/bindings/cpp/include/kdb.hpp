/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_HPP
#define ELEKTRA_KDB_HPP

#include <string>

#include <kdbexcept.hpp>
#include <key.hpp>
#include <keyset.hpp>

#include <elektra/kdb.h>


/**
 * @brief This is the main namespace for the C++ binding and libraries.
 *
 * Classes or Functions directly below this namespace are header-only.
 * Sub namespaces are intended for libraries and you need to link
 * the library if you want to use them.
 * - @see kdb::tools
 */
namespace kdb
{

/**
 * @copydoc KDB
 *
 * @brief Access to the key database.
 *
 * @invariant the object holds a valid connection to the key database
 * or is empty
 */
class KDB
{
public:
	KDB ();
	explicit KDB (Key & errorKey);
	virtual ~KDB () throw ()
	{
		close ();
	}

	virtual inline void open (Key & errorKey);
	virtual inline void close () throw ();
	virtual inline void close (Key & errorKey) throw ();

	virtual inline int get (KeySet & returned, std::string const & keyname);
	virtual inline int get (KeySet & returned, Key & parentKey);
	virtual inline int set (KeySet & returned, std::string const & keyname);
	virtual inline int set (KeySet & returned, Key & parentKey);

	inline int ensure (const KeySet & contract, Key & parentKey);

private:
	ckdb::KDB * handle; ///< holds an kdb handle
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
	open (errorKey);
}

/**
 * Constructs a class KDB.
 *
 * @param errorKey is useful if you want to get the warnings in
 * the successful case, when no exception is thrown.
 *
 * @throw KDBException if database could not be opened
 *
 * @copydoc kdbOpen
 */
inline KDB::KDB (Key & errorKey)
{
	open (errorKey);
}

/**
 * Open the database
 *
 * @param errorKey is useful if you want to get the warnings in
 * the successful case, when no exception is thrown.
 *
 * @copydoc kdbOpen
 */
inline void KDB::open (Key & errorKey)
{
	handle = ckdb::kdbOpen (errorKey.getKey ());
	if (!handle)
	{
		throw kdb::KDBException (errorKey);
	}
}

/**
 * Close the database.
 *
 * The return value does not matter because its only a null pointer check.
 *
 * @copydoc kdbClose
 */
inline void KDB::close () throw ()
{
	Key errorKey;
	ckdb::kdbClose (handle, errorKey.getKey ());
	handle = nullptr;
}


/**
 * Close the database.
 *
 * The return value does not matter because its only a null pointer check.
 *
 * @param errorKey is useful if you want to get the warnings
 *
 * @copydoc kdbClose
 */
inline void KDB::close (Key & errorKey) throw ()
{
	ckdb::kdbClose (handle, errorKey.getKey ());
	handle = nullptr;
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
 * @throw KDBException if there were problems with the database
 *
 * @see KDB::get (KeySet & returned, Key & parentKey)
 */
inline int KDB::get (KeySet & returned, std::string const & keyname)
{
	Key parentKey (keyname.c_str (), KEY_CASCADING_NAME, KEY_END);
	return get (returned, parentKey);
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
 * @throw KDBException if there were problems with the database
 */
inline int KDB::get (KeySet & returned, Key & parentKey)
{
	int ret = ckdb::kdbGet (handle, returned.getKeySet (), parentKey.getKey ());
	if (ret == -1)
	{
		throw KDBException (parentKey);
	}
	return ret;
}

/**
 * Set all keys below keyname.
 *
 * If the keyname of the parentKey is invalid (e.g. empty) all keys will be set.
 *
 * @copydoc kdbSet
 *
 * @copydetails doxygenKDBReturn
 *
 * @param returned the keyset where the keys will be in
 * @param keyname the keyname below the names should be set
 *
 * @throw KDBException if there were problems with the database
 */
inline int KDB::set (KeySet & returned, std::string const & keyname)
{
	Key parentKey (keyname.c_str (), KEY_CASCADING_NAME, KEY_END);
	return set (returned, parentKey);
}

/**
 * Set all keys below parentKey.
 *
 * If the keyname of the parentKey is invalid (e.g. empty) all keys will be set.
 *
 * @copydoc kdbSet
 *
 * @copydetails doxygenKDBReturn
 *
 * @param returned the keyset where the keys are passed to the user
 * @param parentKey the parentKey of returned
 *
 * @throw KDBException if there were problems with the database
 */
inline int KDB::set (KeySet & returned, Key & parentKey)
{
	int ret = ckdb::kdbSet (handle, returned.getKeySet (), parentKey.getKey ());
	if (ret == -1)
	{
		throw KDBException (parentKey);
	}
	return ret;
}

/**
 * Ensures that the conditions defined in @p contract are met by this KDB.
 *
 * @see ckdb::kdbEnsure()
 *
 * @param contract  The contract to ensure.
 * @param parentKey The parentKey to use.
 *
 * @throw KDBException if there were problems with the contract or the database
 * @throw ContractException if the contract couldn't be ensured
 */
int KDB::ensure (const KeySet & contract, Key & parentKey)
{
	// have to ksDup because contract is consumed and ksDel()ed by kdbEnsure
	int ret = ckdb::kdbEnsure (handle, ckdb::ksDup (contract.getKeySet ()), parentKey.getKey ());
	if (ret == -1)
	{
		throw KDBException (parentKey);
	}
	if (ret == 1)
	{
		throw ContractException (parentKey);
	}
	return ret;
}


} // end of namespace kdb

#endif
