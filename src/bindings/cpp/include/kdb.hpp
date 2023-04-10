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
#include <vector>

#include <kdbexcept.hpp>
#include <key.hpp>
#include <keyset.hpp>
#include <elektradiff.hpp>

#include <kdb.h>
#include <kdbgopts.h>
#include <kdbchangetracking.h>


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
	explicit KDB (KeySet & contract);
	KDB (KeySet & contract, Key & errorKey);
	virtual ~KDB () throw ()
	{
		close ();
	}

	virtual inline void open (Key & errorKey);
	virtual inline void open (KeySet & contract, Key & errorKey);
	virtual inline void close () throw ();
	virtual inline void close (Key & errorKey) throw ();

	virtual inline int get (KeySet & returned, std::string const & keyname);
	virtual inline int get (KeySet & returned, Key & parentKey);
	virtual inline int set (KeySet & returned, std::string const & keyname);
	virtual inline int set (KeySet & returned, Key & parentKey);

	virtual inline ElektraDiff calculateChanges (KeySet & changedKeySet, Key & parentKey);

	inline ckdb::KDB * getKdb () const;
	inline ckdb::KDB * operator* () const;

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
 * Constructs a class KDB.
 *
 * @param contract the contract that should be ensured
 * @param errorKey is useful if you want to get the warnings in
 * the successful case, when no exception is thrown.
 *
 * @throw KDBException if database could not be opened
 *
 * @copydoc kdbOpen
 */
inline KDB::KDB (KeySet & contract)
{
	Key errorKey;
	open (contract, errorKey);
}

/**
 * Constructs a class KDB.
 *
 * @param contract the contract that should be ensured
 * @param errorKey is useful if you want to get the warnings in
 * the successful case, when no exception is thrown.
 *
 * @throw KDBException if database could not be opened
 *
 * @copydoc kdbOpen
 */
inline KDB::KDB (KeySet & contract, Key & errorKey)
{
	open (contract, errorKey);
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
	handle = ckdb::kdbOpen (NULL, errorKey.getKey ());
	if (!handle)
	{
		throw kdb::KDBException (errorKey);
	}
}

/**
 * Open the database
 *
 * @param contract the contract that should be ensured
 * @param errorKey is useful if you want to get the warnings in
 * the successful case, when no exception is thrown.
 *
 * @copydoc kdbOpen
 */
inline void KDB::open (KeySet & contract, Key & errorKey)
{
	handle = ckdb::kdbOpen (contract.getKeySet (), errorKey.getKey ());
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
	Key parentKey (keyname.c_str (), KEY_END);
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
	Key parentKey (keyname.c_str (), KEY_END);
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
 * Calculates the changes between the provided KeySet and the current state of the KDB
 *
 * @param changedKeySet the keyset that should be used to diff
 * @param parentKey only changes same or below this keys are calculated
 *
 * @return a diff with all the changes
 */
inline ElektraDiff KDB::calculateChanges (KeySet & changedKeySet, Key & parentKey)
{
	const ckdb::ChangeTrackingContext * context = ckdb::elektraChangeTrackingGetContextFromKdb (handle);
	ckdb::ElektraDiff * diff = ckdb::elektraChangeTrackingCalculateDiff (changedKeySet.getKeySet(), context, parentKey.getKey());
	return ElektraDiff (diff);
}

/**
 * Passes out the raw kdb pointer.
 *
 * This pointer can be used to directly interact with the underlying kdb instance
 *
 * \note that the ownership remains in the object
 */
inline ckdb::KDB * KDB::getKdb () const
{
	return handle;
}

/**
 * Is an abbreviation for getKdb.
 *
 * @copydoc getKdb
 *
 * @see getKdb()
 */
inline ckdb::KDB * KDB::operator* () const
{
	return handle;
}

/**
 * @see elektraGOptsContract
 */
inline int goptsContract (kdb::KeySet & contract, int argc, const char * const * argv, const char * const * envp,
			  const kdb::Key & parentKey, kdb::KeySet & goptsConfig)
{
	return ckdb::elektraGOptsContract (contract.getKeySet (), argc, argv, envp, parentKey.getKey (), goptsConfig.getKeySet ());
}

/**
 * Prefer to use goptsContract with argc, argv and envp if possible
 * (especially when you are calling this in your main function)
 *
 * This function mainly exists for use from language bindings.
 *
 * @see elektraGOptsContractFromStrings
 */
inline int goptsContract (kdb::KeySet & contract, const std::string & argsString, const std::string & envString, const kdb::Key & parentKey,
			  kdb::KeySet & goptsConfig)
{
	return ckdb::elektraGOptsContractFromStrings (contract.getKeySet (), argsString.size (), argsString.c_str (), envString.size (),
						      envString.c_str (), parentKey.getKey (), goptsConfig.getKeySet ());
}

/**
 * Prefer to use goptsContract with argc, argv and envp if possible
 * (especially when you are calling this in your main function)
 *
 * This function mainly exists for use from language bindings.
 *
 * @see elektraGOptsContractFromStrings
 */
inline int goptsContract (kdb::KeySet & contract, const std::vector<std::string> & args, const std::vector<std::string> & env,
			  const kdb::Key & parentKey, kdb::KeySet & goptsConfig)
{
	std::stringstream argStringStream;
	for (auto && arg : args)
	{
		argStringStream << arg << '\0';
	}
	std::string argString = argStringStream.str ();

	std::stringstream envStringStream;
	for (auto && envvar : env)
	{
		envStringStream << envvar << '\0';
	}
	std::string envString = envStringStream.str ();

	return ckdb::elektraGOptsContractFromStrings (contract.getKeySet (), argString.size (), argString.c_str (), envString.size (),
						      envString.c_str (), parentKey.getKey (), goptsConfig.getKeySet ());
}

} // end of namespace kdb

#endif
