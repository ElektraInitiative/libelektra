/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_ELEKTRADIFF_HPP
#define ELEKTRA_ELEKTRADIFF_HPP

#include <elektradiffexcept.hpp>

#include <key.hpp>
#include <keyset.hpp>

#include <kdbdiff.h>

namespace kdb
{

/**
 * This class is a wrapper around the ElektraDiff C struct.
 *
 * \invariant always holds an underlying ElektraDiff C object.
 */
class ElektraDiff
{
public:
	inline ElektraDiff ();
	inline explicit ElektraDiff (ckdb::ElektraDiff * cdiff);
	inline ElektraDiff (ElektraDiff & other);
	inline ElektraDiff (ElektraDiff const & other);

	inline ~ElektraDiff ();

	inline static ElektraDiff calculateDiff (const KeySet & newKeys, const KeySet & oldKeys, const std::string & parentKeyName);
	inline static ElektraDiff calculateDiff (const KeySet & newKeys, const KeySet & oldKeys, const Key & parentKey);

	inline void undo (KeySet & ks);

	// reference handling

	inline void operator++ (int) const;
	inline void operator++ () const;

	inline void operator-- (int) const;
	inline void operator-- () const;

	inline ckdb::ElektraDiff * getDiff () const;
	inline ckdb::ElektraDiff * operator* () const;

	inline ElektraDiff & operator= (const ElektraDiff & other);

	inline uint16_t getReferenceCounter () const;

	inline void removeOther (std::string const & parentKeyName);
	inline void removeOther (const Key & parentKey);

	inline void removeSameOrBelow (std::string const & cutpointName);
	inline void removeSameOrBelow (const Key & cutpoint);

	inline void removeKey (std::string const & keyName);
	inline void removeKey (const Key & key);

	inline ElektraDiff cut (std::string const & cutpointName);
	inline ElektraDiff cut (const Key & cutpoint);

	inline ElektraDiff dup () const;

	inline bool isEmpty () const;
	inline KeySet getAddedKeys () const;
	inline KeySet getModifiedKeys () const;
	inline KeySet getRemovedKeys () const;

	inline KeySet getAddedMetaKeys (std::string const & keyName) const;
	inline KeySet getAddedMetaKeys (const Key & key) const;

	inline KeySet getModifiedMetaKeys (std::string const & keyName) const;
	inline KeySet getModifiedMetaKeys (const Key & key) const;

	inline KeySet getRemovedMetaKeys (std::string const & keyName) const;
	inline KeySet getRemovedMetaKeys (const Key & key) const;

private:
	ckdb::ElektraDiff * diff;
};

inline ElektraDiff::ElektraDiff () : diff (nullptr)
{
}

/**
 * Constructs a diff out of a C diff.
 *
 * @param cdiff the diff to work with
 */
inline ElektraDiff::ElektraDiff (ckdb::ElektraDiff * cdiff) : diff (cdiff)
{
	if (diff == nullptr)
	{
		throw ElektraDiffNullException ();
	}

	operator++ ();
}

/**
 * Takes a reference of another diff.
 *
 * The diff will not be copied, but the reference
 * counter will be increased.
 *
 * @param other the diff to work with
 */
inline ElektraDiff::ElektraDiff (ElektraDiff & other) : ElektraDiff (other.diff)
{
}

/**
 * Takes a reference of another diff.
 *
 * The diff will not be copied, but the reference
 * counter will be increased.
 *
 * @param other the diff to work with
 */
inline ElektraDiff::ElektraDiff (ElektraDiff const & other) : ElektraDiff (other.diff)
{
}

inline ElektraDiff::~ElektraDiff ()
{
	if (!diff)
	{
		return;
	}

	operator-- ();
	ckdb::elektraDiffDel (diff);
}

/**
 * Calculates the difference between the given keysets
 * The diff will contain the keys that were added, modified and removed in @p newKeys.
 *
 * @param newKeys the new keyset
 * @param oldKeys the old keyset
 * @param parentKeyName only changes same or below this key will be calculated
 * @return diff of the two given keysets
 */
inline ElektraDiff ElektraDiff::calculateDiff (const KeySet & newKeys, const KeySet & oldKeys, const std::string & parentKeyName)
{
	Key parentKey (parentKeyName, KEY_END);
	return calculateDiff (newKeys, oldKeys, parentKey);
}

/**
 * Calculates the difference between the given keysets
 * The diff will contain the keys that were added, modified and removed in @p newKeys.
 *
 * @param newKeys the new keyset
 * @param oldKeys the old keyset
 * @param parentKey only changes same or below this key will be calculated
 * @return diff of the two given keysets
 */
inline ElektraDiff ElektraDiff::calculateDiff (const KeySet & newKeys, const KeySet & oldKeys, const Key & parentKey)
{
	ckdb::ElektraDiff * diff = ckdb::elektraDiffCalculate (newKeys.getKeySet (), oldKeys.getKeySet (), parentKey.getKey ());
	return ElektraDiff (diff);
}

/**
 * Undo the changes represented in this diff
 * @param ks the keyset where the changs should be undone
 */
inline void ElektraDiff::undo (KeySet & ks)
{
	ckdb::elektraDiffUndo (diff, ks.getKeySet());
}

/**
 * @copydoc elektraDiffIncRef
 */
inline void ElektraDiff::operator++ (int) const
{
	operator++ ();
}

/**
 * @copydoc elektraDiffIncRef
 */
inline void ElektraDiff::operator++ () const
{
	ckdb::elektraDiffIncRef (diff);
}

/**
 * @copydoc elektraDiffDecRef
 */
inline void ElektraDiff::operator-- (int) const
{
	operator-- ();
}

/**
 * @copydoc elektraDiffDecRef
 */
inline void ElektraDiff::operator-- () const
{
	ckdb::elektraDiffDecRef (diff);
}

/**
 * @copydoc elektraDiffGetRef
 */
inline uint16_t ElektraDiff::getReferenceCounter () const
{
	return ckdb::elektraDiffGetRef (diff);
}

/**
 * Assign a diff
 *
 * @param other the diff to assign
 * @return reference to this
 */
inline ElektraDiff & ElektraDiff::operator= (const ElektraDiff & other)
{
	if (this != &other)
	{
		if (diff != nullptr)
		{
			operator-- ();
			ckdb::elektraDiffDel (diff);
		}

		diff = other.diff;
		operator++ ();
	}

	return *this;
}


/**
 * Passes out the raw diff pointer.
 *
 * This pointer can be used to directly change the underlying diff
 * object.
 *
 * \note that the ownership remains in the object
 */
inline ckdb::ElektraDiff * ElektraDiff::getDiff () const
{
	return diff;
}

/**
 * Passes out the raw diff pointer.
 *
 * This pointer can be used to directly change the underlying diff
 * object.
 *
 * \note that the ownership remains in the object
 */
inline ckdb::ElektraDiff * ElektraDiff::operator* () const
{
	return diff;
}

/**
 * @copydoc elektraDiffRemoveOther
 */
inline void ElektraDiff::removeOther (std::string const & parentKeyName)
{
	Key parentKey (parentKeyName, KEY_END);
	return removeOther (parentKey);
}

/**
 * @copydoc elektraDiffRemoveOther
 */
inline void ElektraDiff::removeOther (const Key & parentKey)
{
	ckdb::elektraDiffRemoveOther (diff, parentKey.getKey ());
}

/**
 * @copydoc elektraDiffRemoveSameOrBelow
 */
inline void ElektraDiff::removeSameOrBelow (std::string const & cutpointName)
{
	Key cutpoint (cutpointName, KEY_END);
	return removeSameOrBelow (cutpoint);
}

/**
 * @copydoc elektraDiffRemoveSameOrBelow
 */
inline void ElektraDiff::removeSameOrBelow (const Key & cutpoint)
{
	ckdb::elektraDiffRemoveSameOrBelow (diff, cutpoint.getKey ());
}

/**
 * @copydoc elektraDiffRemoveKey
 */
inline void ElektraDiff::removeKey (std::string const & keyName)
{
	Key key (keyName, KEY_END);
	return removeKey (key);
}

/**
 * @copydoc elektraDiffRemoveKey
 */
inline void ElektraDiff::removeKey (const Key & key)
{
	ckdb::elektraDiffRemoveKey (diff, key.getKey ());
}

/**
 * @copydoc elektraDiffCut
 */
inline ElektraDiff ElektraDiff::cut (std::string const & cutpointName)
{
	Key cutpoint (cutpointName, KEY_END);
	return cut (cutpoint);
}

/**
 * @copydoc elektraDiffCut
 */
inline ElektraDiff ElektraDiff::cut (const Key & cutpoint)
{
	return ElektraDiff (ckdb::elektraDiffCut (diff, cutpoint.getKey ()));
}

/**
 * @copydoc elektraDiffDup
 */
inline ElektraDiff ElektraDiff::dup () const
{
	return ElektraDiff (ckdb::elektraDiffDup (diff));
}

/**
 * @copydoc elektraDiffIsEmpty
 */
inline bool ElektraDiff::isEmpty () const
{
	return ckdb::elektraDiffIsEmpty (diff);
}

/**
 * @copydoc elektraDiffGetAddedKeys
 */
inline KeySet ElektraDiff::getAddedKeys () const
{
	return { ckdb::elektraDiffGetAddedKeys (diff) };
}

/**
 * @copydoc elektraDiffGetModifiedKeys
 */
inline KeySet ElektraDiff::getModifiedKeys () const
{
	return { ckdb::elektraDiffGetModifiedKeys (diff) };
}

/**
 * @copydoc elektraDiffGetRemovedKeys
 */
inline KeySet ElektraDiff::getRemovedKeys () const
{
	return { ckdb::elektraDiffGetRemovedKeys (diff) };
}

/**
 * @copydoc elektraDiffGetAddedMetaKeys
 */
inline KeySet ElektraDiff::getAddedMetaKeys (std::string const & keyName) const
{
	Key key (keyName, KEY_END);
	return getAddedMetaKeys (key);
}

/**
 * @copydoc elektraDiffGetAddedMetaKeys
 */
inline KeySet ElektraDiff::getAddedMetaKeys (const Key & key) const
{
	return { ckdb::elektraDiffGetAddedMetaKeys (diff, key.getKey ()) };
}

/**
 * @copydoc elektraDiffGetModifiedMetaKeys
 */
inline KeySet ElektraDiff::getModifiedMetaKeys (std::string const & keyName) const
{
	Key key (keyName, KEY_END);
	return getModifiedMetaKeys (key);
}

/**
 * @copydoc elektraDiffGetModifiedMetaKeys
 */
inline KeySet ElektraDiff::getModifiedMetaKeys (const Key & key) const
{
	return { ckdb::elektraDiffGetModifiedMetaKeys (diff, key.getKey ()) };
}

/**
 * @copydoc elektraDiffGetRemovedMetaKeys
 */
inline KeySet ElektraDiff::getRemovedMetaKeys (std::string const & keyName) const
{
	Key key (keyName, KEY_END);
	return getRemovedMetaKeys (key);
}

/**
 * @copydoc elektraDiffGetRemovedMetaKeys
 */
inline KeySet ElektraDiff::getRemovedMetaKeys (const Key & key) const
{
	return { ckdb::elektraDiffGetRemovedMetaKeys (diff, key.getKey ()) };
}

}

#endif // ELEKTRA_ELEKTRADIFF_HPP
