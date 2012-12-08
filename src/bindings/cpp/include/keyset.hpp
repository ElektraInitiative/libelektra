#ifndef ELEKTRA_KEYSET_HPP
#define ELEKTRA_KEYSET_HPP

#include <string>

#include <key.hpp>

#include <kdb.h>

namespace kdb {

/**
 * @brief A keyset holds together a set of keys.
 *
 * @copydoc keyset
 *
 * \invariant always holds an underlying elektra keyset.
 *
 * \note that the cursor is mutable,
 * so it might be changed even in const functions
 * as described.
 * */
class KeySet
{
public:
	inline KeySet();
	inline KeySet(ckdb::KeySet *k);
	inline KeySet(const KeySet &other);

	inline explicit KeySet(size_t alloc, va_list ap);
	inline explicit KeySet(size_t alloc, ...);

	inline ~KeySet ();

	ckdb::KeySet * release();

	ckdb::KeySet * getKeySet() const;
	void setKeySet(ckdb::KeySet * k);

	KeySet& operator=(KeySet const& other);

	size_t size() const;

	ckdb::KeySet* dup() const;

	void copy(const KeySet &other);
	void clear();

	ssize_t append(const Key &toAppend);
	ssize_t append(const KeySet &toAppend);

	Key head() const;
	Key tail() const;

	void rewind() const;
	Key next() const;
	Key current() const;

	void setCursor(cursor_t cursor) const;
	cursor_t getCursor() const;

	Key pop();

	KeySet cut (Key k);

	Key lookup (const Key &k, const option_t options = KDB_O_NONE) const;
	Key lookup (std::string const & name, const option_t options = KDB_O_NONE) const;

private:
	ckdb::KeySet * ks; ///< holds an elektra keyset
};


/**
 * Creates a new empty keyset with no keys
 *
 * @copydoc ksNew
 */
inline KeySet::KeySet () :
	ks (ckdb::ksNew(0))
{}

/**
 * Takes ownership of keyset!
 *
 * Keyset will be destroyed at destructor
 * you cant continue to use keyset afterwards!
 *
 * Use KeySet::release() to avoid destruction.
 *
 * @param k the keyset to take the ownership from
 * @see release()
 * @see setKeySet()
 */
inline KeySet::KeySet (ckdb::KeySet *k) :
	ks(k)
{}

/**
 * Duplicate a keyset.
 *
 *  This keyset will be a duplicate of the other afterwards.
 *
 * @note that they still reference to the same Keys, so
 *       if you change key values also the keys in the original keyset
 *       will be changed.
 *
 * @see dup
 */
inline KeySet::KeySet (const KeySet &other)
{
	ks = other.dup();
}

/**
 * @brief Create a new keyset.
 *
 * @param alloc minimum number of keys to allocate
 * @param ap variable arguments list
 *
 * @copydoc ksVNew
 */
inline KeySet::KeySet (size_t alloc, va_list ap)
{
	ks = ckdb::ksVNew (alloc, ap);
}

/**
 * @brief Create a new keyset
 *
 * @param alloc minimum number of keys to allocate
 * @param ... variable argument list
 *
 * @copydoc ksVNew
 */
inline KeySet::KeySet (size_t alloc, ...)
{
	va_list va;

	va_start (va, alloc);
	ks = ckdb::ksVNew (alloc, va);
	va_end (va);
}

/**
 * @brief Deconstruct a keyset
 *
 * @copydoc ksDel
 */
inline KeySet::~KeySet ()
{
	ckdb::ksDel (ks);
}

/**
 * If you dont want destruction of keyset at
 * the end you can release the pointer.
 * */
inline ckdb::KeySet * KeySet::release()
{
	ckdb::KeySet *ret = ks;
	ks = ckdb::ksNew(0);
	return ret;
}

/**
 * @brief Passes out the raw keyset pointer
 *
 * @return pointer to internal ckdb KeySet
 *
 * @see release()
 * @see setKeySet()
 */
inline ckdb::KeySet * KeySet::getKeySet () const
{
	return ks;
}

/**
 * @brief Take ownership of passed keyset
 *
 * @param k the keyset to take ownership from
 * @see release()
 * @see getKeySet()
 */
inline void KeySet::setKeySet (ckdb::KeySet * k)
{
	ckdb::ksDel(ks);
	ks = k;
}

/**
 * Duplicate a keyset.
 *
 * This keyset will be a duplicate of the other afterwards.
 *
 * @note that they still reference to the same Keys, so
 *       if you change key values also the keys in the original keyset
 *       will be changed.
 */
inline KeySet& KeySet::operator=(KeySet const& other)
{
	if (this != &other)
	{
		ckdb::ksDel(ks);
		ks = other.dup();
	}
	return *this;
}

/**
 * @brief The size of the keyset
 *
 * @return the number of keys in the keyset
 */
inline size_t KeySet::size () const
{
	return ckdb::ksGetSize(ks);
}

/**
 * @brief Duplicate a keyset
 *
 * @return a copy of the keys
 *
 * @copydoc ksDup()
 */
inline ckdb::KeySet* KeySet::dup () const
{
	return ckdb::ksDup(ks);
}

/**
 * @brief Copy a keyset
 *
 * @param other other keyset to copy
 *
 * @copydoc ksCopy()
 */
inline void KeySet::copy (const KeySet &other)
{
	ckdb::ksCopy(ks,other.ks);
}

/**
 * @brief Clear the keyset
 *
 * Keyset will have no keys afterwards.
 */
inline void KeySet::clear ()
{
	ckdb::ksCopy(ks,0);
}

/**
 * @brief append a key
 *
 * @param toAppend key to append
 *
 * @return number of keys in the keyset
 *
 * @copydoc ksAppendKey()
 */
inline ssize_t KeySet::append (const Key &toAppend)
{
	return ckdb::ksAppendKey(ks, toAppend.getKey());
}

/**
 * @brief append a keyset
 *
 * @param toAppend keyset to append
 *
 * @return number of keys in the keyset
 *
 * @copydoc ksAppend()
 */
inline ssize_t KeySet::append (KeySet const & toAppend)
{
	return ckdb::ksAppend(ks, toAppend.getKeySet());
}

/**
 * @return alphabetical first key
 *
 * @copydoc ksHead()
 */
inline Key KeySet::head() const
{
	return Key(ckdb::ksHead(ks));
}

/**
 * @return alphabetical last key
 *
 * @copydoc ksTail()
 */
inline Key KeySet::tail() const
{
	return Key(ckdb::ksTail(ks));
}


/**
 * @copydoc ksRewind()
 */
inline void KeySet::rewind () const
{
	ckdb::ksRewind(ks);
}

/**
 * @copydoc ksNext()
 */
inline Key KeySet::next() const
{
	ckdb::Key *k = ckdb::ksNext(ks);
	return Key(k);
}

/**
 * @copydoc ksCurrent()
 */
inline Key KeySet::current() const
{
	return Key(ckdb::ksCurrent(ks));
}

/**
 * @copydoc ksSetCursor()
 */
inline void KeySet::setCursor(cursor_t cursor) const
{
	ckdb::ksSetCursor(ks, cursor);
}

/**
 * @copydoc ksGetCursor()
 */
inline cursor_t KeySet::getCursor() const
{
	return ckdb::ksGetCursor(ks);
}

/**
 * @copydoc ksPop()
 */
inline Key KeySet::pop()
{
	ckdb::Key *k = ckdb::ksPop(ks);
	Key key(k);
	return key;
}

/**
 * @copydoc ksCut()
 */
inline KeySet KeySet::cut (Key k)
{
	return KeySet(ckdb::ksCut(ks,*k));
}

/**
 * @copydoc ksLookup()
 *
 * @note That the internal key cursor will point to the found key
 */
inline Key KeySet::lookup (const Key &key, const option_t options) const
{
	ckdb::Key *k = ckdb::ksLookup(ks, key.getKey(), options);
	return Key(k);
}

/**
 * @brief Lookup a key by name
 *
 * @param name the name to look for
 * @param options some options to pass
 *
 * @return the found key
 * @see lookup (const Key &key, const option_t options)
 *
 * @note That the internal key cursor will point to the found key
 */
inline Key KeySet::lookup (std::string const & name, option_t const options) const
{
	ckdb::Key *k = ckdb::ksLookupByName(ks, name.c_str(), options);
	return Key(k);
}

} // end of namespace kdb

#endif
