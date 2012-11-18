#ifndef CPP_KEYSET_H
#define CPP_KEYSET_H

#include <string>

#include <key.hpp>

#include <kdb.h>

namespace kdb {

/**
 * Represents an elektra keyset.
 *
 * \invariant always holds an underlying elektra keyset.
 *
 * \note that the cursor is mutable,
 * so it might be changed even in const functions.
 * */
class KeySet
{
public:
	KeySet () :ks (ckdb::ksNew(0)) {}
	/**
	 * Takes ownership of keyset!
	 *
	 * Keyset will be destroyed at destructor
	 * you cant continue to use keyset afterwards!
	 *
	 * Use KeySet::release() to avoid destruction.
	*/
	KeySet (ckdb::KeySet *k) :ks(k) {}
	KeySet (const KeySet &other);
	KeySet (size_t alloc, va_list ap);
	KeySet (size_t alloc, ...);
	~KeySet ();

	/**
	 * If you dont want destruction of keyset at
	 * the end you can release the pointer.
	 * */
	ckdb::KeySet * release() {ckdb::KeySet *ret = ks; ks = ckdb::ksNew(0); return ret;}

	ckdb::KeySet * getKeySet () const { return ks; }
	void setKeySet (ckdb::KeySet * k) { ks = k; }

	KeySet& operator=(KeySet const& other);

	ckdb::KeySet* dup () const { return ckdb::ksDup(ks); }

	void copy (const KeySet &other) { ckdb::ksCopy(ks,other.ks); }
	void clear () { ckdb::ksCopy(ks,0); }

	KeySet cut (Key k) { return KeySet(ckdb::ksCut(ks,*k)); }

	size_t size () const;

	ssize_t append (const Key &toAppend);
	ssize_t append (const KeySet &toAppend);

	Key pop();

	Key head() const;
	Key tail() const;

	void rewind () const;
	Key next() const;
	Key current() const;

	void setCursor(cursor_t cursor) const;
	cursor_t getCursor() const;

	Key lookup (const Key &k, const option_t options = KDB_O_NONE) const;
	Key lookup (const std::string &name, const option_t options = KDB_O_NONE) const;
	Key lookup (const char *name, const option_t options = KDB_O_NONE) const;

private:
	ckdb::KeySet * ks; ///< holds an elektra keyset
};

inline KeySet::KeySet (size_t alloc, va_list ap)
{
	ks = ckdb::ksVNew (alloc, ap);
}

inline KeySet::KeySet (size_t alloc, ...)
{
	va_list va;

	va_start (va, alloc);
	ks = ckdb::ksVNew (alloc, va);
	va_end (va);
}

/**Duplicate a keyset.
 *
 * This keyset will be a duplicate of the other afterwards.
 *
 * @note that they still reference to the same Keys, so
 *       if you change key values also the keys in the original keyset
 *       will be changed.
 */
inline KeySet::KeySet (const KeySet &other)
{
	ks = other.dup();
}

inline KeySet::~KeySet ()
{
	ckdb::ksDel (ks);
}

/**Duplicate a keyset.
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
		ckdb::ksDel (ks);
		ks = other.dup();
	}
	return *this;
}

inline size_t KeySet::size () const
{
	return ckdb::ksGetSize(ks);
}

inline void KeySet::rewind () const
{
	ckdb::ksRewind(ks);
}


inline void KeySet::setCursor(cursor_t cursor) const
{
	ckdb::ksSetCursor(ks, cursor);
}

inline cursor_t KeySet::getCursor() const
{
	return ckdb::ksGetCursor(ks);
}


inline Key KeySet::next() const
{
	ckdb::Key *k = ckdb::ksNext(ks);
	return Key(k);
}

inline Key KeySet::current() const
{
	return Key(ckdb::ksCurrent(ks));
}

inline Key KeySet::head() const
{
	return Key(ckdb::ksHead(ks));
}

inline Key KeySet::tail() const
{
	return Key(ckdb::ksTail(ks));
}

inline Key KeySet::pop()
{
	ckdb::Key *k = ckdb::ksPop(ks);
	Key key(k);
	return key;
}

inline Key KeySet::lookup (const Key &key, const option_t options) const
{
	ckdb::Key *k = ckdb::ksLookup(ks, key.getKey(), options);
	return Key(k);
}

inline Key KeySet::lookup (const std::string &name, const option_t options) const
{
	ckdb::Key *k = ckdb::ksLookupByName(ks, name.c_str(), options);
	return Key(k);
}

inline Key KeySet::lookup (const char *name, const option_t options) const
{
	ckdb::Key *k = ckdb::ksLookupByName(ks, name, options);
	return Key(k);
}

inline ssize_t KeySet::append (const Key &toAppend)
{
	return ckdb::ksAppendKey(ks, toAppend.getKey());
}

inline ssize_t KeySet::append (const KeySet &toAppend)
{
	return ckdb::ksAppend(ks, toAppend.getKeySet());
}

} // end of namespace kdb

#endif

