/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KEYSET_HPP
#define ELEKTRA_KEYSET_HPP

#ifndef ELEKTRA_WRONG
#define ELEKTRA_WRONG // make swig happy
#endif

#include <elektra/kdb.h>
#include <key.hpp>
#include <string>

namespace kdb
{

#ifndef ELEKTRA_WITHOUT_ITERATOR
class KeySetIterator;
class KeySetReverseIterator;
#endif


/**
 * @brief Needed to avoid constructor ambiguity
 *
 * when ... is same type as va_list
 */
struct VaAlloc
{
	explicit VaAlloc (size_t size) : alloc (size)
	{
	}
	size_t alloc;
};

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
	inline KeySet ();
	inline KeySet (ckdb::KeySet * k);
	inline KeySet (const KeySet & other);

	ELEKTRA_WRONG explicit KeySet (Key, ...);

	inline explicit KeySet (size_t alloc, ...) ELEKTRA_SENTINEL;
	inline explicit KeySet (VaAlloc alloc, va_list ap);

	inline ~KeySet ();

	ckdb::KeySet * release ();

	ckdb::KeySet * getKeySet () const;

	KeySet & operator= (KeySet const & other);

	ssize_t size () const;

	ckdb::KeySet * dup () const;

	void copy (const KeySet & other);
	void clear ();

	ssize_t append (const Key & toAppend);
	ssize_t append (const KeySet & toAppend);

	Key pop ();
	Key at (elektraCursor pos) const;

	KeySet cut (Key k);

	Key lookup (const Key & k, const elektraLookupFlags options = KDB_O_NONE) const;
	Key lookup (std::string const & name, const elektraLookupFlags options = KDB_O_NONE) const;
	template <typename T>
	T get (std::string const & name, const elektraLookupFlags options = KDB_O_NONE) const;

	ssize_t search (const Key & toSearch) const;
	ssize_t search (std::string const & name) const;

	// operators
	inline bool operator== (const KeySet & ks) const;
	inline bool operator!= (const KeySet & ks) const;

#ifndef ELEKTRA_WITHOUT_ITERATOR
	typedef KeySetIterator iterator;
	typedef KeySetIterator const_iterator;
	typedef KeySetReverseIterator reverse_iterator;
	typedef KeySetReverseIterator const_reverse_iterator;

	iterator begin ();
	const_iterator begin () const;
	iterator end ();
	const_iterator end () const;
	reverse_iterator rbegin ();
	const_reverse_iterator rbegin () const;
	reverse_iterator rend ();
	const_reverse_iterator rend () const;

	const_iterator cbegin () const noexcept;
	const_iterator cend () const noexcept;
	const_reverse_iterator crbegin () const noexcept;
	const_reverse_iterator crend () const noexcept;
#endif // ELEKTRA_WITHOUT_ITERATOR

private:
	ckdb::KeySet * ks; ///< holds an elektra keyset
};


#ifndef ELEKTRA_WITHOUT_ITERATOR
/**
 * For C++ forward Iteration over KeySets.
 * (External Iterator)
 * @code
	for (Key k:ks3)
	{
		std::cout << k.getName() << std::endl;
	}
 * @endcode
 */
class KeySetIterator
{
public:
	typedef Key value_type;
	typedef elektraCursor difference_type;
	typedef Key pointer;
	typedef Key reference;
	typedef std::random_access_iterator_tag iterator_category;

	KeySetIterator (KeySet const & k) : ks (k), current (){};
	KeySetIterator (KeySet const & k, const elektraCursor c) : ks (k), current (c){};
	// conversion to const iterator?

	Key get () const
	{
		return Key (ckdb::ksAtCursor (ks.getKeySet (), current));
	}
	Key get (elektraCursor pos) const
	{
		return Key (ckdb::ksAtCursor (ks.getKeySet (), pos));
	}

	KeySet const & getKeySet () const
	{
		return ks;
	}

	// Forward iterator requirements
	reference operator* () const
	{
		return get ();
	}
	pointer operator->() const
	{
		return get ();
	}
	KeySetIterator & operator++ ()
	{
		++current;
		return *this;
	}
	KeySetIterator operator++ (int)
	{
		return KeySetIterator (ks, current++);
	}

	// Bidirectional iterator requirements
	KeySetIterator & operator-- ()
	{
		--current;
		return *this;
	}
	KeySetIterator operator-- (int)
	{
		return KeySetIterator (ks, current--);
	}

	// Random access iterator requirements
	reference operator[] (const difference_type & pos) const
	{
		return get (pos);
	}
	KeySetIterator & operator+= (const difference_type & pos)
	{
		current += pos;
		return *this;
	}
	KeySetIterator operator+ (const difference_type & pos) const
	{
		return KeySetIterator (ks, current + pos);
	}
	KeySetIterator & operator-= (const difference_type & pos)
	{
		current -= pos;
		return *this;
	}
	KeySetIterator operator- (const difference_type & pos) const
	{
		return KeySetIterator (ks, current - pos);
	}
	const elektraCursor & base () const
	{
		return current;
	}

private:
	KeySet const & ks;
	elektraCursor current;
};


// Forward iterator requirements
inline bool operator== (const KeySetIterator & lhs, const KeySetIterator & rhs)
{
	return &lhs.getKeySet () == &rhs.getKeySet () && lhs.base () == rhs.base ();
}

inline bool operator!= (const KeySetIterator & lhs, const KeySetIterator & rhs)
{
	return &lhs.getKeySet () != &rhs.getKeySet () || lhs.base () != rhs.base ();
}

// Random access iterator requirements
inline bool operator<(const KeySetIterator & lhs, const KeySetIterator & rhs)
{
	return lhs.base () < rhs.base ();
}

inline bool operator> (const KeySetIterator & lhs, const KeySetIterator & rhs)
{
	return lhs.base () > rhs.base ();
}

inline bool operator<= (const KeySetIterator & lhs, const KeySetIterator & rhs)
{
	return lhs.base () <= rhs.base ();
}

inline bool operator>= (const KeySetIterator & lhs, const KeySetIterator & rhs)
{
	return lhs.base () >= rhs.base ();
}

// DR 685.
inline auto operator- (const KeySetIterator & lhs, const KeySetIterator & rhs) -> decltype (lhs.base () - rhs.base ())
{
	return lhs.base () - rhs.base ();
}

inline KeySetIterator operator+ (KeySetIterator::difference_type n, const KeySetIterator & i)
{
	return KeySetIterator (i.getKeySet (), i.base () + n);
}


// some code duplication because std::reverse_iterator
// does not work on value_types
/**
 * For C++ reverse Iteration over KeySets.
 * (External Iterator)
 */
class KeySetReverseIterator
{
public:
	typedef Key value_type;
	typedef elektraCursor difference_type;
	typedef Key pointer;
	typedef Key reference;
	typedef std::random_access_iterator_tag iterator_category;

	KeySetReverseIterator (KeySet const & k) : ks (k), current (){};
	KeySetReverseIterator (KeySet const & k, const elektraCursor c) : ks (k), current (c){};
	// conversion to const iterator?

	Key get () const
	{
		return Key (ckdb::ksAtCursor (ks.getKeySet (), current));
	}
	Key get (elektraCursor pos) const
	{
		return Key (ckdb::ksAtCursor (ks.getKeySet (), pos));
	}

	KeySet const & getKeySet () const
	{
		return ks;
	}

	// Forward iterator requirements
	reference operator* () const
	{
		return get ();
	}
	pointer operator->() const
	{
		return get ();
	}
	KeySetReverseIterator & operator++ ()
	{
		--current;
		return *this;
	}
	KeySetReverseIterator operator++ (int)
	{
		return KeySetReverseIterator (ks, current--);
	}

	// Bidirectional iterator requirements
	KeySetReverseIterator & operator-- ()
	{
		++current;
		return *this;
	}
	KeySetReverseIterator operator-- (int)
	{
		return KeySetReverseIterator (ks, current++);
	}

	// Random access iterator requirements
	reference operator[] (const difference_type & pos) const
	{
		return get (ks.size () - pos - 1);
	}
	KeySetReverseIterator & operator+= (const difference_type & pos)
	{
		current -= pos;
		return *this;
	}
	KeySetReverseIterator operator+ (const difference_type & pos) const
	{
		return KeySetReverseIterator (ks, current - pos);
	}
	KeySetReverseIterator & operator-= (const difference_type & pos)
	{
		current += pos;
		return *this;
	}
	KeySetReverseIterator operator- (const difference_type & pos) const
	{
		return KeySetReverseIterator (ks, current + pos);
	}
	const elektraCursor & base () const
	{
		return current;
	}

private:
	KeySet const & ks;
	elektraCursor current;
};


// Forward iterator requirements
inline bool operator== (const KeySetReverseIterator & lhs, const KeySetReverseIterator & rhs)
{
	return &lhs.getKeySet () == &rhs.getKeySet () && lhs.base () == rhs.base ();
}

inline bool operator!= (const KeySetReverseIterator & lhs, const KeySetReverseIterator & rhs)
{
	return &lhs.getKeySet () != &rhs.getKeySet () || lhs.base () != rhs.base ();
}

// Random access iterator requirements
inline bool operator<(const KeySetReverseIterator & lhs, const KeySetReverseIterator & rhs)
{
	return lhs.base () < rhs.base ();
}

inline bool operator> (const KeySetReverseIterator & lhs, const KeySetReverseIterator & rhs)
{
	return lhs.base () > rhs.base ();
}

inline bool operator<= (const KeySetReverseIterator & lhs, const KeySetReverseIterator & rhs)
{
	return lhs.base () <= rhs.base ();
}

inline bool operator>= (const KeySetReverseIterator & lhs, const KeySetReverseIterator & rhs)
{
	return lhs.base () >= rhs.base ();
}

// DR 685.
inline auto operator- (const KeySetReverseIterator & lhs, const KeySetReverseIterator & rhs) -> decltype (lhs.base () - rhs.base ())
{
	return lhs.base () - rhs.base ();
}

inline KeySetReverseIterator operator+ (KeySetReverseIterator::difference_type n, const KeySetReverseIterator & i)
{
	return KeySetReverseIterator (i.getKeySet (), i.base () + n);
}


inline KeySet::iterator KeySet::begin ()
{
	return KeySet::iterator (*this, 0);
}

inline KeySet::const_iterator KeySet::begin () const
{
	return KeySet::const_iterator (*this, 0);
}

inline KeySet::iterator KeySet::end ()
{
	return KeySet::iterator (*this, size ());
}

inline KeySet::const_iterator KeySet::end () const
{
	return KeySet::const_iterator (*this, size ());
}

inline KeySet::reverse_iterator KeySet::rbegin ()
{
	return KeySet::reverse_iterator (*this, size () - 1);
}

inline KeySet::const_reverse_iterator KeySet::rbegin () const
{
	return KeySet::const_reverse_iterator (*this, size () - 1);
}

inline KeySet::reverse_iterator KeySet::rend ()
{
	return KeySet::reverse_iterator (*this, -1);
}

inline KeySet::const_reverse_iterator KeySet::rend () const
{
	return KeySet::const_reverse_iterator (*this, -1);
}

inline KeySet::const_iterator KeySet::cbegin () const noexcept
{
	return KeySet::const_iterator (*this, 0);
}

inline KeySet::const_iterator KeySet::cend () const noexcept
{
	return KeySet::const_iterator (*this, size ());
}

inline KeySet::const_reverse_iterator KeySet::crbegin () const noexcept
{
	return KeySet::const_reverse_iterator (*this, size () - 1);
}

inline KeySet::const_reverse_iterator KeySet::crend () const noexcept
{
	return KeySet::const_reverse_iterator (*this, -1);
}
#endif // ELEKTRA_WITHOUT_ITERATOR


/**
 * Creates a new empty keyset with no keys
 *
 * @copydoc ksNew
 */
inline KeySet::KeySet () : ks (ckdb::ksNew (0, KS_END))
{
}

/**
 * Take ownership of a `ckdb::KeySet *`.
 *
 * // TODO: use ksIncRef/ksDecRef
 *
 * @param keyset the `KeySet *` to take the ownership of
 * @see release()
 */
inline KeySet::KeySet (ckdb::KeySet * keyset) : ks (keyset)
{
}

/**
 * Duplicate a keyset.
 *
 *  This keyset will be a duplicate of the other afterwards.
 *
 * @note that they still reference to the same Keys, so
 *       if you change key values also the keys in the original keyset
 *       will be changed.
 *
 * So it is shallow copy, to create a deep copy you have to dup() every
 * key (it still won't copy metadata, but they are COW):
 * @snippet cpp_example_dup.cpp ksDeepCopy
 *
 * @see dup
 */
inline KeySet::KeySet (const KeySet & other)
{
	ks = other.dup ();
}

/**
 * @brief Create a new keyset.
 *
 * @param alloc minimum number of keys to allocate
 * @param ap variable arguments list
 *
 * Use va as first argument to use this constructor, e.g.:
 * @code
 * KeySet ks(va, 23, ...);
 * @endcode
 *
 * @copydoc ksVNew
 */
inline KeySet::KeySet (VaAlloc alloc, va_list av)
{
	ks = ckdb::ksVNew (alloc.alloc, av);
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
	va_list vl;

	va_start (vl, alloc);
	ks = ckdb::ksVNew (alloc, vl);
	va_end (vl);
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
 * If you don't want destruction of keyset at
 * the end you can release the pointer.
 * */
inline ckdb::KeySet * KeySet::release ()
{
	ckdb::KeySet * ret = ks;
	ks = ckdb::ksNew (0, KS_END);
	return ret;
}

/**
 * @brief Passes out the raw keyset pointer
 *
 * This function exists so that pure C functions that do not
 * have a C++ binding can be called.
 *
 * @return pointer to internal ckdb KeySet
 */
inline ckdb::KeySet * KeySet::getKeySet () const
{
	return ks;
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
inline KeySet & KeySet::operator= (KeySet const & other)
{
	if (this != &other)
	{
		ckdb::ksDel (ks);
		ks = other.dup ();
	}
	return *this;
}

/**
 * @brief The size of the keyset
 *
 * @return the number of keys in the keyset
 */
inline ssize_t KeySet::size () const
{
	return ckdb::ksGetSize (ks);
}

/**
 * @brief Duplicate a keyset
 *
 * @return a copy of the keys
 *
 * This is only a shallow copy. For a deep copy you need to dup every
 * key.
 *
 * @copydoc ksDup()
 */
inline ckdb::KeySet * KeySet::dup () const
{
	return ckdb::ksDup (ks);
}

/**
 * @brief Copy a keyset
 *
 * Replaces all keys in `this` with the ones from `other`.
 * This is only a shallow copy. For a deep copy you need to manually
 * Key::dup every key.
 *
 * @param other other keyset to copy
 *
 * @copydoc ksCopy()
 */
inline void KeySet::copy (const KeySet & other)
{
	ckdb::ksCopy (ks, other.ks);
}

/**
 * @brief Clear the keyset
 *
 * Keyset will be empty afterwards.
 */
inline void KeySet::clear ()
{
	ckdb::ksClear (ks);
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
inline ssize_t KeySet::append (const Key & toAppend)
{
	return ckdb::ksAppendKey (ks, toAppend.getKey ());
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
	return ckdb::ksAppend (ks, toAppend.getKeySet ());
}

/**
 * @copydoc ksPop()
 */
inline Key KeySet::pop ()
{
	ckdb::Key * k = ckdb::ksPop (ks);
	Key key (k);
	return key;
}

/**
 * @brief Lookup a key by index
 *
 * @param pos cursor position
 *
 * @return the found key
 */
inline Key KeySet::at (elektraCursor pos) const
{
	if (pos < 0) pos += size ();
	return Key (ckdb::ksAtCursor (ks, pos));
}

/**
 * @copydoc ksCut()
 */
inline KeySet KeySet::cut (Key k)
{
	return KeySet (ckdb::ksCut (ks, k.getKey ()));
}

/**
 * @copydoc ksLookup()
 *
 * @note That the internal key cursor will point to the found key
 */
inline Key KeySet::lookup (const Key & key, const elektraLookupFlags options) const
{
	ckdb::Key * k = ckdb::ksLookup (ks, key.getKey (), options);
	return Key (k);
}

/**
 * @brief Lookup a key by name
 *
 * @param name the name to look for
 * @param options some options to pass
 *
 * @return the found key
 * @see lookup (const Key &key, const elektraLookupFlags options)
 *
 * @note That the internal key cursor will point to the found key
 */
inline Key KeySet::lookup (std::string const & name, elektraLookupFlags const options) const
{
	ckdb::Key * k = ckdb::ksLookupByName (ks, name.c_str (), options);
	return Key (k);
}

template <typename T>
struct KeySetTypeWrapper;

template <typename T>
struct KeySetTypeWrapper
{
	T operator() (KeySet const & ks, std::string const & name, elektraLookupFlags const options) const
	{
		Key k = ks.lookup (name, options);
		if (!k) throw kdb::KeyNotFoundException ("key " + name + " was not found");
		return k.get<T> ();
	}
};

/**
 * @brief Generic lookup+get for keysets
 *
 * @param name the key name to get
 * @param options the options to be passed to lookup()
 *
 * @throw KeyNotFoundException if no key found
 *
 * @note To specialize more complex types (which are generic themselves) you
 * can also specialize KeySetTypeWrapper<T>.
 *
 * Use
 * @code
#include <keysetget.hpp>
 * @endcode
 * to include specializations for std types.
 *
 * @return the requested type
 */
template <typename T>
inline T KeySet::get (std::string const & name, elektraLookupFlags const options) const
{
	KeySetTypeWrapper<T> typeWrapper;
	return typeWrapper (*this, name, options);
}

inline bool KeySet::operator== (const KeySet & o) const
{
	return size () == o.size () && std::equal (begin (), end (), o.begin ());
}


inline bool KeySet::operator!= (const KeySet & o) const
{
	return !(*this == o);
}


/**
 * @copydoc ksSearch()
 */
inline ssize_t KeySet::search (const Key & toSearch) const
{
	return ckdb::ksSearch (ks, toSearch.getKey ());
}

/**
 * @copydoc ksSearch()
 *
 * @note Accepts a keyname as string instead of a Key object.
 */
inline ssize_t KeySet::search (const std::string & name) const
{
	return search (Key (name));
}


} // end of namespace kdb

#endif
