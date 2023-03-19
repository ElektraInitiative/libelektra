/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KEY_HPP
#define ELEKTRA_KEY_HPP

#include <cstdarg>
#include <cstring>

#if __GNUC__ >= 12
#pragma GCC diagnostic error "-Wmaybe-uninitialized"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#include <functional>
#pragma GCC diagnostic pop
#pragma GCC diagnostic pop
#else
#include <functional>
#endif

#include <locale>
#include <sstream>
#include <string>

#include <keyexcept.hpp>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/core/namespace.h>
namespace kdb
{

#ifndef ELEKTRA_WITHOUT_ITERATOR
class NameIterator;
class NameReverseIterator;
#endif

enum class ElektraNamespace : std::uint8_t
{
	NONE = KEY_NS_NONE,
	CASCADING = KEY_NS_CASCADING,
	META = KEY_NS_META,
	SPEC = KEY_NS_SPEC,
	PROC = KEY_NS_PROC,
	DIR = KEY_NS_DIR,
	USER = KEY_NS_USER,
	SYSTEM = KEY_NS_SYSTEM,
	DEFAULT = KEY_NS_DEFAULT,
};

static const ElektraNamespace ELEKTRA_NAMESPACES[] = {
	ElektraNamespace::CASCADING, ElektraNamespace::META, ElektraNamespace::SPEC,   ElektraNamespace::PROC,
	ElektraNamespace::DIR,	     ElektraNamespace::USER, ElektraNamespace::SYSTEM, ElektraNamespace::DEFAULT,
};

/**
 * @copydoc key
 *
 * This class is an wrapper for an optional, refcounted ckdb::Key.
 * It is like an shared_ptr<ckdb::Key>, but the
 * shared_ptr functionality is already within the Key and exposed
 * with this wrapper.
 *
 * @par optional
 * A key can be constructed with an null pointer, by using
 * Key (static_cast<ckdb::Key*>(0));
 * or made empty afterwards by using release() or assign a null key.
 * To check if there is an associated managed object the user
 * can use isNull().
 *
 * @par references
 * Copies of keys are cheap because they are only flat.
 * If you really need a deep copy, you can use copy() or dup().
 * If you release() an object, the reference counter will
 * stay
 * All other operations operate on references.
 *
 * @par documentation
 * Note that the documentation is typically copied from the underlying
 * function which is wrapped and sometimes extended with C++ specific
 * details. So you might find C examples within the C++ documentation.
 *
 *
 * @invariant Key either has a working underlying Elektra Key
 * object or a null pointer.
 * The Key, however, might be invalid (see isValid()) or null
 * (see isNull()).
 *
 * @note that the reference counting in the keys is mutable,
 * so that const keys can be passed around by value.
 */
class Key
{
public:
	// constructors

	inline Key ();
	inline Key (ckdb::Key * k);
	inline Key (Key & k);
	inline Key (Key const & k);

	inline explicit Key (const char * keyName, ...);
	inline explicit Key (const std::string keyName, ...);
	inline explicit Key (const char * keyName, va_list ap);


	// reference handling

	inline void operator++ (int) const;
	inline void operator++ () const;

	inline void operator-- (int) const;
	inline void operator-- () const;

	inline uint16_t getReferenceCounter () const;


	// basic methods


	inline Key & operator= (ckdb::Key * k);
	inline Key & operator= (const Key & k);

	inline void copy (const Key & other, elektraCopyFlags flags = KEY_CP_ALL);
	inline void clear ();

	inline Key * operator->();

	inline ckdb::Key * getKey () const;
	inline ckdb::Key * operator* () const;

	inline ckdb::Key * release ();
	inline ckdb::Key * dup (elektraCopyFlags flags = KEY_CP_ALL) const;
	inline ~Key ();


	// name operations


	inline std::string getName () const;
	inline ssize_t getNameSize () const;

	inline std::string getBaseName () const;
	inline ssize_t getBaseNameSize () const;

	inline void setName (const std::string & newName);
	inline void addName (const std::string & addedName);
	inline void setBaseName (const std::string & baseName);
	inline void addBaseName (const std::string & baseName);
	inline void delBaseName ();

#ifndef ELEKTRA_WITHOUT_ITERATOR
	typedef NameIterator iterator;
	typedef NameIterator const_iterator;
	typedef NameReverseIterator reverse_iterator;
	typedef NameReverseIterator const_reverse_iterator;

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


	// operators


	inline bool operator== (const Key & k) const;
	inline bool operator!= (const Key & k) const;
	inline bool operator<(const Key & other) const;
	inline bool operator<= (const Key & other) const;
	inline bool operator> (const Key & other) const;
	inline bool operator>= (const Key & other) const;

	inline bool isNull () const;
	inline operator bool () const;
	inline bool needSync () const;


	// value operations


	template <class T>
	inline T get () const;

	template <class T>
	inline void set (T x);

	inline std::string getString () const;
	inline void setString (const char * newString);
	inline void setString (const std::string & newString);
	inline ssize_t getStringSize () const;

	typedef void (*func_t) ();
	inline func_t getFunc () const;

	typedef ckdb::Key * (*callback_t) (ckdb::KeySet * ks, ckdb::Key * key, ckdb::Key * found, elektraLookupFlags flags);
	inline void setCallback (callback_t fct);

	inline const void * getValue () const;
	inline std::string getBinary () const;
	inline ssize_t getBinarySize () const;
	inline ssize_t setBinary (const void * newBinary, size_t dataSize);


	// metadata
	//
	//
	inline bool hasMeta (const std::string & metaName) const;

	template <class T>
	inline T getMeta (const std::string & metaName) const;

	template <class T>
	inline void setMeta (const std::string & metaName, T x);

	inline void delMeta (const std::string & metaName);

	inline void copyMeta (const Key & other, const std::string & metaName);
	inline void copyAllMeta (const Key & other);

	// Methods for Making tests
	inline bool isValid () const;
	inline ElektraNamespace getNamespace () const;
	inline ssize_t setNamespace (ElektraNamespace ns) const;
	inline bool isCascading () const;
	inline bool isSpec () const;
	inline bool isProc () const;
	inline bool isDir () const;
	inline bool isUser () const;
	inline bool isSystem () const;

	inline bool isString () const;
	inline bool isBinary () const;

	inline bool isBelow (const Key & k) const;
	inline bool isBelowOrSame (const Key & k) const;
	inline bool isDirectBelow (const Key & k) const;

	inline bool isNameLocked () const;
	inline bool isValueLocked () const;
	inline bool isMetaLocked () const;

private:
	inline int del ();

	ckdb::Key * key; ///< holds an elektra key
};

#ifndef ELEKTRA_WITHOUT_ITERATOR
/**
 * For C++ forward Iteration over Names.
 * (External Iterator)
 * @code
	for (std::string s:k3)
	{
		std::cout << s << std::endl;
	}
 * @endcode
 */
class NameIterator
{
public:
	typedef std::string value_type;
	typedef std::string reference;
	typedef std::string pointer; // STL typedef is required by SWIG
	typedef int difference_type; // STL typedef is required by SWIG
	typedef std::bidirectional_iterator_tag iterator_category;

	NameIterator (Key const & k, bool last)
	: begin (static_cast<const char *> (keyUnescapedName (*k))), end (begin + keyGetUnescapedNameSize (*k)),
	  current (last ? end : begin)
	{
	}

	NameIterator (const char * begin_, const char * end_, const char * current_) : begin (begin_), end (end_), current (current_)
	{
	}

	std::string get () const
	{
		if (current == end || current == begin - 1) return "";
		return std::string (current);
	}

	const char * pos () const
	{
		return current;
	}

	const char * findNext () const
	{
		if (end - begin == 3)
		{
			// special handling for root key
			return end;
		}

		const char * c = current >= end ? end - 1 : current;

		while (c < end && *c != '\0')
		{
			++c;
		}

		return c < end ? c + 1 : end;
	}

	const char * findPrevious () const
	{
		if (end - begin == 3)
		{
			// special handling for root key
			return begin;
		}

		const char * c = current >= end ? end - 1 : current - 1;

		do
		{
			--c;
		} while (c > begin && *c != '\0');

		return c > begin ? c + 1 : begin;
	}

	// Forward iterator requirements
	reference operator* () const
	{
		return get ();
	}
	NameIterator & operator++ ()
	{
		current = findNext ();
		return *this;
	}
	NameIterator operator++ (int)
	{
		NameIterator ret (begin, end, current);
		current = findNext ();
		return ret;
	}

	// Bidirectional iterator requirements
	NameIterator & operator-- ()
	{
		current = findPrevious ();
		return *this;
	}
	NameIterator operator-- (int)
	{
		NameIterator ret (begin, end, current);
		current = findPrevious ();
		return ret;
	}

protected:
	const char * begin;
	const char * end;
	const char * current;
};


// Forward iterator requirements
inline bool operator== (const NameIterator & lhs, const NameIterator & rhs)
{
	return lhs.pos () == rhs.pos ();
}

inline bool operator!= (const NameIterator & lhs, const NameIterator & rhs)
{
	return lhs.pos () != rhs.pos ();
}

// some code duplication because std::reverse_iterator
// needs a difference_type
/**
 * For C++ reverse Iteration over Names.
 * (External Iterator)
 */
class NameReverseIterator : private NameIterator
{
public:
	typedef std::string value_type;
	typedef std::string reference;
	typedef std::string pointer; // STL typedef is required by SWIG
	typedef int difference_type; // STL typedef is required by SWIG
	typedef std::bidirectional_iterator_tag iterator_category;

	NameReverseIterator (Key const & k, bool last) : NameIterator (k, last)
	{
		if (!last)
		{
			current = begin - 1;
		}
		else
		{
			current = findPrevious ();
		}
	}

	NameReverseIterator (const char * begin_, const char * end_, const char * current_) : NameIterator (begin_, end_, current_)
	{
	}

	const char * findPrevious () const
	{
		if (current <= begin) return begin - 1;
		return NameIterator::findPrevious ();
	}

	const char * findNext () const
	{
		if (current == begin - 1) return begin;
		return NameIterator::findNext ();
	}

	std::string get () const
	{
		if (current == begin - 1) return "";
		return NameIterator::get ();
	}

	const char * pos () const
	{
		return NameIterator::pos ();
	}

	// Forward iterator requirements
	reference operator* () const
	{
		return get ();
	}
	NameReverseIterator & operator++ ()
	{
		current = findPrevious ();
		return *this;
	}
	NameReverseIterator operator++ (int)
	{
		NameReverseIterator ret (begin, end, current);
		current = findPrevious ();
		return ret;
	}

	// Bidirectional iterator requirements
	NameReverseIterator & operator-- ()
	{
		current = findNext ();
		return *this;
	}
	NameReverseIterator operator-- (int)
	{
		NameReverseIterator ret (begin, end, current);
		current = findNext ();
		return ret;
	}
};


// Forward iterator requirements
inline bool operator== (const NameReverseIterator & lhs, const NameReverseIterator & rhs)
{
	return lhs.pos () == rhs.pos ();
}

inline bool operator!= (const NameReverseIterator & lhs, const NameReverseIterator & rhs)
{
	return lhs.pos () != rhs.pos ();
}


inline Key::iterator Key::begin ()
{
	return Key::iterator (*this, false);
}

inline Key::const_iterator Key::begin () const
{
	return Key::const_iterator (*this, false);
}

inline Key::iterator Key::end ()
{
	return Key::iterator (*this, true);
}

inline Key::const_iterator Key::end () const
{
	return Key::const_iterator (*this, true);
}

inline Key::reverse_iterator Key::rbegin ()
{
	return Key::reverse_iterator (*this, true);
}

inline Key::const_reverse_iterator Key::rbegin () const
{
	return Key::const_reverse_iterator (*this, true);
}

inline Key::reverse_iterator Key::rend ()
{
	return Key::reverse_iterator (*this, false);
}

inline Key::const_reverse_iterator Key::rend () const
{
	return Key::const_reverse_iterator (*this, false);
}

inline Key::const_iterator Key::cbegin () const noexcept
{
	return Key::const_iterator (*this, true);
}

inline Key::const_iterator Key::cend () const noexcept
{
	return Key::const_iterator (*this, false);
}

inline Key::const_reverse_iterator Key::crbegin () const noexcept
{
	return Key::const_reverse_iterator (*this, true);
}

inline Key::const_reverse_iterator Key::crend () const noexcept
{
	return Key::const_reverse_iterator (*this, false);
}
#endif // ELEKTRA_WITHOUT_ITERATOR


/**
 * Constructs a key with the name `/`.
 *
 * @note That this is not a null key, so the key will
 * evaluate to true.
 *
 * @see isValid(), isNull()
 */
inline Key::Key () : key (ckdb::keyNew ("/", KEY_END))
{
	operator++ ();
}

/**
 * Constructs a key out of a C key.
 *
 * @note If you pass a null pointer here, the key will
 * evaluate to false.
 *
 * @param k the key to work with
 *
 * @see isValid(), isNull()
 */
inline Key::Key (ckdb::Key * k) : key (k)
{
	operator++ ();
}

/**
 * Takes a reference of another key.
 *
 * The key will not be copied, but the reference
 * counter will be increased.
 *
 * @param k the key to work with
 */
inline Key::Key (Key & k) : key (k.key)
{
	operator++ ();
}

/**
 * Takes a reference of another key.
 *
 * The key will not be copied, but the reference
 * counter will be increased.
 *
 * @param k the key to work with
 */
inline Key::Key (Key const & k) : key (k.key)
{
	operator++ ();
}

/**
 * @copydoc keyNew
 *
 * @throw KeyInvalidName if key could not be constructed
 *
 * @param keyName the name of the new key
 */
inline Key::Key (const char * keyName, ...)
{
	va_list ap;

	va_start (ap, keyName);
	key = ckdb::keyVNew (keyName, ap);
	va_end (ap);

	if (!key) throw KeyInvalidName (keyName, "A key name consists of [<namespace>:]/<path>.");

	operator++ ();
}

/**
 * @copydoc keyNew
 *
 * @throw KeyInvalidName if key could not be constructed
 *
 * @warning Not supported on some compilers, e.g.
 * clang which requires you to only pass non-POD
 * in varg lists.
 *
 * @param keyName the name of the new key
 */
inline Key::Key (const std::string keyName, ...)
{
	va_list ap;

	va_start (ap, keyName);
	key = ckdb::keyVNew (keyName.c_str (), ap);
	va_end (ap);

	if (!key) throw KeyInvalidName (keyName, "A key name consists of [<namespace>:]/<path>.");

	operator++ ();
}

/**
 * @copydoc keyNew
 *
 * @throw KeyInvalidName if key could not be constructed
 *
 * @param keyName the name of the new key
 * @param ap the variable argument list pointer
 */
inline Key::Key (const char * keyName, va_list ap)
{
	key = ckdb::keyVNew (keyName, ap);

	if (!key) throw KeyInvalidName (keyName, "A key name consists of [<namespace>:]/<path>.");

	operator++ ();
}

/**
 * @copydoc keyIncRef
 */
void Key::operator++ (int) const
{
	operator++ ();
}

/**
 * @copydoc keyIncRef
 */
void Key::operator++ () const
{
	if (ckdb::keyIncRef (key) == UINT16_MAX)
	{
		/* TODO: if activated, nullptr can't be assigned to a Key-object
		 * which breaks some existing tests */
		/* throw KeyException (); */
	}
}

/**
 * @copydoc keyDecRef
 */
void Key::operator-- (int) const
{
	operator-- ();
}

/**
 * @copydoc keyDecRef
 */
void Key::operator-- () const
{
	if (ckdb::keyDecRef (key) == UINT16_MAX)
	{
		throw KeyException ();
	}
}

/**
 * @copydoc keyGetRef
 */
inline uint16_t Key::getReferenceCounter () const
{
	uint16_t result = ckdb::keyGetRef (key);
	if (result == UINT16_MAX)
	{
		throw KeyException ();
	}
	else
	{
		return result;
	}
}

/**
 * Assign a C key.
 *
 * Will call del() on the old key.
 */
inline Key & Key::operator= (ckdb::Key * k)
{
	if (key != k)
	{
		del ();
		key = k;
		operator++ ();
	}
	return *this;
}

/**
 * Assign a key.
 *
 * Will call del() on the old key.
 */
inline Key & Key::operator= (const Key & k)
{
	if (this != &k)
	{
		del ();
		key = k.key;
		operator++ ();
	}
	return *this;
}

/**
 * @copydoc keyCopy
 */
inline void Key::copy (const Key & other, elektraCopyFlags flags)
{
	if (ckdb::keyCopy (key, other.key, flags) == nullptr)
	{
		throw KeyException ();
	}
}

/**
 * Clears/Invalidates a key.
 *
 * Afterwards the object is empty again.
 *
 * @note This is not a null key, so it will
 * evaluate to true.
 * isValid() will, however, be false.
 *
 * @see release()
 * @see isValid(), isNull()
 *
 * @copydoc keyClear
 */
inline void Key::clear ()
{
	if (ckdb::keyClear (key) == -1)
	{
		throw KeyException ();
	}
}

/**
 * Passes out the raw key pointer.
 *
 * This pointer can be used to directly change the underlying key
 * object.
 *
 * \note that the ownership remains in the object
 */
ckdb::Key * Key::getKey () const
{
	return key;
}

/**
 * Is an abbreviation for getKey.
 *
 * @copydoc getKey
 *
 * @see getKey()
 */
ckdb::Key * Key::operator* () const
{
	return key;
}

/**
 * @returns a pointer to this object
 *
 * Needed for KeySet iterators.
 * @see KeySetIterator
 */
Key * Key::operator->()
{
	return this;
}

/**
 * Passes out the raw key pointer and resets internal key handle.
 *
 * \note that the ownership is moved outside.
 *
 * @retval 0 if no key is held (null pointer), no action is done then.
 */
ckdb::Key * Key::release ()
{
	ckdb::Key * ret = key;
	if (key)
	{
		operator-- ();
		key = nullptr;
	}
	return ret;
}

/**
 * @copydoc keyDup
 */
ckdb::Key * Key::dup (elektraCopyFlags flags) const
{
	return ckdb::keyDup (getKey (), flags);
}

/**
 * Destructs the key.
 *
 * @copydoc del()
 *
 * @see del()
 */
inline Key::~Key ()
{
	if (key)
	{
		del ();
	}
}

/**
 * @copydoc keyName
 *
 * @throw KeyException if key is null
 *
 * @note unlike in the C version, it is safe to change the returned
 * string.
 */
inline std::string Key::getName () const
{
	if (!key)
	{
		throw KeyException ();
	}
	return std::string (ckdb::keyName (key));
}

/**
 * @copydoc keyGetNameSize
 */
inline ssize_t Key::getNameSize () const
{
	return ckdb::keyGetNameSize (getKey ());
}


/**
 * @copydoc keyGetBaseNameSize
 */
inline ssize_t Key::getBaseNameSize () const
{
	return ckdb::keyGetBaseNameSize (getKey ());
}

/**
 * @copydoc keyBaseName
 */
inline std::string Key::getBaseName () const
{
	return std::string (ckdb::keyBaseName (key));
}

/**
 * @copydoc keySetName
 *
 * @throw KeyInvalidName if the name is not valid
 * */
inline void Key::setName (const std::string & newName)
{
	if (ckdb::keySetName (getKey (), newName.c_str ()) == -1)
	{
		throw KeyInvalidName (newName, "");
	}
}

inline void Key::addName (const std::string & addedName)
{
	if (ckdb::keyAddName (getKey (), addedName.c_str ()) == -1)
	{
		throw KeyInvalidName (addedName, "(added to end)");
	}
}

/**Sets a base name for a key.
 *
 * @copydoc keySetBaseName
 *
 * @throw KeyInvalidName if the name is not valid
 */
inline void Key::setBaseName (const std::string & baseName)
{
	if (ckdb::keySetBaseName (getKey (), baseName.c_str ()) == -1)
	{
		throw KeyInvalidName (baseName, "(as base name)");
	}
}

/** Adds a base name for a key
 *
 * @copydoc keyAddBaseName
 *
 * @throw KeyInvalidName if the name is not valid
 */
inline void Key::addBaseName (const std::string & baseName)
{
	if (ckdb::keyAddBaseName (getKey (), baseName.c_str ()) == -1)
	{
		throw KeyInvalidName (baseName, "(as base name)");
	}
}

/** Delete the baseName of a key.
 *
 * @throw KeyInvalidName if the name is not valid
 */
inline void Key::delBaseName ()
{
	if (ckdb::keySetBaseName (getKey (), 0) == -1)
	{
		throw KeyInvalidName ("", "(removing base name)");
	}
}

/**
 * @copydoc keyCmp
 *
 * @retval true == 0
 */
inline bool Key::operator== (const Key & k) const
{
	return ckdb::keyCmp (key, k.key) == 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true != 0
 */
inline bool Key::operator!= (const Key & k) const
{
	return ckdb::keyCmp (key, k.key) != 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true < 0
 */
inline bool Key::operator<(const Key & other) const
{
	return ckdb::keyCmp (key, other.key) < 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true <= 0
 */
inline bool Key::operator<= (const Key & other) const
{
	return ckdb::keyCmp (key, other.key) <= 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true > 0
 */
inline bool Key::operator> (const Key & other) const
{
	return ckdb::keyCmp (key, other.key) > 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true >= 0
 */
inline bool Key::operator>= (const Key & other) const
{
	return ckdb::keyCmp (key, other.key) >= 0;
}


/**
 * This is for loops and lookups only.
 *
 * Opposite of isNull()
 *
 * For loops it checks if there are still more keys.
 * For lookups it checks if a key could be found.
 *
 * @warning you should not construct or use null keys
 *
 * @see isNull(), isValid()
 * @return false on null keys
 * @return true otherwise
 */
inline Key::operator bool () const
{
	return !isNull ();
}

/**
 * @brief Checks if C++ wrapper has an underlying key
 *
 * @see operator bool(), isValid()
 * @return true if no underlying key exists
 */
inline bool Key::isNull () const
{
	return key == nullptr;
}

/**
 * @copydoc keyNeedSync
 */
inline bool Key::needSync () const
{
	return ckdb::keyNeedSync (key);
}

/**
 * Get a key value.
 *
 * You can write your own template specialication, e.g.:
 * @code
template <>
inline QColor Key::get() const
{
	if (getStringSize() < 1)
	{
		throw KeyTypeConversion();
	}

	std::string str = getString();
	QColor c(str.c_str());
	return c;
}
 * @endcode
 *
 * @copydoc getString
 *
 * This method tries to serialise the string to the given type.
 */
template <class T>
inline T Key::get () const
{
	std::string str = getString ();
	std::istringstream ist (str);
	ist.imbue (std::locale ("C"));
	T x;
	ist >> x; // convert string to type
	if (ist.fail () || !ist.eof ())
	{
		throw KeyTypeConversion ();
	}
	return x;
}


template <>
inline std::string Key::get () const
{
	return getString ();
}

/**
 * Set a key value.
 *
 * @copydoc setString
 *
 * This method tries to deserialise the string to the given type.
 */
template <class T>
inline void Key::set (T x)
{
	std::ostringstream ost;
	ost.imbue (std::locale ("C"));
	ost << x; // convert type to string
	if (ost.fail ())
	{
		throw KeyTypeConversion ();
	}
	setString (ost.str ());
}


/**
 * @return the string directly from the key.
 *
 * It should be the same as get().
 * @return empty string on null pointers
 *
 * @throw KeyException on null key or not a valid size
 * @throw KeyTypeMismatch if key holds binary data and not a string
 *
 * @note unlike in the C version, it is safe to change the returned
 * string.
 *
 * @see isString(), getBinary()
 */
inline std::string Key::getString () const
{
	ssize_t csize = getStringSize ();
	if (csize == -1)
	{
		throw KeyException ();
	}

	if (csize == 0)
	{
		return "";
	}

	std::string str (static_cast<size_t> (csize - 1), '\0');
	if (ckdb::keyGetString (getKey (), &str[0], static_cast<size_t> (csize)) == -1)
	{
		throw KeyTypeMismatch ();
	}
	return str;
}

/**
 * @copydoc keyGetValueSize()
 */
inline ssize_t Key::getStringSize () const
{
	return ckdb::keyGetValueSize (key);
}

/**
 * Elektra can store function pointers as binary.
 * This function returns such a function pointer.
 *
 * @throw KeyTypeMismatch if no binary data found, or binary data has not correct length
 *
 * @return a function pointer stored with setBinary()
 */
inline Key::func_t Key::getFunc () const
{
	union
	{
		Key::func_t f;
		void * v;
	} conversation;
	static_assert (sizeof (conversation) == sizeof (func_t), "union does not have size of function pointer");

	if (ckdb::keyGetBinary (getKey (), &conversation.v, sizeof (conversation)) != sizeof (conversation)) throw KeyTypeMismatch ();

	return conversation.f;
}


inline void Key::setCallback (callback_t fct)
{
	union
	{
		callback_t f;
		void * v;
	} conversation;
	static_assert (sizeof (conversation) == sizeof (callback_t), "union does not have size of function pointer");

	conversation.f = fct;
	if (ckdb::keySetBinary (getKey (), &conversation.v, sizeof (conversation)) == -1)
	{
		throw KeyException ();
	}
	if (ckdb::keySetMeta (getKey (), "callback", "") == -1)
	{
		throw KeyException ();
	}
}


/**
 * @copydoc keySetString
 */
inline void Key::setString (const char * newString)
{
	if (ckdb::keySetString (getKey (), newString) == -1)
	{
		throw KeyException ();
	}
}

inline void Key::setString (const std::string & newString)
{
	setString (newString.c_str ());
}

/**
 * @copydoc keyValue
 *
 * @return the value of the key
 * @see getBinary()
 */
inline const void * Key::getValue () const
{
	return ckdb::keyValue (getKey ());
}

/**
 * @returns the binary Value of the key.
 *
 * @retval "" on null pointers (size == 0) and on data only containing \\0
 *
 * @note if you need to distinguish between null pointers and data
 * containing \\0 you can use getValue().
 *
 * @throw KeyException on invalid binary size
 * @throw KeyTypeMismatch if key is string and not a binary
 *
 * @copydoc keyGetBinary
 *
 * @see isBinary(), getString(), getValue()
 **/
inline std::string Key::getBinary () const
{
	ssize_t csize = getBinarySize ();
	if (csize == -1)
	{
		throw KeyException ();
	}

	if (csize == 0)
	{
		return "";
	}

	std::string str (static_cast<size_t> (csize), '\0');
	if (ckdb::keyGetBinary (getKey (), &str[0], static_cast<size_t> (csize)) == -1)
	{
		throw KeyTypeMismatch ();
	}
	return str;
}

/**
 * @copydoc keyGetValueSize()
 */
inline ssize_t Key::getBinarySize () const
{
	return ckdb::keyGetValueSize (key);
}

/**
 * @copydoc keySetBinary
 */
inline ssize_t Key::setBinary (const void * newBinary, size_t dataSize)
{
	return ckdb::keySetBinary (getKey (), newBinary, dataSize);
}


/**
 * @copydoc keyGetMeta
 *
 * You can specify your own template specialisation:
 * @code
template<>
inline yourtype Key::getMeta(const std::string &name) const
{
	yourtype x;
	std::string str;
	str = std::string(
		static_cast<const char*>(
			ckdb::keyValue(
				ckdb::keyGetMeta(key, name.c_str())
				)
			)
		);
	return yourconversion(str);
}
 * @endcode
 *
 * @throw KeyTypeConversion if metadata could not be parsed
 *
 * @note No exception will be thrown if a const Key or char* is requested,
 * but don't forget the const: getMeta<const Key>,
 * otherwise you will get an compiler error.
 *
 * If no meta is available:
 * - char* is null (evaluates to 0)
 * - const Key is null (evaluate to false)
 * - otherwise the default constructed type will be returned
 * @see hasMeta
 *
 * @see delMeta(), setMeta(), copyMeta(), copyAllMeta()
 */
template <class T>
inline T Key::getMeta (const std::string & metaName) const
{
	Key k (const_cast<ckdb::Key *> (ckdb::keyGetMeta (key, metaName.c_str ())));
	if (!k)
	{
		return T ();
	}
	return k.get<T> ();
}


/**
 * @retval true if there is a metadata with given name
 * @retval false if no such metadata exists
 *
 *@see getMeta()
 */
inline bool Key::hasMeta (const std::string & metaName) const
{
	Key k (const_cast<ckdb::Key *> (ckdb::keyGetMeta (key, metaName.c_str ())));
	return k;
}

template <>
inline const ckdb::Key * Key::getMeta (const std::string & name) const
{
	return ckdb::keyGetMeta (key, name.c_str ());
}

template <>
inline const Key Key::getMeta (const std::string & name) const
{
	const ckdb::Key * k = ckdb::keyGetMeta (key, name.c_str ());
	return Key (const_cast<ckdb::Key *> (k));
}

template <>
inline const char * Key::getMeta (const std::string & name) const
{
	return static_cast<const char *> (ckdb::keyValue (ckdb::keyGetMeta (key, name.c_str ())));
}

template <>
inline std::string Key::getMeta (const std::string & name) const
{
	const char * v = static_cast<const char *> (ckdb::keyValue (ckdb::keyGetMeta (key, name.c_str ())));
	if (!v)
	{
		return std::string ();
	}
	std::string str;
	str = std::string (v);
	return str;
}

/**
 * Set metadata for key.
 *
 * @copydoc keySetMeta
 *
 * @warning unlike the C Interface, it is not possible to remove
 * metadata with this method.
 * k.setMeta("something", NULL) will lead to set the number 0 or to
 * something different (may depend on compiler definition of NULL).
 * See discussion in Issue
 * https://github.com/ElektraInitiative/libelektra/issues/8
 *
 * Use delMeta() to avoid these issues.
 *
 * @see delMeta(), getMeta(), copyMeta(), copyAllMeta()
 */
template <class T>
inline void Key::setMeta (const std::string & metaName, T x)
{
	Key k;
	k.set<T> (x);
	if (ckdb::keySetMeta (key, metaName.c_str (), k.getString ().c_str ()) == -1)
	{
		throw KeyException ();
	}
}

/**
 * Delete metadata for key.
 *
 * @see setMeta(), getMeta(), copyMeta(), copyAllMeta()
 */
inline void Key::delMeta (const std::string & metaName)
{
	if (ckdb::keySetMeta (key, metaName.c_str (), nullptr) == -1)
	{
		throw KeyException ();
	}
}

/**
 * @copydoc keyCopyMeta
 *
 * @see getMeta(), setMeta(), copyAllMeta()
 */
inline void Key::copyMeta (const Key & other, const std::string & metaName)
{
	if (ckdb::keyCopyMeta (key, other.key, metaName.c_str ()) == -1)
	{
		throw KeyException ();
	}
}

/**
 * @copydoc keyCopyAllMeta
 *
 * @see getMeta(), setMeta(), copyMeta()
 */
inline void Key::copyAllMeta (const Key & other)
{
	if (ckdb::keyCopyAllMeta (key, other.key) == -1)
	{
		throw KeyException ();
	}
}

/** @return if the key is valid
 *
 * An invalid key has no name.
 * The name of valid keys either start with user or system.
 *
 * @retval true if the key has a valid name
 * @retval false if the key has an invalid name
 *
 * @see getName(), isUser(), isSystem(), getNamespace()
 */
inline bool Key::isValid () const
{
	return key != nullptr;
}

/**
 * @return namespace of the key
 *
 * @see ElektraNamespace, keyGetNamespace
 */
inline ElektraNamespace Key::getNamespace () const
{
	return static_cast<ElektraNamespace> (ckdb::keyGetNamespace (key));
}

/**
 * Set the namespace of the key
 *
 * @see ElektraNamespace, keySetNamespace
 */
inline ssize_t Key::setNamespace (ElektraNamespace ns) const
{
	return ckdb::keySetNamespace (key, static_cast<elektraNamespace> (ns));
}


/**
 * Determines if the key is in cascading namespace.
 *
 * @retval true if it is a cascading key
 * @retval false otherwise
 */
inline bool Key::isCascading () const
{
	return ckdb::keyGetNamespace (getKey ()) == KEY_NS_CASCADING;
}

/**
 * Determines if the key is in spec namespace.
 *
 * @retval true if it is a spec key
 * @retval false otherwise
 */
inline bool Key::isSpec () const
{
	return ckdb::keyGetNamespace (getKey ()) == KEY_NS_SPEC;
}

/**
 * Determines if the key is in proc namespace.
 *
 * @retval true if it is a proc key
 * @retval false otherwise
 */
inline bool Key::isProc () const
{
	return ckdb::keyGetNamespace (getKey ()) == KEY_NS_PROC;
}

/**
 * Determines if the key is in dir namespace.
 *
 * @retval true if it is a dir key
 * @retval false otherwise
 */
inline bool Key::isDir () const
{
	return ckdb::keyGetNamespace (getKey ()) == KEY_NS_DIR;
}

/**
 * Determines if the key is in user namespace.
 *
 * @retval true if it is a user key
 * @retval false otherwise
 */
inline bool Key::isUser () const
{
	return ckdb::keyGetNamespace (getKey ()) == KEY_NS_USER;
}

/**
 * Determines if the key is in system namespace.
 *
 * @retval true if it is a system key
 * @retval false otherwise
 */
inline bool Key::isSystem () const
{
	return ckdb::keyGetNamespace (getKey ()) == KEY_NS_SYSTEM;
}

/**
 * @copydoc keyIsString
 */
inline bool Key::isString () const
{
	return ckdb::keyIsString (key);
}

/**
 * @copydoc keyIsBinary
 */
inline bool Key::isBinary () const
{
	return ckdb::keyIsBinary (key);
}

/**
 * @param k the other key
 * @return true if our key is below k
 *
 * @copydoc keyIsBelow
 */
inline bool Key::isBelow (const Key & k) const
{
	int ret = ckdb::keyIsBelow (k.getKey (), key);
	if (ret == -1) return false;
	return ret;
}

/**
 * @param k the other key
 * @return true if our key is below k or the same as k
 *
 * @copydoc keyIsBelowOrSame
 */
inline bool Key::isBelowOrSame (const Key & k) const
{
	int ret = ckdb::keyIsBelowOrSame (k.getKey (), key);
	if (ret == -1) return false;
	return ret;
}

/**
 * @param k the other key
 * @return true if our key is direct below k
 *
 * @copydoc keyIsDirectlyBelow
 */
inline bool Key::isDirectBelow (const Key & k) const
{
	int ret = ckdb::keyIsDirectlyBelow (k.getKey (), key);
	if (ret == -1) return false;
	return ret;
}

/**
 * @return true if the name of our key has been locked
 */
inline bool Key::isNameLocked () const
{
	return ckdb::keyIsLocked (key, KEY_LOCK_NAME) == KEY_LOCK_NAME;
}

/**
 * @return true if the value of our key has been locked
 */
inline bool Key::isValueLocked () const
{
	return ckdb::keyIsLocked (key, KEY_LOCK_VALUE) == KEY_LOCK_VALUE;
}

/**
 * @return true if the metadata of our key has been locked
 */
inline bool Key::isMetaLocked () const
{
	return ckdb::keyIsLocked (key, KEY_LOCK_META) == KEY_LOCK_META;
}

/**
 * Deallocate the key if the reference counter reached zero.
 *
 * If there are still references, the function will only
 * decrement the reference counter.
 *
 * @retval -1 if no key is held (null pointer)
 *
 * @copydoc keyDel
 */
inline int Key::del ()
{
	if (key)
	{
		operator-- ();
		return ckdb::keyDel (key);
	}
	return -1;
}


} // end of namespace kdb


namespace std
{
/**
 * @brief Support for putting Key in a hash
 */
template <>
struct hash<kdb::Key>
{
	size_t operator() (kdb::Key const & k) const
	{
		// use key name as hash value
		return std::hash<std::string> () (k.getName ());
	}
};
} // end of namespace std

#endif
