#ifndef ELEKTRA_KEY_HPP
#define ELEKTRA_KEY_HPP

#include <sstream>
#include <string>
#include <cstring>
#include <cstdarg>

#include <kdbexcept.hpp>

#include <kdb.h>

namespace kdb {


/**
 * @copydoc key
 *
 * Keys are refcounted and are cheap to copy or copy-construct.
 * If you really need a deep copy, you can use copy() or dup().
 * If you want to break references, use clear().
 * All other operations operate on references.
 *
 *
 * @invariant Key always has a working underlying Elektra Key
 * object. So clear() and release() reset to a new internal Key
 * object. This Key, however, might be invalid (see isValid()).
 *
 * \note that the reference counting in the keys is mutable,
 * so that const keys can be passed around by value.
 */
class Key
{
public:
	// constructors

	inline Key ();
	inline Key (ckdb::Key *k);
	inline Key (Key &k);
	inline Key (const Key &k);

	inline Key (const char * keyName, ...);
	inline Key (const std::string keyName, ...);
	inline Key (const char * keyName, va_list ap);


	// reference handling

	inline void operator ++(int) const;
	inline void operator ++() const;

	inline void operator --(int) const;
	inline void operator --() const;

	inline size_t getReferenceCounter() const;


	// basic methods


	inline Key& operator= (ckdb::Key *k);
	inline Key& operator= (const Key &k);

	inline void copy (const Key &other);
	inline void clear ();
	inline ckdb::Key* getKey () const;
	inline ckdb::Key* operator* () const;
	inline ckdb::Key* release ();
	inline ckdb::Key* dup () const;
	inline bool checkIdentity (const Key & k) const;
	inline ~Key ();


	// name manipulation

	inline std::string getName() const;
	inline size_t getNameSize() const;

	inline std::string getBaseName() const;
	inline size_t getBaseNameSize() const;
	inline std::string getDirName() const;

	inline void setName (const std::string &newName);
	inline void setBaseName (const std::string &baseName);
	inline void addBaseName (const std::string &baseName);

	inline size_t getFullNameSize() const;
	inline std::string getFullName() const;

	inline Key& operator=  (const std::string &newName);
	inline Key& operator+= (const std::string &baseName);
	inline Key& operator-= (const std::string &baseName);

	inline Key& operator=  (const char *newName);
	inline Key& operator+= (const char *baseName);
	inline Key& operator-= (const char *baseName);


	// operators

	inline bool operator ==(const Key &k) const;
	inline bool operator !=(const Key &k) const;
	inline bool operator < (const Key& other) const;
	inline bool operator <= (const Key& other) const;
	inline bool operator > (const Key& other) const;
	inline bool operator >= (const Key& other) const;

	inline operator bool() const;


	// value operations

	template <class T>
	inline T get() const;

	template <class T>
	inline void set(T x);

	inline std::string getString() const;
	inline void setString(std::string newString);
	inline size_t getStringSize() const;

	typedef void (*func_t)();

	inline func_t getFunc() const;

	inline std::string getBinary() const;
	inline size_t getBinarySize() const;
	inline size_t setBinary(const void *newBinary, size_t dataSize);


	// meta data

	template <class T>
	inline T getMeta(const std::string &metaName);

	template <class T>
	inline void setMeta(const std::string &metaName, T x);

	inline void copyMeta(const Key &other, const std::string &metaName);
	inline void copyAllMeta(const Key &other);

	inline void rewindMeta () const;
	inline const Key nextMeta ();
	inline const Key currentMeta () const;


	// Methods for Making tests

	inline bool isValid() const;
	inline bool isSystem() const;
	inline bool isUser() const;

	inline bool isString() const;
	inline bool isBinary() const;

	inline bool isInactive() const;

	inline bool isBelow(const Key &k) const;
	inline bool isBelowOrSame(const Key &k) const;
	inline bool isDirectBelow(const Key &k) const;

private:
	inline int del ();

	ckdb::Key * key; ///< holds an elektra key
};

/**
 * Constructs an empty, invalid key.
 *
 * @note That this is not a null key, so it will
 * evaluate to true.
 *
 * @see isValid(), operator bool()
 */
inline Key::Key () :key (ckdb::keyNew (0))
{
	operator++(); 
}

/**
 * Constructs a key out of a C key.
 *
 * If you pass a null pointer here, it will
 * evaluate to false.
 *
 * @see isValid(), operator bool()
 */
inline Key::Key (ckdb::Key *k) :key(k)
{
	operator++(); 
}

/**
 * Takes a reference of another key.
 */
inline Key::Key (Key &k) :key(k.key)
{
	operator++();
}

/**
 * Takes a reference of another key.
 */
inline Key::Key (const Key &k) :key(k.key)
{
	operator++();
}

/**
 * @copydoc keyNew
 */
inline Key::Key (const char * str, ...)
{
	va_list ap;

	va_start(ap, str);
	key = ckdb::keyVNew (str, ap);
	va_end(ap);

	operator++();
}

/**
 * @copydoc keyVNew
 *
 * @note Not supported on some compilers, e.g.
 * clang which require you to only pass non-POD
 * in varg lists.
 */
inline Key::Key (const std::string str, ...)
{
	va_list ap;

	va_start(ap, str);
	key = ckdb::keyVNew (str.c_str(), ap);
	va_end(ap);

	operator++();
}

/**
 * @copydoc keyVNew
 */
inline Key::Key (const char * keyName, va_list ap)
{
	key = ckdb::keyVNew (keyName, ap);

	operator++();
}

/**
 * @copydoc keyIncRef
 */
void Key::operator ++(int) const
{
	operator++();
}

/**
 * @copydoc keyIncRef
 */
void Key::operator ++() const
{
	ckdb::keyIncRef(key);
}

/**
 * @copydoc keyDecRef
 */
void Key::operator --(int) const
{
	operator--();
}

/**
 * @copydoc keyDecRef
 */
void Key::operator --() const
{
	ckdb::keyDecRef(key);
}

/**
 * @copydoc keyGetRef
 */
inline size_t Key::getReferenceCounter() const
{
	return ckdb::keyGetRef(key);
}

/**
 * Assign a C key.
 *
 * Will call del() on the old key.
 */
inline Key& Key::operator= (ckdb::Key *k)
{
	if (key != k)
	{
		del();
		key = k;
		operator++();
	}
	return *this;
}

/**
 * Assign a key.
 *
 * Will call del() on the old key.
 */
inline Key& Key::operator= (const Key &k)
{
	if (this != &k)
	{
		del();
		key = k.key;
		operator++();
	}
	return *this;
}

/**
 * @copydoc keyCopy
 */
inline void Key::copy (const Key &other)
{
	ckdb::keyCopy(key,other.key);
}

/**
 * Clears a key.
 *
 * Afterwards the object is empty again.
 *
 * @note That this is not a null key, so it will
 * evaluate to true.
 *
 * @see isValid(), operator bool()
 *
 * @copydoc keyClear
 */
inline void Key::clear ()
{
	ckdb::keyClear(key);
	key = ckdb::keyNew (0);

	operator++();
}

/**
 * Passes out the raw key pointer.
 *
 * This pointer can be used to directly change the underlying key
 * object.
 *
 * \note that the ownership remains in the object
 */
ckdb::Key* Key::getKey () const
{
	return key;
}

/**
 * @copydoc getKey
 */
ckdb::Key* Key::operator* () const
{
	return key;
}

/**
 * Passes out the raw key pointer.
 *
 * \note that the ownership is moved outside.
 *
 * The container will be initialized with a new key.
 */
ckdb::Key* Key::release ()
{
	ckdb::Key* ret = key;
	key = ckdb::keyNew(0);
	return ret;
}

/**
 * @copydoc keyDup
 */
ckdb::Key* Key::dup () const
{
	return ckdb::keyDup(getKey());
}


/**
 * Check if the underlying key object is the same.
 *
 * @see dup()
 */
inline bool Key::checkIdentity (const Key & k) const
{
	return key == k.key;
}

/**
 * Destructs the key.
 *
 * @see del
 */
inline Key::~Key ()
{
	del();
}

/**
 * @copydoc keyName
 */
inline std::string Key::getName() const
{
	return std::string (ckdb::keyName(key));
}

/**
 * @copydoc keyGetNameSize
 */
inline size_t Key::getNameSize() const
{
	return ckdb::keyGetNameSize (getKey());
}


/**
 * @copydoc keyGetBaseNameSize
 */
inline size_t Key::getBaseNameSize() const
{
	return ckdb::keyGetBaseNameSize(getKey());
}

/**
 * @copydoc keyBaseName
 */
inline std::string Key::getBaseName() const
{
	return std::string (ckdb::keyBaseName(key));
}

/**
 * @return the dir name of the key
 *
 * e.g. system/sw/dir/key
 * will return system/sw/dir
 */
inline std::string Key::getDirName() const
{
	std::string ret = ckdb::keyName(key);
	return ret.substr(0, ret.rfind('/'));
}


/**
 * @copydoc keySetName
 *
 * @throw kdb::KeyInvalidName when the name is not valid
 * */
inline void Key::setName (const std::string &newName)
{
	if (ckdb::keySetName (getKey(), newName.c_str()) == -1)
	{
		throw KeyInvalidName();
	}
}

/**Sets a base name for a key.
 *
 * @copydoc keySetBaseName
 *
 * @throw kdb::KeyInvalidName when the name is not valid
 */
inline void Key::setBaseName (const std::string &newSetBaseName)
{
	if (ckdb::keySetBaseName (getKey(), newSetBaseName.c_str()) == -1)
	{
		throw KeyInvalidName();
	}
}

/**
 * @copydoc keyAddBaseName
 *
 * @throw KeyInvalidName
 */
inline void Key::addBaseName (const std::string &newAddBaseName)
{
	if (ckdb::keyAddBaseName (getKey(), newAddBaseName.c_str()) == -1)
	{
		throw KeyInvalidName();
	}
}

/**
 * @copydoc keyGetFullNameSize
 */
inline size_t Key::getFullNameSize() const
{
	return ckdb::keyGetFullNameSize (getKey());
}

/**
 * @copydoc keyGetFullName
 */
inline std::string Key::getFullName() const
{
	ssize_t csize = getFullNameSize();
	if (csize == -1)
	{
		throw KeyException();
	}

	std::string str (csize-1, '\0');
	ckdb::keyGetFullName (getKey(), &str[0], csize);
	return str;
}

/**
 * Assign the name of a key.
 *
 * @see keySetName
 */
inline Key& Key::operator= (const std::string &newName)
{
	ckdb::keySetName(getKey(), newName.c_str());
	return *this;
}

/**
 * Add a new basename.
 *
 * @see keyAddBaseName()
 */
inline Key& Key::operator+= (const std::string &newAddBaseName)
{
	ckdb::keyAddBaseName(getKey(), newAddBaseName.c_str());
	return *this;
}

/**
 * Set a new basename.
 *
 * @see keySetBaseName()
 */
inline Key& Key::operator-= (const std::string &newSetBaseName)
{
	ckdb::keySetBaseName(getKey(), newSetBaseName.c_str());
	return *this;
}

/**
 * @copydoc Key::operator= (const std::string &newName)
 */
inline Key& Key::operator= (const char *newName)
{
	ckdb::keySetName(getKey(), newName);
	return *this;
}

/**
 * @copydoc Key::operator+= (const std::string &)
 */
inline Key& Key::operator+= (const char *newAddBaseName)
{
	ckdb::keyAddBaseName(getKey(), newAddBaseName);
	return *this;
}

/**
 * @copydoc Key::operator-= (const std::string &)
 */
inline Key& Key::operator-= (const char *newSetBaseName)
{
	ckdb::keySetBaseName(getKey(), newSetBaseName);
	return *this;
}

/**
 * @copydoc keyCmp
 *
 * @retval true == 0
 */
inline bool Key::operator ==(const Key &k) const
{
	return ckdb::keyCmp(key, k.key) == 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true != 0
 */
inline bool Key::operator !=(const Key &k) const
{
	return ckdb::keyCmp(key, k.key) != 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true < 0
 */
inline bool Key::operator < (const Key& other) const
{
	return ckdb::keyCmp(key, other.key) < 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true <= 0
 */
inline bool Key::operator <= (const Key& other) const
{
	return ckdb::keyCmp(key, other.key) <= 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true > 0
 */
inline bool Key::operator > (const Key& other) const
{
	return ckdb::keyCmp(key, other.key) > 0;
}

/**
 * @copydoc keyCmp
 *
 * @retval true >= 0
 */
inline bool Key::operator >= (const Key& other) const
{
	return ckdb::keyCmp(key, other.key) >= 0;
}


/**
 * This is for loops and lookups only.
 *
 * For loops it checks if there are still more keys.
 * For lookups it checks if a key could be found.
 *
 * @warning you should not construct or use null keys
 *
 * @return false on null keys
 * @return true otherwise
 */
inline Key::operator bool() const
{
	return key != 0;
}

/**
 * Get a key value.
 *
 * You can write your own template specialication, e.g.:
 * @code
 * @endcode
 *
 * @copydoc getString
 *
 * This method tries to serialize the string to the given type.
 */
template <class T>
inline T Key::get() const
{
	std::string str;
	str = getString();
	std::istringstream ist(str);
	T x;
	ist >> x;	// convert string to type
	return x;
}

template <>
inline std::string Key::get() const
{
	return getString();
}

template <>
inline mode_t Key::get() const
{
	std::string str;
	str = getString();
	std::istringstream ist(str);
	mode_t x;
	ist >> std::oct >> x >> std::dec;
	return x;
}

/**
 * Set a key value.
 *
 * @copydoc setString
 *
 * This method tries to deserialize the string to the given type.
 */
template <class T>
inline void Key::set(T x)
{
	std::string str;
	std::ostringstream ost;
	ost << x;	// convert type to string
	setString (ost.str());
}


/**
 * @return the string directly from the key.
 *
 * It should be the same as get().
 * @return empty string on null pointers
 *
 * @throw KeyException
 */
inline std::string Key::getString() const
{
	ssize_t csize = getStringSize();
	if (csize == -1)
	{
		throw KeyException();
	}

	std::string str (csize-1, '\0');
	if (ckdb::keyGetString (getKey(), &str[0], csize) == -1)
	{
		throw KeyTypeMismatch();
	}
	return str;
}

/**
 * @copydoc keyGetValueSize()
 */
inline size_t Key::getStringSize() const
{
	return ckdb::keyGetValueSize(key);
}

/**
 * Elektra can store function pointers as binary.
 * This function returns such a function pointer.
 *
 * @throw KeyTypeMismatch if no binary data found, or binary data has not correct length
 * @return a function pointer stored with setBinary()
 */
inline Key::func_t Key::getFunc() const
{
	union {Key::func_t f; void* v;} conversation;

	if (ckdb::keyGetBinary(getKey(),
			&conversation.v,
			sizeof(conversation)) != sizeof(conversation))
				throw KeyTypeMismatch();

	return conversation.f;
}

/**
 * @copydoc keySetString
 */
inline void Key::setString(std::string newString)
{
	ckdb::keySetString (getKey(), newString.c_str());
}

/**
 * @returns the binary Value of the key.
 * @throw KeyException on invalid binary size
 * @throw KeyTypeMismatch if key is string
 *
 * @copydoc keyGetBinary
 **/
inline std::string Key::getBinary() const
{
	ssize_t csize = getBinarySize();
	if (csize == -1)
	{
		throw KeyException();
	}

	std::string str (csize, '\0');
	if (ckdb::keyGetBinary (getKey(), &str[0], csize) == -1)
	{
		throw KeyTypeMismatch();
	}
	return str;
}

/**
 * @copydoc keyGetValueSize()
 */
inline size_t Key::getBinarySize() const
{
	return ckdb::keyGetValueSize(key);
}

/**
 * @copydoc keySetBinary
 */
inline size_t Key::setBinary(const void *newBinary, size_t dataSize)
{
	size_t s = ckdb::keySetBinary (getKey(), newBinary, dataSize);
	return s;
}


/**@note don't forget the const: getMeta<const ckdb::Key*>*/
template<>
inline const ckdb::Key* Key::getMeta(const std::string &name)
{
	return
		ckdb::keyGetMeta(key, name.c_str());
}

/**@note don't forget the const: getMeta<const kdb::Key>*/
template<>
inline const Key Key::getMeta(const std::string &name)
{
	return
		Key (
			const_cast<ckdb::Key*>(
				ckdb::keyGetMeta(key, name.c_str())
				)
			);
}

template<>
inline const char* Key::getMeta(const std::string &name)
{
	return
		static_cast<const char*>(
			ckdb::keyValue(
				ckdb::keyGetMeta(key, name.c_str())
				)
			);
}

template<>
inline std::string Key::getMeta(const std::string &name)
{
	std::string str;
	const char *v = 
		static_cast<const char*>(
			ckdb::keyValue(
				ckdb::keyGetMeta(key, name.c_str())
				)
			);
	if (!v)
	{
		throw KeyNoSuchMeta();
	}
	str = std::string(v);
	return str;
}

/**
 * @copydoc keyGetMeta
 *
 * You can specify your own template specialisation.
 * @code
template<>
inline mode_t Key::getMeta(const std::string &name)
{
	mode_t x;
	std::string str;
	str = std::string(
		static_cast<const char*>(
			ckdb::keyValue(
				ckdb::keyGetMeta(key, name.c_str())
				)
			)
		);
	std::istringstream ist(str);
	ist >> std::oct >> x;	// convert string to type
	return x;
}
 * @endcode
 *
 * @note Because mode_t is in fact an int, this would
 * also change all other int types.
 *
 * @throw KeyNoSuchMeta if no meta data found
 * @throw KeyBadMeta if meta data could not be parsed
 */
template <class T>
inline T Key::getMeta(const std::string &metaName)
{
	T x;
	std::string str;
	const char *v = 
		static_cast<const char*>(
			ckdb::keyValue(
				ckdb::keyGetMeta(key, metaName.c_str())
				)
			);
	if (!v)
	{
		throw KeyNoSuchMeta();
	}
	str = std::string(v);
	std::istringstream ist(str);
	ist >> x;	// convert string to type
	if (ist.fail())
	{
		throw KeyBadMeta();
	}
	return x;
}

/**
 * @copydoc keySetMeta
 */
template <class T>
inline void Key::setMeta(const std::string &metaName, T x)
{
	std::string str;
	std::ostringstream ost;
	ost << x;	// convert type to string
	ckdb::keySetMeta(key, metaName.c_str(), ost.str().c_str());
}

/**
 * @copydoc keyCopyMeta
 */
inline void Key::copyMeta(const Key &other, const std::string &metaName)
{
	ckdb::keyCopyMeta(key, other.key, metaName.c_str());
}

/**
 * @copydoc keyCopyAllMeta
 */
inline void Key::copyAllMeta(const Key &other)
{
	ckdb::keyCopyAllMeta(key, other.key);
}

/**
 * @copydoc keyRewindMeta
 */
inline void Key::rewindMeta() const
{
	ckdb::keyRewindMeta(key);
}

/**
 * @copydoc keyNextMeta
 */
inline const Key Key::nextMeta()
{
	const ckdb::Key *k = ckdb::keyNextMeta(key);
	return Key(const_cast<ckdb::Key*>(k));
}


/**
 * @copydoc keyCurrentMeta
 */
inline const Key Key::currentMeta() const
{
	return Key(
		const_cast<ckdb::Key*>(
			ckdb::keyCurrentMeta(const_cast<const ckdb::Key*>(
				key)
				)
			)
		);
}


/** @return if the key is valid
 *
 * An invalid key has no name.
 * The name of valid keys either start with user or system.
 *
 * @retval true if the key has a valid name
 * @retval false if the key has an invalid name
 *
 * @see getName(), isUser(), isSystem()
 */
inline bool Key::isValid() const
{
	return ckdb::keyGetNameSize(getKey()) > 1;
}


/**
 * Name starts with "system".
 *
 * @retval true if it is a system key
 * @retval false otherwise
 */
inline bool Key::isSystem() const
{
	return !strncmp(ckdb::keyName(key), "system", 6);
}

/**
 * Name starts with "user".
 *
 * @retval true if it is a user key
 * @retval false otherwise
 */
inline bool Key::isUser() const
{
	return !strncmp(ckdb::keyName(key), "user", 4);
}

/**
 * @copydoc keyIsString
 */
inline bool Key::isString() const
{
	return ckdb::keyIsString(key);
}

/**
 * @copydoc keyIsBinary
 */
inline bool Key::isBinary() const
{
	return ckdb::keyIsBinary(key);
}

/**
 * @copydoc keyIsInactive
 */
inline bool Key::isInactive () const
{
	return ckdb::keyIsInactive (key);
}

/**
 * @copydoc keyIsBelow
 */
inline bool Key::isBelow(const Key & k) const
{
	return ckdb::keyIsBelow(key, k.getKey());
}

/**
 * @copydoc keyIsBelowOrSame
 */
inline bool Key::isBelowOrSame(const Key & k) const
{
	return ckdb::keyIsBelowOrSame(key, k.getKey());
}

/**
 * @copydoc keyIsDirectBelow
 */
inline bool Key::isDirectBelow(const Key & k) const
{
	return ckdb::keyIsDirectBelow(key, k.getKey());
}

/**
 * Deallocate the key
 *
 * @copydoc keyDel
 */
inline int Key::del ()
{
	operator --();
	return ckdb::keyDel(key);
}


} // end of namespace kdb

#endif

