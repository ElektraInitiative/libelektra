#include <key>
#include <iostream>

#include <cstdarg>

namespace kdb {

Key::Key (const char * str, va_list ap)
{
	key = ckdb::keyVNew (str, ap);

	operator++();
}

Key::Key (const char * str, ...)
{
	va_list ap;

	va_start(ap, str);
	key = ckdb::keyVNew (str, ap);
	va_end(ap);

	operator++();
}

Key::Key (const std::string str, ...)
{
	va_list ap;

	va_start(ap, str);
	key = ckdb::keyVNew (str.c_str(), ap);
	va_end(ap);

	operator++();
}

Key& Key::operator= (ckdb::Key *k)
{
	if (key != k)
	{
		del();
		key = k;
		operator++();
	}
	return *this;
}

Key& Key::operator= (const Key &k)
{
	if (this != &k)
	{
		del();
		key = k.key;
		operator++();
	}
	return *this;
}

Key& Key::operator= (const std::string &name)
{
	ckdb::keySetName(getKey(), name.c_str());
	return *this;
}

Key& Key::operator+= (const std::string &name)
{
	ckdb::keyAddBaseName(getKey(), name.c_str());
	return *this;
}

Key& Key::operator-= (const std::string &name)
{
	ckdb::keySetBaseName(getKey(), name.c_str());
	return *this;
}

Key& Key::operator= (const char *name)
{
	ckdb::keySetName(getKey(), name);
	return *this;
}

Key& Key::operator+= (const char *name)
{
	ckdb::keyAddBaseName(getKey(), name);
	return *this;
}

Key& Key::operator-= (const char *name)
{
	ckdb::keySetBaseName(getKey(), name);
	return *this;
}

/**Returns the type of the Key*/
type_t Key::getType() const
{
	return ckdb::keyGetType (key);
}

/**Sets the type of the Key*/
void Key::setType(type_t type)
{
	ckdb::keySetType (getKey(), type);
}

size_t Key::getNameSize() const
{
	return ckdb::keyGetNameSize (getKey());
}

std::string Key::getName() const
{
	return std::string (ckdb::keyName(key));
}

const char* Key::name() const
{
	return ckdb::keyName(getKey());
}

size_t Key::getBaseNameSize() const
{
	return ckdb::keyGetBaseNameSize (getKey());
}

std::string Key::getBaseName() const
{
	return std::string (ckdb::keyBaseName(key));
}


const char* Key::baseName() const
{
	return ckdb::keyBaseName(getKey());
}


/**Sets a name for a key.
 * Throws kdb::KeyInvalidName when the name is not valid*/
void Key::setName (const std::string &name)
{
	if (ckdb::keySetName (getKey(), name.c_str()) == -1)
		throw KeyInvalidName();
}

/**Sets a base name for a key.
 * Throws kdb::KeyInvalidName when the name is not valid*/
void Key::setBaseName (const std::string &name)
{
	if (ckdb::keySetBaseName (getKey(), name.c_str()) == -1)
		throw KeyInvalidName();
}

size_t Key::getFullNameSize() const
{
	return ckdb::keyGetFullNameSize (getKey());
}

std::string Key::getFullName() const
{
	size_t csize = ckdb::keyGetFullNameSize (getKey());
	std::string str;
	char * field = new char [csize];

	ckdb::keyGetFullName (getKey(), field, csize);
	str = field;
	delete [] field;
	return str;
}

/**Returns the comment for the key.*/
std::string Key::getComment() const
{
	return std::string(ckdb::keyComment(key));
}

const char* Key::comment() const
{
	return ckdb::keyComment (key);
}

/**Returns the size of the comment*/
size_t Key::getCommentSize() const
{
	return ckdb::keyGetCommentSize (key);
}

/**Sets a comment for the specified key.*/
void Key::setComment(const std::string &comment)
{
	ckdb::keySetComment (getKey(), comment.c_str());
}

/**Returns the UID of the the key. It always
 * returs the current UID*/
uid_t Key::getUID() const
{
	return ckdb::keyGetUID (getKey());
}

/**Sets another UID for a key. This will always
 * fail, because you are a user.*/
void Key::setUID(uid_t uid)
{
	ckdb::keySetUID (getKey(), uid);
}

/**Gets the Groupid from a specific key.*/
gid_t Key::getGID() const
{
	return ckdb::keyGetGID (getKey());
}

/**Sets the Groupid for a specific key. Only
 * groups where the user is a member a valid.*/
void Key::setGID(gid_t gid)
{
	ckdb::keySetGID (getKey(), gid);
}

/**Returns the Mode of a key.
 * It is based on 4 Octets: special, user, group and other
 * The first bit is execute (not used for kdb)
 * The second bit is write mode.
 * The third bit is read mode.
 * See more in chmod (2).
 * An easier description may be in chmod (1), but
 * the +-rwx method is not supported.*/
mode_t Key::getMode() const
{
	return ckdb::keyGetMode(getKey());
}

/**Sets the Mode of a key. For more info see
 * getMode (std::string ).*/
void Key::setMode(mode_t mode)
{
	ckdb::keySetMode (getKey(),mode);
}

/**Returns the Owner of the Key. It will always return
 * the current user.*/
std::string Key::getOwner() const
{
	return std::string (ckdb::keyOwner(key));
}

const char* Key::owner() const
{
	return ckdb::keyOwner(key);
}

size_t Key::getOwnerSize() const
{
	return ckdb::keyGetOwnerSize(key);
}

/**Sets the Owner of the Key. It will fail, because
 * you are not root.*/
void Key::setOwner(const std::string &owner)
{
	ckdb::keySetOwner(getKey(), owner.c_str());
}

const void*Key::value() const
{
	return ckdb::keyValue (key);
}

/**Returns the DataSize of the Binary or String. It is used
 * for the internal malloc().*/
size_t Key::getValueSize() const
{
	return ckdb::keyGetValueSize (key);
}

/**Returns the string directly from the key. It should be
 * the same as get().*/
std::string Key::getString() const
{
	size_t csize =  ckdb::keyGetValueSize (getKey());
	std::string str;	
	char * field = new char [csize];

	ckdb::keyGetString (getKey(), field, csize);
	str = field;
	delete [] field;
	return str;
}

/**Sets the String of a key.*/
void Key::setString(std::string newString)
{
	ckdb::keySetString (getKey(), newString.c_str());
}

/**Returns the binary Value of the key. It will not be encoded
 * or decoded.*/
size_t Key::getBinary(void *returnedBinary, size_t maxSize) const
{
	return ckdb::keyGetBinary (getKey(), returnedBinary, maxSize);
}

/**Sets a binary Value of a key*/
size_t Key::setBinary(const void *newBinary, size_t dataSize)
{	
	size_t s = ckdb::keySetBinary (getKey(), newBinary, dataSize);
	return s;
}

void Key::setDir ()
{
	ckdb::keySetDir (key);
}

void Key::setMTime (time_t time)
{
	ckdb::keySetMTime (key, time);
}

void Key::setATime (time_t time)
{
	ckdb::keySetATime (key, time);
}

void Key::setCTime (time_t time)
{
	ckdb::keySetCTime (key, time);
}

/**Returns the time the Key was modified*/
time_t Key::getMTime() const
{
	return ckdb::keyGetMTime(key);
}

/**Returns the last access time.*/
time_t Key::getATime() const
{
	return ckdb::keyGetATime(key);
}

/**Returns when the Key last was changed.*/
time_t Key::getCTime() const
{
	return ckdb::keyGetCTime(key);
}

bool Key::isSystem() const
{
	return ckdb::keyIsSystem(key);
}

bool Key::isUser() const
{
	return ckdb::keyIsUser(key);
}

size_t Key::getReference() const
{
	return ckdb::keyGetRef(key);
}

bool Key::isDir() const
{
	return ckdb::keyIsDir(key);
}

bool Key::isString() const
{
	return ckdb::keyIsString(key);
}

bool Key::isBinary() const
{
	return ckdb::keyIsBinary(key);
}

bool Key::isInactive () const
{
	return ckdb::keyIsInactive (key);
}

bool Key::isBelow(const Key & k) const
{
	return ckdb::keyIsBelow(key, k.getKey());
}

bool Key::isDirectBelow(const Key & k) const
{
	return ckdb::keyIsDirectBelow(key, k.getKey());
}

bool Key::needSync() const
{
	return ckdb::keyNeedSync(key);
}

bool Key::needRemove() const
{
	return ckdb::keyNeedRemove(key);
}

bool Key::needStat() const
{
	return ckdb::keyNeedStat(key);
}

void Key::remove()
{
	ckdb::keyRemove (key);
}

void Key::stat()
{
	ckdb::keyStat (key);
}

/*
ssize_t Key::toStream(FILE* stream, unsigned long options) const
{
	return ckdb::keyToStream (getKey(), stream, options);
}

ssize_t Key::output(FILE* stream, unsigned long options) const
{
	return ckdb::keyOutput (getKey(), stream, options);
}

ssize_t Key::generate(FILE* stream, unsigned long options) const
{
	return ckdb::keyGenerate(getKey(), stream, options);
}
*/

std::ostream & operator << (std::ostream & os, const Key &k)
{
	os << "Name: " << k.getName() 
	   << " Value: " << k.getString() 
	   << " Comment: " << k.getComment()
	   << " Domain: " << (k.isUser() ? "user" : "system");
	return os;
}

std::istream & operator >> (std::istream & is, Key &k)
{
	std::string name, value, comment;
	is >> name >> value >> comment;
	k.setName(name);
	k.set<std::string>(value);
	k.setComment(comment);
	return is;
}

} // end of namespace kdb

