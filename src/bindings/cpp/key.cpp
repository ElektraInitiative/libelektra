#include <key>
#include <iostream>

#include <cstdarg>

namespace kdb {

Key::Key ()
{
	key = ckdb::keyNew ("user/noname");
	std::cout << "key constr no name" << std::endl;
}

Key::Key (ckdb::Key * k)
{
	// if (k == 0) std:cout << "dont do that" << std::endl;
	key = k;
	std::cout << "key constr given key" << std::endl;
}

Key::Key (const char * str, ...)
{
	va_list liste;
	
	va_start (liste, str);
	key = ckdb::keyNew (str, liste);
	va_end (liste);
	std::cout << "key constr given name" << std::endl;
}

/**The destructor automatically commit a write.*/
Key::~Key ()
{
	// Destruct
	// kdbClose(&handle);
	keyDel (key);
}


/**Returns the type of the Key*/
uint8_t Key::getType()
{
	return ckdb::keyGetType (key);
}

/**Sets the type of the Key*/
void Key::setType(uint8_t type)
{
	ckdb::keySetType (getKey(), type);
}

/**Sets a user specific flag to the key*/
void Key::setFlag()
{
	ckdb::keySetFlag (getKey());
}

/**Clears a user specific flag from the key*/
void Key::clearFlag()
{
	ckdb::keyClearFlag (getKey());
}

/**Returns a user specific flag from the key*/
int Key::getFlag()
{
	return ckdb::keyGetFlag (getKey());
}


/**Returns the Name of the Key.
 * This will be user/$root/$key, where $root
 * is the root given to the constructor and
 * $key is the Parameter key*/
std::string Key::getName()
{
	size_t csize = ckdb::keyGetNameSize (getKey());
	std::string str;
	char * field = new char [csize];

	ckdb::keyGetName (getKey(), field, csize);
	str = field;
	delete (field);
	return str;
}

/**Returns the full Name of the key.
 * This will be user:domain/$root/$key, where
 * $root is the root given to the constructor
 * and $key is the Parameter key.*/
std::string Key::getFullName()
{
	size_t csize = ckdb::keyGetFullNameSize (getKey());
	std::string str;	
	char * field = new char [csize];

	ckdb::keyGetFullName (getKey(), field, csize);
	str = field;
	delete (field);
	return str;
}

void Key::setName (const std::string & name)
{
	ckdb::keySetName (getKey(), name.c_str());
}

/**Returns the size of the comment*/
size_t Key::getCommentSize()
{
	return ckdb::keyGetCommentSize (key);
}

/**Returns the comment for the key.*/
std::string Key::getComment()
{
	size_t csize = ckdb::keyGetCommentSize (getKey());
	std::string str;	
	char * field = new char [csize];

	ckdb::keyGetComment (getKey(), field, csize);
	str = field;
	delete (field);
	return str;
}

/**Sets a comment for the specified key.*/
void Key::setComment(std::string comment)
{
	ckdb::keySetComment (getKey(), comment.c_str());
}

/**Returns the UID of the the key. It always
 * returs the current UID*/
uid_t Key::getUID()
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
gid_t Key::getGID()
{
	return ckdb::keyGetGID (getKey());
}

/**Sets the Groupid for a specific key. Only
 * groups where the user is a member a valid.*/
void Key::setGID(gid_t gid)
{
	ckdb::keySetGID (getKey(), gid);
}

/**Returns the Accessmode of a key.
 * It is based on 4 Octets: special, user, group and other
 * The first bit is execute (not used for kdb)
 * The second bit is write access.
 * The third bit is read access.
 * See more in chmod (2).
 * An easier description may be in chmod (1), but
 * the +-rwx method is not supported.*/
mode_t Key::getAccess()
{
	return ckdb::keyGetAccess(getKey());
}

/**Sets the Accessmode of a key. For more info see
 * getAccess (std::string ).*/
void Key::setAccess(mode_t mode)
{
	ckdb::keySetAccess (getKey(),mode);
}

/**Returns the Owner of the Key. It will always return
 * the current user.*/
std::string Key::getOwner()
{
	size_t csize =  ckdb::keyGetOwnerSize (getKey());
	std::string str;	
	char * field = new char [csize];

	ckdb::keyGetOwner (getKey(), field, csize);
	str = field;
	delete (field);
	return str;
}

/**Sets the Owner of the Key. It will fail, because
 * you are not root.*/
void Key::setOwner(std::string userDomain)
{
	ckdb::keySetOwner(getKey(), userDomain.c_str());
}

/**Returns the DataSize of the Binary or String. It is used
 * for the internal malloc().*/
size_t Key::getDataSize()
{
	return ckdb::keyGetDataSize (key);
}

/**Returns the string directly from the key. It should be
 * the same as get().*/
std::string Key::getString()
{
	size_t csize =  ckdb::keyGetDataSize (getKey());
	std::string str;	
	char * field = new char [csize];

	ckdb::keyGetString (getKey(), field, csize);
	str = field;
	delete (field);
	return str;
}

/**Sets the String of a key.*/
void Key::setString(std::string newString)
{
	ckdb::keySetString (getKey(), newString.c_str());
}

/**Returns the binary Value of the key. It will not be encoded
 * or decoded.*/
size_t Key::getBinary(void *returnedBinary, size_t maxSize)
{
	return ckdb::keyGetBinary (getKey(), returnedBinary, maxSize);
}

/**Sets a binary Value of a key*/
size_t Key::setBinary(const void *newBinary, size_t dataSize)
{	
	size_t s = ckdb::keySetBinary (getKey(), newBinary, dataSize);
	return s;
}


bool Key::getBool ()
{
	bool b;
	getBinary (&b, sizeof (bool));
	return b;
}

void Key::setBool (bool b)
{
	setBinary (&b, sizeof (bool));
}


char Key::getChar ()
{
	char b;
	getBinary (&b, sizeof (char));
	return b;
}

void Key::setChar (char c)
{
	setBinary (&c, sizeof (char));
}

unsigned char Key::getUnsignedChar ()
{
	unsigned char b;
	getBinary (&b, sizeof (unsigned char));
	return b;
}

void Key::setUnsignedChar (unsigned char uc)
{
	setBinary (&uc, sizeof (unsigned char));
}

wchar_t Key::getWChar_t ()
{
	wchar_t b;
	getBinary (&b, sizeof (wchar_t));
	return b;
}

void Key::setWChar_t (wchar_t wc)
{
	setBinary (&wc, sizeof (wchar_t));
}


int Key::getInt ()
{
	int b;
	getBinary (&b, sizeof (int));
	return b;
}

void Key::setInt (int i)
{
	setBinary (&i, sizeof (int));
}

unsigned int Key::getUnsignedInt ()
{
	unsigned int  b;
	getBinary (&b, sizeof (unsigned int ));
	return b;
}

void Key::setUnsignedInt (unsigned int ui)
{
	setBinary (&ui, sizeof ( unsigned int));
}

short Key::getShort ()
{
	short b;
	getBinary (&b, sizeof (short));
	return b;
}

void Key::setShort (short s)
{
	setBinary (&s, sizeof (short));
}

unsigned short Key::getUnsignedShort ()
{
	unsigned short b;
	getBinary (&b, sizeof (unsigned short));
	return b;
}

void Key::setUnsignedShort (unsigned short us)
{
	setBinary (&us, sizeof (unsigned short));
}

long Key::getLong ()
{
	long b;
	getBinary (&b, sizeof (long));
	return b;
}

void Key::setLong (long l)
{
	setBinary (&l, sizeof (long));
}

unsigned long Key::getUnsignedLong ()
{
	unsigned long b;
	getBinary (&b, sizeof (unsigned long));
	return b;
}

void Key::setUnsignedLong (unsigned long ul)
{
	setBinary (&ul, sizeof (unsigned long));
}


#if defined __GNUC__ && defined __USE_GNU
long long Key::getLongLong ()
{
	long long b;
	getBinary (&b, sizeof (long long));
	return b;
}

void Key::setLongLong (long long ll)
{
	setBinary (&ll, sizeof (long long));
}

unsigned long long Key::getUnsignedLongLong ()
{
	unsigned long long b;
	getBinary (&b, sizeof (unsigned long long));
	return b;
}

void Key::setUnsignedLongLong (unsigned long long ull)
{
	setBinary (&ull, sizeof (unsigned long long));
}

#endif

float Key::getFloat ()
{
	float b;
	getBinary (&b, sizeof (float));
	return b;
}

void Key::setFloat (float f)
{
	setBinary (&f, sizeof (float));
}

double Key::getDouble ()
{
	double b;
	getBinary (&b, sizeof (double));
	return b;
}

void Key::setDouble (double d)
{
	setBinary (&d, sizeof (double));
}

long double Key::getLongDouble ()
{
	long double b;
	getBinary (&b, sizeof (long double));
	return b;
}

void Key::setLongDouble (long double ld)
{
	setBinary (&ld, sizeof (long double));
}

/**Returns the time the Key was modified*/
time_t Key::getMTime()
{
	return ckdb::keyGetMTime(key);
}

/**Returns the last access time.*/
time_t Key::getATime()
{
	return ckdb::keyGetATime(key);
}

/**Returns when the Key last was changed.*/
time_t Key::getCTime()
{
	return ckdb::keyGetCTime(key);
}

int Key::isSystem()
{
	return ckdb::keyIsSystem(key);
}

int Key::isUser()
{
	return ckdb::keyIsUser(key);
}


int Key::getNamespace()
{
	return ckdb::keyGetNamespace(key);
}

int Key::isDir()
{
	return ckdb::keyIsDir(key);
}

int Key::isLink()
{
	return ckdb::keyIsLink(key);
}

size_t Key::toStream(FILE* stream, unsigned long options = ckdb::KDB_O_XMLHEADERS)
{
	return ckdb::keyToStream (getKey(), stream, options);
}

} // end of namespace kdb

