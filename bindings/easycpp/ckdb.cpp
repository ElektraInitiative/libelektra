#include <ckdb.h>

//TODO: increase Buffer Size
#define BUFFER_SIZE 5

/**Constructs a class ckdb. 
 * Read reads all values of root and stores them into a hash.
 * User and System focus is done automatically.
 * If there is no user-value is available, the system value is
 * taken.
 * The root is only the part without user or system. E.g. sw/test
 * will collect all keys/values pairs from system/sw/test and
 * user/sw/test.*/
ckdb::ckdb (std::string root /// Root of app
		)
{	
       	char keyName [BUFFER_SIZE + 1];
        char value   [BUFFER_SIZE + 1];
	std::string sname, svalue, app_root;
    Key *current;
	Key *k;
	size_t csize;
	char * field;
	KeySet * user_keys;
	
	user_root = "user/";
	user_root += root;
	needs_sync = false;
	container.clear();

	for (int i=0; i< 2; i++)
	{
		if (i) app_root = "user/";
		else app_root = "system/";
		app_root += root;

		// Initializise
		user_keys = new KeySet;
		ksInit(user_keys);
		kdbOpen();


		// Get all value keys for this application
		if (kdbGetChildKeys(app_root.c_str(), user_keys, KDB_O_RECURSIVE))
			cerr << "Could not get Configuration for " << app_root << strerror(errno) << endl;

		// Destruct
		kdbClose();
		
		for (current=user_keys->start; current; current=current->next) 
		{
			csize = keyGetBaseNameSize (current);
			if (csize < BUFFER_SIZE)
			{
				keyGetBaseName(current,keyName,sizeof(keyName));
				sname = keyName;
			} else {
				field = new char [csize];
				keyGetBaseName(current, field, csize);
				sname = field;
				delete (field);
			}
			
			csize = keyGetDataSize (current);
			if (csize < BUFFER_SIZE)
			{
				keyGetString(current,value,sizeof(value)); // fast Method
				svalue = value;
			} else {
				field = new char [csize]; // slow Method
				keyGetString(current, field, csize);
				svalue = field;
				delete (field);
			}
				
			container [sname] = svalue;
			if (i)
			{
				k = new Key;
				keyInit (k);
				keyDup (current, k);
				add_keys [sname] = k;
			}
		}
		ksClose(user_keys);	
		delete (user_keys);
	}
}

/**The destructor automatically commit a write.*/
ckdb::~ckdb ()
{
	write();
}

/**Writes all commits to the keydatabase. The same root is taken
 * as in read(std::string). The key/value pairs are stored in
 * user/, so no root privilegies are required.*/
void ckdb::write ()
{	
	Key * k;
	if (! needs_sync) return;
	needs_sync = false;

    kdbOpen();
	typedef std::map <string, Key*> :: const_iterator CI;
	for (CI p = add_keys.begin(); p!=add_keys.end(); ++p)
	{
		k = p->second;
		if (kdbSetKey (k))
			cerr << "Storing Key failed" <<  strerror (errno) <<  endl;
		keyClose (k);
		delete k;
	}
	kdbClose();
	add_keys.clear();
}

/**Returns the pointer to a specific key. This method is mostly
	used internal. Changes in the key will directly affect the
	database. You must not delete or close the key!*/
/*inline*/ Key * ckdb::getKey (std::string key)
{
	Key * k;
	needs_sync = true;
	if (add_keys.find(key) != add_keys.end())	// does element exists?
	{
		k = add_keys [key];	// get old key
	} else {		
		k = new Key;  	// generate new key
		add_keys[key] = k;
		keyInit (k);
		keySetName (k, (user_root + "/" + key).c_str());
	}
	return k;
}

/**Overwrites a key with another key. If the key has already
	been in the KeySet, the old Key will be deleted.*/
void ckdb::setKey (std::string key, Key * overwrite)
{
	if (add_keys.find(key) != add_keys.end())	// does element exists?
	{
		Key * k;
		k = add_keys [key];  	// generate new key
		keyClose (k);
		delete k;
	}
	add_keys[key] = overwrite;
}

/**Get the value from a specific key. This access is very fast, because it only
 * returns the refernce from a hash.*/
/*inline*/ std::string & ckdb::get (std::string key)
{
	return container [key];
}

/**Sets a key with a specific value.*/
/*inline*/ void ckdb::set (std::string key, std::string value)
{
	Key * k = getKey (key);
	keySetString (k, value.c_str());
	container [key] = value;
}

/**Returns a key completely independent from the internal Keydatabase.
 * You have to keyClose(Key *) and free() the Key yourself!*/
/*inline*/ Key* ckdb::dup(std::string key)
{
	Key * n = new Key;
	keyDup(getKey (key),n);
	return n;
}

/**Returns the type of the Key*/
/*inline*/ u_int8_t ckdb::getType(std::string key)
{
	return keyGetType (getKey (key));
}

/**Sets the type of the Key*/
/*inline*/ void ckdb::setType(std::string key,u_int8_t type)
{
	keySetType (getKey (key), type);
}

/**Sets a user specific flag to the key*/
/*inline*/ void ckdb::setFlag(std::string key)
{
	keySetFlag (getKey (key));
}

/**Clears a user specific flag from the key*/
/*inline*/ void ckdb::clearFlag(std::string key)
{
	keyClearFlag (getKey (key));
}

/**Returns a user specific flag from the key*/
/*inline*/ int ckdb::getFlag(std::string key)
{
	return keyGetFlag (getKey (key));
}

/**Returns the size of the Name*/
/*inline*/ size_t ckdb::getNameSize(std::string key)
{
	return keyGetNameSize (getKey (key));
}

/**Returns the size of the full Name. It includes
 * the user name (user:<name>/full/path). It is
 * always the current user.*/
/*inline*/ size_t ckdb::getFullNameSize(std::string key)
{
	return keyGetFullNameSize (getKey (key));
}

/**Returns the size of the RootName. The root name
 * is always user*/
/*inline*/ size_t ckdb::getRootNameSize(std::string key)
{
	return keyGetRootNameSize(getKey(key));
}

/**Returns the size of the FullRootName. The full
 * root name is user:<username>*/
/*inline*/ size_t ckdb::getFullRootNameSize(std::string key)
{
	return keyGetFullRootNameSize(getKey(key));
}

/**Returns the size of the BaseName.
 * The Basename is the last part of the Name.
 * E.g. user/full/root/name is name. Actually
 * it will return exactly the size of key.*/
/*inline*/ size_t ckdb::getBaseNameSize(std::string key)
{
	return keyGetBaseNameSize(getKey(key));
}

/**Returns the Name of the Key.
 * This will be user/$root/$key, where $root
 * is the root given to the constructor and
 * $key is the Parameter key*/
std::string ckdb::getName(std::string key)
{
	Key * k = getKey(key);
	size_t csize = keyGetNameSize (k);
	std::string str;	
	char * field = new char [csize];

	keyGetName (k, field, csize);
	str = field;
	delete (field);
	return str;
}

/**Returns the full Name of the key.
 * This will be user:domain/$root/$key, where
 * $root is the root given to the constructor
 * and $key is the Parameter key.*/
std::string ckdb::getFullName(std::string key)
{
	Key * k = getKey(key);
	size_t csize = keyGetFullNameSize (k);
	std::string str;	
	char * field = new char [csize];

	keyGetFullName (k, field, csize);
	str = field;
	delete (field);
	return str;
}

/**Returns user*/
std::string ckdb::getRootName(std::string key)
{
	Key * k = getKey(key);
	size_t csize = keyGetRootNameSize (k);
	std::string str;	
	char * field = new char [csize];

	keyGetRootName (k, field, csize);
	str = field;
	delete (field);
	return str;
}

/**Returns user:domain*/
std::string ckdb::getFullRootName(std::string key)
{
	Key * k = getKey(key);
	size_t csize = keyGetFullRootNameSize (k);
	std::string str;	
	char * field = new char [csize];

	keyGetFullRootName (k, field, csize);
	str = field;
	delete (field);
	return str;
}

/**Returns key*/
std::string ckdb::getBaseName(std::string key)
{
	Key * k = getKey(key);
	size_t csize = keyGetBaseNameSize (k);
	std::string str;	
	char * field = new char [csize];

	keyGetBaseName (k, field, csize);
	str = field;
	delete (field);
	return str;
}

/**Returns the size of the comment*/
/*inline*/ size_t ckdb::getCommentSize(std::string key)
{
	return keyGetCommentSize (getKey(key));
}

/**Returns the comment for the key.*/
std::string ckdb::getComment(std::string key)
{
	Key * k = getKey(key);
	size_t csize = keyGetCommentSize (k);
	std::string str;	
	char * field = new char [csize];

	keyGetComment (k, field, csize);
	str = field;
	delete (field);
	return str;
}

/**Sets a comment for the specified key.*/
/*inline*/ void ckdb::setComment(std::string key, std::string comment)
{
	keySetComment (getKey (key), comment.c_str());
}

/**Returns the UID of the the key. It always
 * returs the current UID*/
/*inline*/ uid_t ckdb::getUID(std::string key)
{
	return keyGetUID (getKey (key));
}

/**Sets another UID for a key. This will always
 * fail, because you are a user*/
/*inline*/ void ckdb::setUID(std::string key, uid_t uid)
{
	keySetUID (getKey (key), uid);
}

/**Gets the Groupid from a specific key*/
/*inline*/ gid_t ckdb::getGID(std::string key)
{
	return keyGetGID (getKey (key));
}

/**Sets the Groupid for a specific key. Only
 * groups where the user is a member a valid*/
/*inline*/ void ckdb::setGID(std::string key, gid_t gid)
{
	keySetGID (getKey (key), gid);
}

/**Returns the Accessmode of a key.
 * It is based on 4 Octets: special, user, group and other
 * The first bit is execute (not used for kdb)
 * The second bit is write access.
 * The third bit is read access.
 * See more in chmod (2).
 * An easier description may be in chmod (1), but
 * the +-rwx method is not supported.*/
/*inline*/ mode_t ckdb::getAccess(std::string key)
{
	return keyGetAccess(getKey(key));
}

/**Sets the Accessmode of a key. For more info see
 * getAccess (std::string ).*/
/*inline*/ void ckdb::setAccess(std::string key, mode_t mode)
{
	keySetAccess (getKey(key),mode);
}

/**Returns the Owner of the Key. It will always return
 * the current user.*/
std::string ckdb::getOwner(std::string key)
{
	Key * k = getKey(key);
	size_t csize =  keyGetOwnerSize (k);
	std::string str;	
	char * field = new char [csize];

	keyGetOwner (k, field, csize);
	str = field;
	delete (field);
	return str;
}

/**Sets the Owner of the Key. It will fail, because
 * you are not root.*/
/*inline*/ void ckdb::setOwner(std::string key, std::string userDomain)
{
	keySetOwner(getKey(key), userDomain.c_str());
}

/**Returns the DataSize of the Binary or String. It is used
 * for the internal malloc().*/
/*inline*/ size_t ckdb::getDataSize(std::string key)
{
	return keyGetDataSize (getKey(key));
}

/**Returns the string directly from the key. It should be
 * the same as get().*/
/*inline*/ std::string ckdb::getString(std::string key)
{
	Key * k = getKey(key);
	size_t csize =  keyGetDataSize (k);
	std::string str;	
	char * field = new char [csize];

	keyGetString (k, field, csize);
	str = field;
	delete (field);
	return str;
}

/**Sets the String of a key.*/
/*inline*/ void ckdb::setString(std::string key, std::string newString)
{
	keySetString (getKey (key), newString.c_str());
	container [key] = newString;
}

/**Returns the binary Value of the key. It will not be encoded
 * or decoded.*/
/*inline*/ size_t ckdb::getBinary(std::string key, void *returnedBinary, size_t maxSize)
{
	return keyGetBinary (getKey (key), returnedBinary, maxSize);
}

/**Sets a binary Value of a key*/
/*inline*/ size_t ckdb::setBinary(std::string key, const void *newBinary, size_t dataSize)
{	
	size_t s = keySetBinary (getKey (key), newBinary, dataSize);
	container [key] = (char *)newBinary;
	return s;
}


/*inline*/ bool ckdb::getBool (std::string key)
{
	bool b;
	getBinary (key, &b, sizeof (bool));
	return b;
}

/*inline*/ void ckdb::setBool (std::string key, bool b)
{
	setBinary (key, &b, sizeof (bool));
}


/*inline*/ char ckdb::getChar (std::string key)
{
	char b;
	getBinary (key, &b, sizeof (char));
	return b;
}

/*inline*/ void ckdb::setChar (std::string key, char c)
{
	setBinary (key, &c, sizeof (char));
}

/*inline*/ unsigned char ckdb::getUnsignedChar (std::string key)
{
	unsigned char b;
	getBinary (key, &b, sizeof (unsigned char));
	return b;
}

/*inline*/ void ckdb::setUnsignedChar (std::string key, unsigned char uc)
{
	setBinary (key, &uc, sizeof (unsigned char));
}

/*inline*/ wchar_t ckdb::getWChar_t (std::string key)
{
	wchar_t b;
	getBinary (key, &b, sizeof (wchar_t));
	return b;
}

/*inline*/ void ckdb::setWChar_t (std::string key, wchar_t wc)
{
	setBinary (key, &wc, sizeof (wchar_t));
}


/*inline*/ int ckdb::getInt (std::string key)
{
	int b;
	getBinary (key, &b, sizeof (int));
	return b;
}

/*inline*/ void ckdb::setInt (std::string key, int i)
{
	setBinary (key, &i, sizeof (int));
}

/*inline*/ unsigned int ckdb::getUnsignedInt (std::string key)
{
	unsigned int  b;
	getBinary (key, &b, sizeof (unsigned int ));
	return b;
}

/*inline*/ void ckdb::setUnsignedInt (std::string key, unsigned int ui)
{
	setBinary (key, &ui, sizeof ( unsigned int));
}

/*inline*/ short ckdb::getShort (std::string key)
{
	short b;
	getBinary (key, &b, sizeof (short));
	return b;
}

/*inline*/ void ckdb::setShort (std::string key, short s)
{
	setBinary (key, &s, sizeof (short));
}

/*inline*/ unsigned short ckdb::getUnsignedShort (std::string key)
{
	unsigned short b;
	getBinary (key, &b, sizeof (unsigned short));
	return b;
}

/*inline*/ void ckdb::setUnsignedShort (std::string key, unsigned short us)
{
	setBinary (key, &us, sizeof (unsigned short));
}

/*inline*/ long ckdb::getLong (std::string key)
{
	long b;
	getBinary (key, &b, sizeof (long));
	return b;
}

/*inline*/ void ckdb::setLong (std::string key, long l)
{
	setBinary (key, &l, sizeof (long));
}

/*inline*/ unsigned long ckdb::getUnsignedLong (std::string key)
{
	unsigned long b;
	getBinary (key, &b, sizeof (unsigned long));
	return b;
}

/*inline*/ void ckdb::setUnsignedLong (std::string key, unsigned long ul)
{
	setBinary (key, &ul, sizeof (unsigned long));
}


#if defined __GNUC__ && defined __USE_GNU
/*inline*/ long long ckdb::getLongLong (std::string key)
{
	long long b;
	getBinary (key, &b, sizeof (long long));
	return b;
}

/*inline*/ void ckdb::setLongLong (std::string key, long long ll)
{
	setBinary (key, &ll, sizeof (long long));
}

/*inline*/ unsigned long long ckdb::getUnsignedLongLong (std::string key)
{
	unsigned long long b;
	getBinary (key, &b, sizeof (unsigned long long));
	return b;
}

/*inline*/ void ckdb::setUnsignedLongLong (std::string key, unsigned long long ull)
{
	setBinary (key, &ull, sizeof (unsigned long long));
}

#endif

/*inline*/ float ckdb::getFloat (std::string key)
{
	float b;
	getBinary (key, &b, sizeof (float));
	return b;
}

/*inline*/ void ckdb::setFloat (std::string key, float f)
{
	setBinary (key, &f, sizeof (float));
}

/*inline*/ double ckdb::getDouble (std::string key)
{
	double b;
	getBinary (key, &b, sizeof (double));
	return b;
}

/*inline*/ void ckdb::setDouble (std::string key, double d)
{
	setBinary (key, &d, sizeof (double));
}

/*inline*/ long double ckdb::getLongDouble (std::string key)
{
	long double b;
	getBinary (key, &b, sizeof (long double));
	return b;
}

/*inline*/ void ckdb::setLongDouble (std::string key, long double ld)
{
	setBinary (key, &ld, sizeof (long double));
}

/**Returns the time the Key was modified*/
/*inline*/ time_t ckdb::getMTime(std::string key)
{
	return keyGetMTime(getKey(key));
}

/**Returns the last access time.*/
/*inline*/ time_t ckdb::getATime(std::string key)
{
	return keyGetATime(getKey(key));
}

/**Returns when the Key last was changed.*/
/*inline*/ time_t ckdb::getCTime(std::string key)
{
	return keyGetCTime(getKey(key));
}

/*inline*/ int ckdb::isSystem(std::string key)
{
	return keyIsSystem(getKey(key));
}

/*inline*/ int ckdb::isUser(std::string key)
{
	return keyIsUser(getKey(key));
}


/*inline*/ int ckdb::getNamespace(std::string key)
{
	return keyGetNamespace(getKey(key));
}

/*inline*/ int ckdb::isDir(std::string key)
{
	return keyIsDir(getKey(key));
}

/*inline*/ int ckdb::isLink(std::string key)
{
	return keyIsLink(getKey(key));
}


// Vergleichsoperatoren!
bool ckdb::compare(std::string key1, std::string key2)
{
	return keyCompare (getKey(key1), getKey(key2));
}

size_t ckdb::toStream(std::string key, FILE* stream, unsigned long options = KDB_O_XMLHEADERS)
{
	return keyToStream (getKey (key), stream, options);
}

