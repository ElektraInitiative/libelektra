#ifndef CKDB_H
#define CKDB_H

#include <string>
#include <map>
#include <iostream>
extern "C"
{
#include <kdb.h>
}

#define Key _Key

/**ckdb is a class for very easy c++ access to elektra.
 * It trys to be fully-feature and remain lean.*/
class ckdb
{
public:
	ckdb (std::string root);
	~ckdb ();

	void write ();
	
	Key* getKey (std::string key);
	void setKey (std::string key, Key * overwrite);
	
	std::string & get (std::string key);
	void set (std::string key, std::string value);

	Key * dup(std::string source);

	uint8_t getType(std::string key);
	void setType(std::string key,uint8_t type);

	void setFlag(std::string key);
	void clearFlag(std::string key);
	int getFlag(std::string key);

	size_t getNameSize(std::string key);
	size_t getFullNameSize(std::string key);
	size_t getRootNameSize(std::string key);
	size_t getFullRootNameSize(std::string key);
	size_t getBaseNameSize(std::string key);

	std::string getName(std::string key);
	std::string getFullName(std::string key);
	std::string getRootName(std::string key);
	std::string getFullRootName(std::string key);
	std::string getBaseName(std::string key);

	size_t getCommentSize(std::string key);
	std::string getComment(std::string key);
	void setComment(std::string key, std::string comment);

	uid_t getUID(std::string key);
	void setUID(std::string key, uid_t uid);

	gid_t getGID(std::string key);
	void setGID(std::string key, gid_t gid);

	mode_t getAccess(std::string key);
	void setAccess(std::string key, mode_t mode);

	std::string getOwner(std::string key);
	void setOwner(std::string key, const std::string userDomain);

	size_t getDataSize(std::string key);

	std::string getString(std::string key);
	void setString(std::string key, std::string newString);

	size_t getBinary(std::string key, void *returnedBinary, size_t maxSize);
	size_t setBinary(std::string key, const void *newBinary, size_t dataSize);

	bool getBool (std::string key);
	void setBool (std::string key, bool b);
	
	
	char getChar (std::string key);
	void setChar (std::string key, char c);
	
	unsigned char getUnsignedChar (std::string key);
	void setUnsignedChar (std::string key, unsigned char uc);

	wchar_t getWChar_t (std::string key);
	void setWChar_t (std::string key, wchar_t wc);

	int getInt (std::string key);
	void setInt (std::string key, int i);

	unsigned int getUnsignedInt (std::string key);
	void setUnsignedInt (std::string key, unsigned int ui);

	short getShort (std::string key);
	void setShort (std::string key, short s);

	unsigned short getUnsignedShort (std::string key);
	void setUnsignedShort (std::string key, unsigned short us);

	long getLong (std::string key);
	void setLong (std::string key, long l);

	unsigned long getUnsignedLong (std::string key);
	void setUnsignedLong (std::string key, unsigned long ul);

#if defined __GNUC__ && defined __USE_GNU
	long long getLongLong (std::string key);
	void setLongLong (std::string key, long long ll);

	unsigned long long getUnsignedLongLong (std::string key);
	void setUnsignedLongLong (std::string key, unsigned long long ull);
#endif
	
	float getFloat (std::string key);
	void setFloat (std::string key, float f);
	
	double getDouble (std::string key);
	void setDouble (std::string key, double d);

	long double getLongDouble (std::string key);
	void setLongDouble (std::string key, long double ld);

	std::string getLink(std::string key);
	void setLink(std::string key, std::string target);

	time_t getMTime(std::string key);
	time_t getATime(std::string key);
	time_t getCTime(std::string key);
	
	int isSystem(std::string key);
	int isUser(std::string key);
	
	int getNamespace(std::string key);
	
	int isDir(std::string key);
	int isLink(std::string key);

	bool compare(std::string key1, std::string key2);

	size_t toStream(std::string key, FILE* stream, unsigned long options);
	// ostream!
protected:
	void read ();
private:	
	std::map <std::string, std::string> container;
	std::map <std::string, Key * > add_keys;
	bool needs_sync;
	std::string user_root;
	std::string root;
	KDBHandle handle;

	static ckdb * instance;
};

extern ckdb config;

#endif

