#ifndef CKDB_H
#define CKDB_H

#include <string>

namespace ckdb {
#include <kdb.h>
}


namespace kdb {

class Key;
class KeySet;


/**
 * ckdb provides a singleton class for c++ access to kdb.
 * */
class Kdb
{
public:
	Kdb ();
	~Kdb ();
	
	Key getKey (const std::string name);
	void setKey (const std::string key, Key & toSet);

	KeySet getKeys (const std::string name);
	void setKeys (const std::string name, KeySet & toSet);
	
	/*std::string & get (std::string key);
	void set (std::string key, std::string value);*/
	
protected:
	/**You may use the KDBHandle in an inherited class*/
	ckdb::KDBHandle handle;
};

} // end of namespace kdb

#endif

