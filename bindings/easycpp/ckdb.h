#ifndef CKDB_H
#define CKDB_H

#include <string>
#include <map>

/**ckdb is a class for very easy access to elektra.
 * It trys to be fully-feature and remain lean.*/
class ckdb
{
public:
	/**Initializise and receive all keys.
	 * root sets the root of the app. If the configuration
	 * should be taken from system/sw/myapp and
	 * user/sw/myapp set root to "sw/myapp".*/
	ckdb (string root);

	~ckdb ();

	/**Returns value of key*/
	string & get (string key);

	void set (string key, string value);
private:
	map <string, string> container;
	bool needs_sync;
	std::string user_root;
};

#endif

