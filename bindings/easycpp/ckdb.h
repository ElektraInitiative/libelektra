#ifndef CKDB_H
#define CKDB_H

#include <string>
#include <map>
#include <iostream>
extern "C"
{
#include <kdb.h>
}

/**ckdb is a class for very easy c++ access to elektra.
 * It trys to be fully-feature and remain lean.*/
class ckdb
{
public:
	ckdb (string root);
	~ckdb ();

	void write ();

	string & get (string key);
	void set (string key, string value);
private:
	void read (string root);

	KeySet * user_keys;
	map <string, string> container;
	bool needs_sync;
	std::string user_root;
};

#endif

