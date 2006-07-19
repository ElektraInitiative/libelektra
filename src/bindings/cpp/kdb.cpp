#include <kdb>
#include <iostream>

namespace kdb
{

/**
 * Constructs a class Kdb. 
 */
Kdb::Kdb ()
{
	ckdb::kdbOpen(&handle);
	std::cout << "open kdb" << std::endl;
}

/**
 * The destructor closes the database
 * */
Kdb::~Kdb ()
{
	ckdb::kdbClose(&handle);
	std::cout << "close kdb" << std::endl;
}

Key & Kdb::getKey (Key & k)
{
	ckdb::Key * ckey = k.getKey();
	ckdb::kdbGetKey(handle, ckey);
	k.setKey (ckey);
	return k;
}

void Kdb::setKey (Key & toSet) 
{
	ckdb::kdbSetKey(handle, toSet.getKey());
}

KeySet & Kdb::getKeys (KeySet & k, const std::string name, int options)
{
	ckdb::KeySet * ckeyset = k.getKeySet();
	ckdb::kdbGetChildKeys (handle, name.c_str(), ckeyset, options);
	k.setKeySet(ckeyset);
	return k;
}


void Kdb::setKeys (KeySet & toSet)
{
	ckdb::kdbSetKeys(handle, toSet.getKeySet());
}

} // end of namespace kdb

