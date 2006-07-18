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

/*KeySet & Kdb::getKeys (KeySet & k, const std::string name)
{}

void Kdb::setKeys (const std::string name, KeySet & toSet)
{}*/

} // end of namespace kdb

