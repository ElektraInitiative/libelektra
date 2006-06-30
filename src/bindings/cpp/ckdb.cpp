#include <ckdb.h>

namespace kdb
{

/**
 * Constructs a class Kdb. 
 */
Kdb::Kdb ()
{
	ckdb::kdbOpen(&handle);
}

/**
 * The destructor closes the database
 * */
Kdb::~Kdb ()
{
	ckdb::kdbClose(&handle);
}

} // end of namespace kdb

