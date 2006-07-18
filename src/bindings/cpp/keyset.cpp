#include <keyset>

namespace kdb {

KeySet::KeySet ()
{
	//key = ckdb::ckdb::keyNew ("");
}

/*Key::Key (const char * str, ...)
{}*/

/**The destructor automatically commit a write.*/
KeySet::~KeySet ()
{
	// Destruct
	// kdbClose(&handle);
}

} // end of KeySet

