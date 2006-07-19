#include <keyset>

namespace kdb {

KeySet::KeySet ()
{
	ks = ckdb::ksNew ();
}

/**The destructor automatically commit a write.*/
KeySet::~KeySet ()
{
	ksDel (ks);
}

} // end of KeySet

