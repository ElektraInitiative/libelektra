#include <kdb.h>

int main()
{
	Key * origKey;
	KeySet * ks = ksNew ();

	Key * key = keyNew ("user/test/name",
			KEY_SWITCH_VALUE, "myvalue",
			KEY_SWITCH_END);
        
	ksInsert (ks, key);

        ksRewind (ks);
        origKey = ksCurrent (ks); // origKey is NULL, why?
        keyDup (origKey, key);	// this crashes
}

