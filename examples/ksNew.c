/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/old_kdb.h>
#include <stdio.h>

// clang-format off

int main (void)
{
{
//! [Simple]
KeySet * keys = ksNew (1, KS_END);
// enough memory for up to 16 keys, without needing reallocation
ksDel (keys);
//! [Simple]
}

{
//! [No Allocation]
// Create KeySet without allocating memory for keys
KeySet * keys = ksNew (0, KS_END);
// The first allocation will happen in ksAppendKey
ksAppendKey(keys, keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key02", KEY_VALUE, "value02", KEY_END));
// work with the KeySet
ksDel (keys);
//! [No Allocation]
}

{
//! [Length 15]
KeySet * keys = ksNew (15, keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key01", KEY_VALUE, "value01", KEY_END),
		       keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key02", KEY_VALUE, "value02", KEY_END),
		       keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key03", KEY_VALUE, "value03", KEY_END),
		       // ...
		       keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key15", KEY_VALUE, "value15", KEY_END), KS_END);
// work with it
ksDel (keys);
//! [Length 15]
}
{
//! [Hint 500]
KeySet * config = ksNew (500, keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key1", KEY_VALUE, "value1", KEY_END),
			 keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key2", KEY_VALUE, "value2", KEY_END),
			 keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key3", KEY_VALUE, "value3", KEY_END),
			 KS_END); // don't forget the KS_END at the end!
// work with it
ksDel (config);
//! [Hint 500]
}
}
