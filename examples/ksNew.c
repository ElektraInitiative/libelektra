/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stdio.h>

// clang-format off

int main (void)
{
{
//! [Simple]
ElektraKeyset * keys = ksNew (1, ELEKTRA_KS_END);
// enough memory for up to 16 keys, without needing reallocation
ksDel (keys);
//! [Simple]
}

{
//! [No Allocation]
// Create KeySet without allocating memory for keys
ElektraKeyset * keys = ksNew (0, ELEKTRA_KS_END);
// The first allocation will happen in ksAppendKey
ksAppendKey(keys, keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key02", ELEKTRA_KEY_VALUE, "value02", ELEKTRA_KEY_END));
// work with the KeySet
ksDel (keys);
//! [No Allocation]
}

{
//! [Length 15]
ElektraKeyset * keys = ksNew (15, keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key01", ELEKTRA_KEY_VALUE, "value01", ELEKTRA_KEY_END),
		       keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key02", ELEKTRA_KEY_VALUE, "value02", ELEKTRA_KEY_END),
		       keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key03", ELEKTRA_KEY_VALUE, "value03", ELEKTRA_KEY_END),
		       // ...
		       keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key15", ELEKTRA_KEY_VALUE, "value15", ELEKTRA_KEY_END), ELEKTRA_KS_END);
// work with it
ksDel (keys);
//! [Length 15]
}
{
//! [Hint 500]
ElektraKeyset * config = ksNew (500, keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key1", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END),
			 keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key2", ELEKTRA_KEY_VALUE, "value2", ELEKTRA_KEY_END),
			 keyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key3", ELEKTRA_KEY_VALUE, "value3", ELEKTRA_KEY_END),
			 ELEKTRA_KS_END); // don't forget the KS_END at the end!
// work with it
ksDel (config);
//! [Hint 500]
}
}
