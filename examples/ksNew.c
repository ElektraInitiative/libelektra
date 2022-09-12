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
ElektraKeyset * keys = elektraKeysetNew (1, ELEKTRA_KS_END);
// enough memory for up to 16 keys, without needing reallocation
elektraKeysetDel (keys);
//! [Simple]
}

{
//! [No Allocation]
// Create KeySet without allocating memory for keys
ElektraKeyset * keys = elektraKeysetNew (0, ELEKTRA_KS_END);
// The first allocation will happen in ksAppendKey
elektraKeysetAppendKey(keys, elektraKeyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key02", ELEKTRA_KEY_VALUE, "value02", ELEKTRA_KEY_END));
// work with the KeySet
elektraKeysetDel (keys);
//! [No Allocation]
}

{
//! [Length 15]
ElektraKeyset * keys = elektraKeysetNew (15, elektraKeyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key01", ELEKTRA_KEY_VALUE, "value01", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key02", ELEKTRA_KEY_VALUE, "value02", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key03", ELEKTRA_KEY_VALUE, "value03", ELEKTRA_KEY_END),
		       // ...
		       elektraKeyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key15", ELEKTRA_KEY_VALUE, "value15", ELEKTRA_KEY_END), ELEKTRA_KS_END);
// work with it
elektraKeysetDel (keys);
//! [Length 15]
}
{
//! [Hint 500]
ElektraKeyset * config = elektraKeysetNew (500, elektraKeyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key1", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END),
			 elektraKeyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key2", ELEKTRA_KEY_VALUE, "value2", ELEKTRA_KEY_END),
			 elektraKeyNew ("user:/sw/org/app/#0/current/fixedConfiguration/key3", ELEKTRA_KEY_VALUE, "value3", ELEKTRA_KEY_END),
			 ELEKTRA_KS_END); // don't forget the KS_END at the end!
// work with it
elektraKeysetDel (config);
//! [Hint 500]
}
}
