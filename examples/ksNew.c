/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <stdio.h>

// clang-format off

int main (void)
{
{
//! [Simple]
KeySet * keys = ksNew (0, KS_END);
// work with it
ksDel (keys);
//! [Simple]
}

{
//! [Length 15]
KeySet * keys = ksNew (15, keyNew ("user/sw/org/app/#0/current/fixedConfiguration/key01", KEY_VALUE, "value01", 0),
		       keyNew ("user/sw/org/app/#0/current/fixedConfiguration/key02", KEY_VALUE, "value02", 0),
		       keyNew ("user/sw/org/app/#0/current/fixedConfiguration/key03", KEY_VALUE, "value03", 0),
		       // ...
		       keyNew ("user/sw/org/app/#0/current/fixedConfiguration/key15", KEY_VALUE, "value15", 0), KS_END);
// work with it
ksDel (keys);
//! [Length 15]
}
{
//! [Hint 500]
KeySet * config = ksNew (500, keyNew ("user/sw/org/app/#0/current/fixedConfiguration/key1", KEY_VALUE, "value1", 0),
			 keyNew ("user/sw/org/app/#0/current/fixedConfiguration/key2", KEY_VALUE, "value2", 0),
			 keyNew ("user/sw/org/app/#0/current/fixedConfiguration/key3", KEY_VALUE, "value3", 0),
			 KS_END); // don't forget the KS_END at the end!
// work with it
ksDel (config);
//! [Hint 500]
}
}
