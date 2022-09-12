/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>

#include <stdio.h>

int main (void)
{
	// clang-format off
{
//! [set base basic]
ElektraKey * k = keyNew ("user:/my/long/name", ELEKTRA_KEY_END);
keySetBaseName (k, "myname");
printf ("%s\n", keyName (k)); // will print user:/my/long/myname
keyDel (k);
//! [set base basic]
}
{
//! [add base basic]
ElektraKey * k = keyNew ("user:/my/long", ELEKTRA_KEY_END);
keyAddBaseName (k, "myname");
printf ("%s\n", keyName (k)); // will print user:/my/long/myname
keyDel (k);
//! [add base basic]
}
{
//! [add base escaped]
ElektraKey * k = keyNew ("user:/my/long", ELEKTRA_KEY_END);
keyAddBaseName (k, "myname");
printf ("%s\n", keyName (k)); // will print user:/my/long/myname
keyDel (k);
//! [add base escaped]
}
}
