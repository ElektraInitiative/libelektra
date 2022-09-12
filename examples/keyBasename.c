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
ElektraKey * k = elektraKeyNew ("user:/my/long/name", ELEKTRA_KEY_END);
elektraKeySetBaseName (k, "myname");
printf ("%s\n", elektraKeyName (k)); // will print user:/my/long/myname
elektraKeyDel (k);
//! [set base basic]
}
{
//! [add base basic]
ElektraKey * k = elektraKeyNew ("user:/my/long", ELEKTRA_KEY_END);
elektraKeyAddBaseName (k, "myname");
printf ("%s\n", elektraKeyName (k)); // will print user:/my/long/myname
elektraKeyDel (k);
//! [add base basic]
}
{
//! [add base escaped]
ElektraKey * k = elektraKeyNew ("user:/my/long", ELEKTRA_KEY_END);
elektraKeyAddBaseName (k, "myname");
printf ("%s\n", elektraKeyName (k)); // will print user:/my/long/myname
elektraKeyDel (k);
//! [add base escaped]
}
}
