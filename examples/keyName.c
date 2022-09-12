/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <assert.h>
#include <string.h>

#include <kdb.h>

int main (void)
{
	// clang-format off
//! [add name]
ElektraKey * k = elektraKeyNew ("user:/x/r", ELEKTRA_KEY_END);
elektraKeyAddName (k, "../y/a//././z");
assert (!strcmp (elektraKeyName (k), "user:/x/y/a/z"));
elektraKeyDel (k);
//! [add name]
//! [namespace]
ElektraKey * n = elektraKeyNew ("user:/away", ELEKTRA_KEY_END);
elektraKeyAddName (n, "../../../new/name");
assert (!strcmp (elektraKeyName (n), "user:/new/name"));
elektraKeyDel (n);
//! [namespace]
}
