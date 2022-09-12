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
ElektraKey * k = keyNew ("user:/x/r", KEY_END);
keyAddName (k, "../y/a//././z");
assert (!strcmp (keyName (k), "user:/x/y/a/z"));
keyDel (k);
//! [add name]
//! [namespace]
ElektraKey * n = keyNew ("user:/away", KEY_END);
keyAddName (n, "../../../new/name");
assert (!strcmp (keyName (n), "user:/new/name"));
keyDel (n);
//! [namespace]
}
