/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdb.h>

#include <stdio.h>

int main ()
{
// clang-format off
{
//! [set base basic]
Key * k = keyNew ("user/my/long/name", KEY_END);
keySetBaseName (k, "myname");
printf ("%s\n", keyName (k)); // will print user/my/long/myname
keyDel (k);
//! [set base basic]
}
{
//! [add base basic]
Key * k = keyNew ("user/my/long", KEY_END);
keyAddBaseName (k, "myname");
printf ("%s\n", keyName (k)); // will print user/my/long/myname
keyDel (k);
//! [add base basic]
}
{
//! [add base escaped]
Key * k = keyNew ("user/my/long", KEY_END);
keyAddBaseName (k, "myname");
printf ("%s\n", keyName (k)); // will print user/my/long/myname
keyDel (k);
//! [add base escaped]
}
}
