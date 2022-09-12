/**
 * @file
 *
 * @brief examples how to iterate over keysets
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stddef.h>
#include <stdio.h>

void iterate_for (ElektraKeyset * ks)
{
	//! [iterate for]
	ElektraKey * cur;
	for (elektraCursor cursor = 0; (cur = ksAtCursor (ks, cursor)) != NULL; ++cursor)
	{
		printf ("%s\n", keyName (cur));
	}
	//! [iterate for]
}


void iterate_while (ElektraKeyset * ks)
{
	//! [iterate while]
	elektraCursor cursor = 0;
	ElektraKey * cur;

	while ((cur = ksAtCursor (ks, cursor)) != 0)
	{
		printf ("%s\n", keyName (cur));
		++cursor;
	}
	//! [iterate while]
}

int main (void)
{
	ElektraKeyset * ks = ksNew (20, keyNew ("user:/name1", ELEKTRA_KEY_END), keyNew ("user:/name2", ELEKTRA_KEY_END), keyNew ("user:/name3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	iterate_while (ks);
	iterate_for (ks);
}
