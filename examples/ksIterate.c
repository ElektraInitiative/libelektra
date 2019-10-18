/**
 * @file
 *
 * @brief examples how to iterate over keysets
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <stddef.h>
#include <stdio.h>

void iterate_for (KeySet * ks)
{
	//! [iterate for]
	Key * cur;
	for (cursor_t cursor = 0; (cur = ksAtCursor (ks, cursor)) != NULL; ++cursor)
	{
		printf ("%s\n", keyName (cur));
	}
	//! [iterate for]
}


void iterate_while (KeySet * ks)
{
	//! [iterate while]
	cursor_t cursor = 0;
	Key * cur;

	while ((cur = ksAtCursor (ks, cursor)) != 0)
	{
		printf ("%s\n", keyName (cur));
		++cursor;
	}
	//! [iterate while]
}

int main (void)
{
	KeySet * ks = ksNew (20, keyNew ("user/name1", 0), keyNew ("user/name2", 0), keyNew ("user/name3", 0), KS_END);
	iterate_while (ks);
	iterate_for (ks);
}
