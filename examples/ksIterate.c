/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stddef.h>
#include <stdio.h>

//! [iterate for]
void iterate_for (KeySet * ks)
{
	Key * cur;
	for (cursor_t cursor = 0; (cur = ksAtCursor (ks, cursor)) != NULL; ++cursor)
	{
		printf ("%s\n", keyName (cur));
	}
}
//! [iterate for]

//! [iterate while]
void iterate_while (KeySet * ks)
{
	cursor_t cursor = 0;
	Key * cursor_key;

	while ((cursor_key = ksAtCursor (ks, cursor)) != 0)
	{
		printf ("%s\n", keyName (cursor_key));
		cursor++;
	}
}
//! [iterate while]

int main (void)
{
	KeySet * ks = ksNew (20, keyNew ("user/name1", 0), keyNew ("user/name2", 0), keyNew ("user/name3", 0), KS_END);
	iterate_while (ks);
	iterate_for (ks);
}
