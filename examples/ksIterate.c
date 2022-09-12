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
	for (elektraCursor cursor = 0; (cur = elektraKeysetAtCursor (ks, cursor)) != NULL; ++cursor)
	{
		printf ("%s\n", elektraKeyName (cur));
	}
	//! [iterate for]
}


void iterate_while (ElektraKeyset * ks)
{
	//! [iterate while]
	elektraCursor cursor = 0;
	ElektraKey * cur;

	while ((cur = elektraKeysetAtCursor (ks, cursor)) != 0)
	{
		printf ("%s\n", elektraKeyName (cur));
		++cursor;
	}
	//! [iterate while]
}

int main (void)
{
	ElektraKeyset * ks = elektraKeysetNew (20, elektraKeyNew ("user:/name1", ELEKTRA_KEY_END), elektraKeyNew ("user:/name2", ELEKTRA_KEY_END), elektraKeyNew ("user:/name3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	iterate_while (ks);
	iterate_for (ks);
}
