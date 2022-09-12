/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stddef.h>

//! [open]
void thread1 (void)
{
	ElektraKey * parent = elektraKeyNew ("/app/part1", ELEKTRA_KEY_END);
	ElektraKdb * h = elektraKdbOpen (NULL, parent);
	// fetch keys and work with them
	elektraKdbClose (h, parent);
}
void thread2 (void)
{
	ElektraKey * parent = elektraKeyNew ("/app/part2", ELEKTRA_KEY_END);
	ElektraKdb * h = elektraKdbOpen (NULL, parent);
	// fetch keys and work with them
	elektraKdbClose (h, parent);
}
//! [open]

int main (void)
{
	thread1 ();
	thread2 ();
}
