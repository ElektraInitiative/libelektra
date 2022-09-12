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
	ElektraKey * parent = keyNew ("/app/part1", ELEKTRA_KEY_END);
	ElektraKdb * h = kdbOpen (NULL, parent);
	// fetch keys and work with them
	kdbClose (h, parent);
}
void thread2 (void)
{
	ElektraKey * parent = keyNew ("/app/part2", ELEKTRA_KEY_END);
	ElektraKdb * h = kdbOpen (NULL, parent);
	// fetch keys and work with them
	kdbClose (h, parent);
}
//! [open]

int main (void)
{
	thread1 ();
	thread2 ();
}
