/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <stddef.h>

//! [open]
void thread1 (void)
{
	Key * parent = keyNew ("/app/part1", KEY_END);
	KDB * h = kdbOpen (NULL, parent);
	// fetch keys and work with them
	kdbClose (h, parent);
}
void thread2 (void)
{
	Key * parent = keyNew ("/app/part2", KEY_END);
	KDB * h = kdbOpen (NULL, parent);
	// fetch keys and work with them
	kdbClose (h, parent);
}
//! [open]

int main (void)
{
	thread1 ();
	thread2 ();
}
