/**
 * @file
 *
 * @brief Source for opts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <sys/types.h> // has to be included before sys/sysctl.h

#include <sys/sysctl.h>

#ifndef KERN_PROC
#error no KERN_PROC
#endif

#ifndef KERN_PROC_ARGS
#error no KERN_PROC_ARGS
#endif

int main (void)
{
	return 0;
}
