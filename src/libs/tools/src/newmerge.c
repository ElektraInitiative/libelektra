#include "newmerge.h"

int mymerge (void)
{
	test ();
	return 3;
}

/**
 * This file finds newmerge.h
 * Else this function would be implicitly declared
 * which would give a compiler warning
 */
int test (void)
{
	return 4;
}
