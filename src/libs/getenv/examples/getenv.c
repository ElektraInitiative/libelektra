/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char** argv, char** environ)
{
	if (argc == 1)
	{
		char** env;
		for (env = environ; *env != 0; env++)
		{
			const size_t len = strcspn(*env, "=");
			char name[len+1];
			strncpy(name, *env, len);
			name[len] = 0;
			const char *c = getenv(name);
			printf ("getenv(\"%s\") -> ", name);
			printf ("%s\n", c);
		}
	}
	else
	{
		for (int i=1; i<argc; ++i)
		{
			const char *name = argv[i];
			const char *c = getenv(name);
			if (!c) return 1;
			printf ("%s\n", c);
		}
	}
	return 0;
}
