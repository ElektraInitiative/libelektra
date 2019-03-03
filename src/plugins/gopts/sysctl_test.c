/**
 * @file
 *
 * @brief Source for opts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "gopts_sysctl.h"

#include <string.h>

int main (int argc, char ** argv)
{
	char ** argv2;
	int argc2 = loadArgs (&argv2);

	if (argc2 != argc)
	{
		cleanupArgs (argc2, argv2);
		return 1;
	}

	for (int i = 0; i < argc; ++i)
	{
		if (argv2[i] == NULL || strcmp (argv[i], argv2[i]) != 0)
		{
			cleanupArgs (argc2, argv2);
			return 1;
		}
	}

	return 0;
}
