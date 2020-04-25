/**
 * @file
 *
 * @brief module for shutting down the gpg-agent
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "gpg_shutdown.h"
#include <stdio.h>
#include <stdlib.h>

/**
 * @brief shutdown the gpg-agent
 * @retval 0 on success.
 * @retval -1 on error.
 */
int ELEKTRA_PLUGIN_FUNCTION (gpgQuitAgent) (void)
{
	// check if the gpg-connect-agent command is available
	int cmdAvailable = system ("command -v gpg-connect-agent");
	if (cmdAvailable != 0)
	{
		// nothing to do here
		return 0;
	}

	// try to gracefully shut down the gpg-agent
	int status = system ("gpg-connect-agent --quiet KILLAGENT /bye");
	if (status != 0)
	{
		// use the hammer
		int killStatus = system ("/bin/sh -c \"pgrep \'gpg-agent\' | xargs -d \'\\n\' \'kill\'\"");
		if (killStatus != 0)
		{
			return -1;
		}
	}
	return 0;
}
