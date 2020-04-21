/**
 * @file
 *
 * @brief common method for shutting down the gpg-agent in unit tests
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#ifndef ELEKTRA_PLUGIN_CRYPTO_GPGAGENT_TEARDOWN_H
#define ELEKTRA_PLUGIN_CRYPTO_GPGAGENT_TEARDOWN_H

#include <stdio.h>
#include <unistd.h> // notice this! you need it!

static inline void test_teardown (void)
{
	//////////////////////////////////////////////////////////////////////////////
	// DEBUG - This code is meant for the build server                          //
	//////////////////////////////////////////////////////////////////////////////
	system ("stress-ng --fork 2 --timeout 10s --metrics-brief &");
	sleep (5); // wait 2 seconds for the stress to start
	//////////////////////////////////////////////////////////////////////////////

	// try to gracefully shut down the gpg-agent
	int status = system ("gpg-connect-agent --quiet KILLAGENT /bye");
	if (status != 0)
	{
		// use the hammer
		int killStatus = system ("/bin/sh -c \"pgrep \'gpg-agent\' | xargs -d \'\\n\' \'kill\'\"");
		if (killStatus != 0)
		{
			fprintf (stderr, "ERROR: Terminating gpg-agent returned with status %d.\nFailed to kill the gpg-agent (status %d).",
				 status, killStatus);
		}
		else
		{
			fprintf (stdout, "INFO: killed gpg-agent because shutdown failed with status %d.\n", status);
		}
	}
}

#endif
