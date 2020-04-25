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

static inline void test_teardown (void)
{
	// try to gracefully shut down the gpg-agent
	int status = system ("gpg-connect-agent --quiet KILLAGENT /bye");
	warn_if_fail (status == 0, "failed to stop the gpg-agent, trying to kill it");
	if (status != 0)
	{
		// use the hammer
		int killStatus = system ("/bin/sh -c \"pgrep \'gpg-agent\' | xargs -d \'\\n\' \'kill\'\"");
		warn_if_fail (killStatus == 0, "failed to kill the gpg-agent");
	}
}

#endif
