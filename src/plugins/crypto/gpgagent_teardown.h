#ifndef ELEKTRA_PLUGIN_CRYPTO_GPGAGENT_TEARDOWN_H
#define ELEKTRA_PLUGIN_CRYPTO_GPGAGENT_TEARDOWN_H

/**
 * @file
 *
 * @brief common method for shutting down the gpg-agent in unit tests
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

static inline void test_teardown (void)
{
	int status = system ("gpg-connect-agent --quiet KILLAGENT /bye");
	if (status != 0)
	{
		fprintf (stderr, "Terminating gpg-agent returned with status “%d”", status);
	}
}

#endif
