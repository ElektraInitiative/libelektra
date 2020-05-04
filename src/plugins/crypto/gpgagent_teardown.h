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

#include <sys/wait.h>
#include <unistd.h>

#define GPG_CONNECT_AGENT_CMD "gpg-connect-agent"

static inline void test_teardown (void)
{
	pid_t pid;
	int status;
	char * argv[] = { GPG_CONNECT_AGENT_CMD, "--quiet", "KILLAGENT", "/bye", NULL };

	// check if gpg-connect-agent is available and executable
	if (access (GPG_CONNECT_AGENT_CMD, F_OK))
	{
		return;
	}
	if (access (GPG_CONNECT_AGENT_CMD, X_OK))
	{
		return;
	}

	// execute the shutdown command
	switch (pid = fork ())
	{
	case -1: // failure
		yield_error ("fork failed");
		return;

	case 0: // child process
		if (execv (GPG_CONNECT_AGENT_CMD, argv) < 0)
		{
			exit (-1);
		}
		// end of the child process
	}

	// parent process - check if execv failed
	// NOTE the return value of gpg-connect-agent is irrelevant because it will
	//      always return 0 (see source code of GnuPG).
	waitpid (pid, &status, 0);
	succeed_if (status != -1, "failed to execute gpg-connect-agent");

	// wait for the agent to properly shut down
	if (status > 0)
	{
		pid = status;
		waitpid (pid, &status, 0);
	}
}

#endif
