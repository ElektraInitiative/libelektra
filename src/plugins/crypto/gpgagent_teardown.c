/**
 * @file
 *
 * @brief common method for shuttind down the gpg-agent in unit tests
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

static void test_teardown (void)
{
	int status = system ("gpg-connect-agent --quiet KILLAGENT /bye");
	if (status != 0)
	{
		fprintf (stderr, "Terminating gpg-agent returned with status “%d”", status);
	}
}
