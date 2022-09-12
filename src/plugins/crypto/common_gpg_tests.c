/**
 * @file
 *
 * @brief common functions for gpg-related unit testing.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

static int gpg_available (ElektraKeyset * conf)
{
	int available = 0;
	char * gpgPath = NULL;
	ElektraKey * parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);

	int gpg_search_result = ELEKTRA_PLUGIN_FUNCTION (gpgGetBinary) (&gpgPath, conf, parentKey);
	if (gpg_search_result == 1)
	{
		available = 1;
	}

	if (gpgPath)
	{
		elektraFree (gpgPath);
	}
	elektraKeyDel (parentKey);
	elektraKeysetDel (conf);
	return available;
}
