/**
 * @file
 * @brief Tests the unit plugin
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbconfig.h>
#include <kdbtypes.h>
#include <stdlib.h>
#include <string.h>
#include <tests_plugin.h>

static void test_unit_normalization (const char * unitstring, const char * unitexpected)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/unit", ELEKTRA_KEY_END);
	ElektraKey * hexkey = elektraKeyNew ("user:/test/unit/unittestval", ELEKTRA_KEY_VALUE, unitstring, ELEKTRA_KEY_META, "check/unit", "any", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (20, hexkey, ELEKTRA_KS_END);

	PLUGIN_OPEN ("unit");

	succeed_if ((plugin->kdbGet (plugin, ks, parentKey) >= 1), "kdbGet did not succeed");
	ElektraKey * foundKey = elektraKeysetLookupByName (ks, "user:/test/unit/unittestval", 0);
	succeed_if (!strcmp (elektraKeyString (foundKey), unitexpected), "Values dont match");
	printf ("test unit plugin normalization test - returned value: %s, expected value: %s\n", elektraKeyString (foundKey), unitexpected);

	succeed_if ((plugin->kdbSet (plugin, ks, parentKey) >= 1), "kdbSet did not succeed");
	foundKey = elektraKeysetLookupByName (ks, "user:/test/unit/unittestval", 0);
	succeed_if (!strcmp (elektraKeyString (foundKey), unitstring), "Values dont match");
	printf ("test unit plugin restoration test - returned value: %s, expected value: %s\n", elektraKeyString (foundKey), unitstring);

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_unit_normalization_error_expected (const char * unitstring, const char * unitexpected)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/unit", ELEKTRA_KEY_END);
	ElektraKey * hexkey = elektraKeyNew ("user:/test/unit/unittestval", ELEKTRA_KEY_VALUE, unitstring, ELEKTRA_KEY_META, "check/unit", "any", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (20, hexkey, ELEKTRA_KS_END);

	PLUGIN_OPEN ("unit");
	printf ("Testing usage of false value %s\n", unitstring);
	int statusCode = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (statusCode < 1, "kdbGet did succeed, despite it should not\n");
	printf ("kdbGet status code %s expected , result: %d\n", unitexpected, statusCode);

	statusCode = plugin->kdbSet (plugin, ks, parentKey);
	succeed_if (statusCode < 1, "kdbSet did succeed, despite it should not\n");
	printf ("kdbSet status code %s expected, result: %d\n", unitexpected, statusCode);

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_unit_validation (const char * unit, const short e_ret)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/unit", ELEKTRA_KEY_END);
	ElektraKey * hexkey = elektraKeyNew ("user:/test/unit/testvalue", ELEKTRA_KEY_VALUE, unit, ELEKTRA_KEY_META, "check/unit", "any", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (20, hexkey, ELEKTRA_KS_END);

	PLUGIN_OPEN ("unit");

	int ret = plugin->kdbSet (plugin, ks, parentKey);

	printf ("Test unit Validity %s, returned value: %d, expected value: %d\n", unit, ret, e_ret);
	succeed_if (ret == e_ret, "Test failed");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_unitplugin (void)
{

	test_unit_validation ("1Z", -1);
	test_unit_validation ("PSOB", -1);
	test_unit_validation ("123YMB", -1);
	test_unit_validation ("123YGB", -1);
	test_unit_validation ("1B", 1);
	test_unit_validation ("10 B", 1);

	test_unit_normalization_error_expected ("MB10GB", "-1");
	test_unit_normalization_error_expected ("X10B", "-1");

	test_unit_normalization_error_expected ("1B1B", "-1");
	test_unit_normalization_error_expected ("1BB", "-1");
	test_unit_normalization_error_expected ("10GB20MB", "-1");
	test_unit_normalization_error_expected ("MB10", "-1");

	test_unit_normalization ("10B", "10");
	test_unit_normalization ("10MB", "10000000");
	test_unit_normalization ("10 MB", "10000000");
	test_unit_normalization ("90 MB", "90000000");
	test_unit_normalization ("256 MB", "256000000");
	test_unit_normalization ("10GB", "10000000000");
	test_unit_normalization ("5TB", "5000000000000");
	test_unit_normalization ("10PB", "10000000000000000");
	test_unit_normalization ("100PB", "100000000000000000");
}

int main (int argc, char ** argv)
{
	(void) argc;
	(void) argv;
	test_unitplugin ();
	print_result ("testmod_unit");
	return nbError;
}
