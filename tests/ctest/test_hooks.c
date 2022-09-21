/**
* @file
*
* @brief
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#include "../../src/libs/elektra/hooks.c"
#include <tests.h>

static void test_getPluginConfigFromContract_withConfigInContract (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract =
		ksNew (30, keyNew ("system:/elektra/something", KEY_VALUE, "3", KEY_END),
		       keyNew ("system:/elektra/contract/mountglobal/myPlugin", KEY_END),
		       keyNew ("system:/elektra/contract/mountglobal/myPlugin/someValue", KEY_VALUE, "1", KEY_END),
		       keyNew ("system:/elektra/contract/mountglobal/myPlugin/someOtherValue", KEY_VALUE, "2", KEY_END),
		       keyNew ("system:/elektra/contract/mountglobal/otherPlugin", KEY_END),
		       keyNew ("system:/elektra/contract/mountglobal/otherPlugin/someValue", KEY_VALUE, "1", KEY_END),
		       keyNew ("system:/elektra/myPlugin", KEY_END),
		       keyNew ("system:/elektra/myPlugin/shouldntSeeMe", KEY_VALUE, "5", KEY_END),
		       keyNew ("system:/elektra/record/enabled", KEY_VALUE, "0", KEY_END), KS_END);

	// Act
	KeySet * result = getPluginConfigFromContract ("myPlugin", contract);

	// Assert
	succeed_if (result != NULL, "result must not be NULL");
	succeed_if (ksGetSize (result) == 3, "expected resulting config to have 3 keys");
	succeed_if (ksLookupByName (result, "user:/", KDB_O_NONE) != NULL, "must contain key user:/");
	succeed_if (ksLookupByName (result, "user:/someValue", KDB_O_NONE) != NULL, "must contain key user:/someValue");
	succeed_if (ksLookupByName (result, "user:/someOtherValue", KDB_O_NONE) != NULL, "must contain key user:/someOtherValue");

	ksDel(contract);
	ksDel (result);
}

static void test_getPluginConfigFromContract_withoutConfigInContract (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract =
		ksNew (30, keyNew ("system:/elektra/something", KEY_VALUE, "3", KEY_END),
		       keyNew ("system:/elektra/contract/mountglobal/otherPlugin", KEY_END),
		       keyNew ("system:/elektra/contract/mountglobal/otherPlugin/someValue", KEY_VALUE, "1", KEY_END),
		       keyNew ("system:/elektra/myPlugin", KEY_END),
		       keyNew ("system:/elektra/myPlugin/shouldntSeeMe", KEY_VALUE, "5", KEY_END),
		       keyNew ("system:/elektra/record/enabled", KEY_VALUE, "0", KEY_END), KS_END);

	// Act
	KeySet * result = getPluginConfigFromContract ("myPlugin", contract);

	// Assert
	succeed_if (result != NULL, "result must not be NULL");
	succeed_if (ksGetSize (result) == 0, "expected resulting config to have 0 keys");

	ksDel(contract);
	ksDel (result);
}

int main(int argc, char ** argv)
{
	printf ("HOOKS       TESTS\n");
	printf ("=================\n\n");

	init (argc, argv);
	test_getPluginConfigFromContract_withConfigInContract ();
	test_getPluginConfigFromContract_withoutConfigInContract();

	printf ("\ntest_hooks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}


