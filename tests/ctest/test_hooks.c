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
	KeySet * contract = ksNew (30, keyNew ("system:/elektra/something", KEY_VALUE, "3", KEY_END),
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

	ksDel (contract);
	ksDel (result);
}

static void test_getPluginConfigFromContract_withoutConfigInContract (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * contract = ksNew (30, keyNew ("system:/elektra/something", KEY_VALUE, "3", KEY_END),
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

	ksDel (contract);
	ksDel (result);
}

static void test_loadPlugin (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * global = ksNew (0, KS_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * contract = ksNew (30, keyNew ("system:/elektra/contract/mountglobal/gopts", KEY_END),
				   keyNew ("system:/elektra/contract/mountglobal/gopts/offset", KEY_VALUE, "1", KEY_END), KS_END);

	Key * errorKey = keyNew ("system:/hello", KEY_END);

	// Act
	Plugin * plugin = loadPlugin ("gopts", global, modules, contract, errorKey);

	// Assert
	succeed_if (plugin != NULL, "must be able to load plugin");
	succeed_if (plugin->modules == modules, "must set modules");
	succeed_if (plugin->global == global, "must set global");
	succeed_if (ksGetSize (plugin->config) == 2, "config size must be 2");

	ksDel (global);
	ksDel (modules);
	ksDel (contract);
	elektraPluginClose (plugin, errorKey);
	keyDel (errorKey);
}

static void test_loadPlugin_inexistent (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KeySet * global = ksNew (0, KS_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * contract = ksNew (0, KS_END);

	Key * errorKey = keyNew ("system:/hello", KEY_END);

	// Act
	Plugin * plugin = loadPlugin ("thisPluginSurelyDoesNotExist123", global, modules, contract, errorKey);

	// Assert
	succeed_if (plugin == NULL, "must not load plugin");

	ksDel (global);
	ksDel (modules);
	ksDel (contract);
	keyDel (errorKey);
}

static void test_isGoptsEnabledByContract (bool shouldBeEnabled)
{
	printf ("Executing %s with shouldBeEnabled=%d\n", __func__, shouldBeEnabled);

	// Arrange
	KeySet * contract = ksNew (1, KS_END);
	if (shouldBeEnabled)
	{
		ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/gopts", KEY_END));
	}

	// Act
	bool result = isGoptsEnabledByContract (contract);

	// Assert
	succeed_if_fmt (result == shouldBeEnabled, "result is %d but should be %d", result, shouldBeEnabled);

	ksDel (contract);
}

static void test_initHooks_shouldInitAllHooksWithoutFailure (void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KDB * kdb = elektraCalloc (sizeof (struct _KDB));
	KeySet * config = ksNew (0, KS_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * contract = ksNew (1, keyNew ("system:/elektra/contract/mountglobal/gopts", KEY_END), KS_END);

	kdb->global = ksNew (0, KS_END);
	kdb->modules = modules;

	Key * errorKey = keyNew ("system:/elektra", KS_END);

	// Act
	int result = initHooks (kdb, config, modules, contract, errorKey);

	// Assert
	KeySet * meta = keyMeta (errorKey);

	succeed_if (result == 0, "result should be 0");
	succeed_if (ksGetSize (meta) == 0, "error key should not have meta data");
	succeed_if (kdb->hooks.gopts.plugin != NULL, "gopts plugin should be loaded");
	succeed_if (kdb->hooks.gopts.get != NULL, "gopts.get should be found");
	succeed_if (kdb->hooks.spec.plugin != NULL, "spec plugin should be loaded");
	succeed_if (kdb->hooks.spec.copy != NULL, "spec.copy should be found");
	succeed_if (kdb->hooks.spec.remove != NULL, "spec.remove should be found");

	ksDel (config);
	ksDel (modules);
	ksDel (contract);

	elektraFree (kdb);
}

static void test_initHooksSendNotifications(void)
{
	printf ("Executing %s\n", __func__);

	// Arrange
	KDB * kdb = elektraCalloc (sizeof (struct _KDB));
	Key * errorKey = keyNew ("/", KEY_END);

	KeySet * contract = ksNew (0, KS_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * config = ksNew (16,
				 keyNew ("system:/elektra/hook/notification/send/plugins/#0", KEY_VALUE, "dbus", KEY_END),
				 keyNew ("system:/elektra/hook/notification/send/plugins/#1", KEY_VALUE, "internalnotification", KEY_END),
				 keyNew ("system:/elektra/hook/notification/send/plugins/#2", KEY_VALUE, "record", KEY_END),
				 KS_END);


	// Act
	initHooksSendNotifications (kdb, config, modules, contract, errorKey);

	// Assert

	ksDel (config);
	keyDel (errorKey);
	elektraFree (kdb);
}

int main (int argc, char ** argv)
{
	printf ("HOOKS       TESTS\n");
	printf ("=================\n\n");

	init (argc, argv);
	test_getPluginConfigFromContract_withConfigInContract ();
	test_getPluginConfigFromContract_withoutConfigInContract ();
	test_loadPlugin ();
	test_loadPlugin_inexistent ();
	test_isGoptsEnabledByContract (true);
	test_isGoptsEnabledByContract (false);
	test_initHooks_shouldInitAllHooksWithoutFailure ();
	test_initHooksSendNotifications ();

	printf ("\ntest_hooks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
