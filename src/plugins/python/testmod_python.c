/*
 * \copydoc python.c
 */

#include <stdlib.h>

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <tests_plugin.h>

static void init_env()
{
	setenv("PYTHONDONTWRITEBYTECODE", "1", 1);
	setenv("PYTHONPATH", ".", 1);
}

char filebuf[KDB_MAX_PATH_LENGTH];
static char *srcdir_rewrite = NULL;
static char *python_file(const char *filename)
{
	if (!srcdir_rewrite)
	{
		/* no rewrite. just append our plugin name */
		strcpy(filebuf, PYTHON_PLUGIN_NAME);
		strcat(strcat(filebuf, "/"), filename);
		return srcdir_file(filebuf);
	}

	/* wipe old value */
	*srcdir_rewrite = '\0';

	/* append plugin name and delete last character */
	strcat(strcat(filebuf, "/"), PYTHON_PLUGIN_NAME);
	*(filebuf + strlen(filebuf) - 1) = '\0';

	strcat(strcat(filebuf, "/"), filename);
	return filebuf;
}

// test simple variable passing
static void test_variable_passing()
{
	printf("Testing simple variable passing...\n");

	KeySet *conf = ksNew(1,
		keyNew("user/script", KEY_VALUE, python_file("python_plugin.py"), KEY_END),
		keyNew("user/shutdown", KEY_VALUE, "1", KEY_END),
		keyNew("user/print", KEY_END),
		KS_END);
	PLUGIN_OPEN(PYTHON_PLUGIN_NAME);

	Key *parentKey = keyNew("user/from_c", KEY_END);
	KeySet *ks = ksNew(0, KS_END);
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if(ksGetSize(ks) == 1, "keyset size is still 0");
	succeed_if(ksGetSize(ks) == 1 && !strcmp(keyName(ksHead(ks)), "user/from_python"), "key in keyset has wrong name");

	ksDel(ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

// test loading python twice
static void test_two_scripts()
{
	printf("Testing loading of two active python plugins...\n");

	KeySet *modules = ksNew(0, KS_END);
	elektraModulesInit(modules, 0);

	KeySet *conf = ksNew(2,
		keyNew("user/script", KEY_VALUE, python_file("python_plugin.py"), KEY_END),
		keyNew("user/shutdown", KEY_VALUE, "1", KEY_END),
		keyNew("user/print", KEY_END),
		KS_END);

	KeySet *conf2 = ksNew(2,
		keyNew("user/script", KEY_VALUE, python_file("python_plugin2.py"), KEY_END),
		keyNew("user/shutdown", KEY_VALUE, "1", KEY_END),
		keyNew("user/print", KEY_END),
		KS_END);

	Key *errorKey = keyNew("", KEY_END);
	Plugin *plugin = elektraPluginOpen(PYTHON_PLUGIN_NAME, modules, conf, errorKey);
	succeed_if(output_warnings(errorKey), "warnings in kdbOpen");
	succeed_if(output_error(errorKey),    "errors in kdbOpen");
	exit_if_fail(plugin != NULL, "unable to load python plugin");
	keyDel(errorKey);

	Key *errorKey2 = keyNew("", KEY_END);
	Plugin *plugin2 = elektraPluginOpen(PYTHON_PLUGIN_NAME, modules, conf2, errorKey2);
	succeed_if(output_warnings(errorKey2), "warnings in kdbOpen");
	succeed_if(output_error(errorKey2),    "errors in kdbOpen");
	exit_if_fail(plugin2 != NULL, "unable to load python plugin again");
	keyDel(errorKey2);

	elektraPluginClose(plugin2, 0);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel(modules);
}

// simple return value test
static void test_fail()
{
	printf("Testing return values from python functions...\n");

	KeySet *conf = ksNew(2,
		keyNew("user/script", KEY_VALUE, python_file("python_plugin_fail.py"), KEY_END),
		keyNew("user/shutdown", KEY_VALUE, "1", KEY_END),
		keyNew("user/print", KEY_END),
		KS_END);
	PLUGIN_OPEN(PYTHON_PLUGIN_NAME);

	Key *parentKey = keyNew("user/tests/from_c", KEY_END);
	KeySet *ks = ksNew(0, KS_END);

	succeed_if(plugin->kdbGet(plugin, ks, parentKey) == -1,   "call to kdbGet didn't fail");
	succeed_if(plugin->kdbSet(plugin, ks, parentKey) == -1,   "call to kdbSet didn't fail");
	succeed_if(plugin->kdbError(plugin, ks, parentKey) == -1, "call to kdbError didn't fail");

	ksDel(ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

// test script with wrong class name
static void test_wrong()
{
	printf("Testing python script with wrong class name...\n");

	KeySet *modules = ksNew(0, KS_END);
	elektraModulesInit(modules, 0);

	KeySet *conf = ksNew(2,
		keyNew("user/script", KEY_VALUE, python_file("python_plugin_wrong.py"), KEY_END),
		keyNew("user/shutdown", KEY_VALUE, "1", KEY_END),
		keyNew("user/print", KEY_END),
		KS_END);

	Key *errorKey = keyNew("", KEY_END);
	Plugin *plugin = elektraPluginOpen(PYTHON_PLUGIN_NAME, modules, conf, errorKey);
	succeed_if(!output_warnings(errorKey), "we expect some warnings");
	succeed_if(!output_error(errorKey),    "we expect some errors");
	succeed_if(plugin == NULL, "python plugin shouldn't be loadable");
	keyDel(errorKey);

	elektraModulesClose(modules, 0);
	ksDel(modules);
}

int main(int argc, char** argv)
{
	printf("PYTHON      TESTS\n");
	printf("==================\n\n");

	init(argc, argv);
	if (argc > 1) {
		strncpy(filebuf, argv[1], sizeof(filebuf));
		/* our files are in pythons plugin directory
		 * -> rewrite srcdir from xxx/python2 to xxx/python
		 */
		if (strlen(filebuf) > strlen("python2") && !strcmp(filebuf + strlen(filebuf)
					- strlen("python2"), "python2"))
		{
			srcdir_rewrite = filebuf + strlen(filebuf) - 1;
			*srcdir_rewrite = '\0';
		}
	}
	init_env();

	test_variable_passing();
	test_two_scripts();

	printf("\n");
	printf("========================================================================\n");
	printf("NOTE: The following errors are intended. We're testing error conditions!\n");
	printf("========================================================================\n");
	test_fail();
	test_wrong();

	printf("\ntest_python RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
