/*
 * \copydoc python.c
 */

#include <stdlib.h>
#include <Python.h>

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <tests_plugin.h>

static int Python_AppendToSysPath(const char *path)
{
	if (path == NULL)
		return 0;

	PyGILState_STATE gstate = PyGILState_Ensure();
	PyObject *sysPath = PySys_GetObject((char *)"path");
	PyObject *pyPath  = PyUnicode_FromString(path);
	PyList_Append(sysPath, pyPath);
	Py_DECREF(pyPath);
	PyGILState_Release(gstate);
	return 1;
}

void init_python()
{
	setenv("PYTHONDONTWRITEBYTECODE", "1", 1);
	if (!Py_IsInitialized())
		Py_Initialize();
	Python_AppendToSysPath(".");
}

// test simple variable passing
static void test_variable_passing()
{
	init_python();

	KeySet *conf = ksNew(1,
		keyNew("user/script", KEY_VALUE, srcdir_file("python/python_plugin.py"), KEY_END),
		KS_END);
	PLUGIN_OPEN(ELEKTRA_PLUGIN_NAME);

	Key *parentKey = keyNew("user/from_c", KEY_END);
	KeySet *ks = ksNew(0, KS_END);
	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (ksGetSize(ks) == 1, "keyset size is still 0");
	succeed_if (!strcmp(keyName(ksHead(ks)), "user/from_python"), "key in keyset has wrong name");

	ksDel (ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

// test loading python twice
static void test_two_scripts()
{
	init_python();

	KeySet *modules = ksNew(0, KS_END);
	elektraModulesInit(modules, 0);

	KeySet *conf = ksNew(1,
		keyNew("user/script", KEY_VALUE, srcdir_file("python/python_plugin.py"), KEY_END),
		KS_END);

	KeySet *conf2 = ksNew(1,
		keyNew("user/script", KEY_VALUE, srcdir_file("python/python_plugin2.py"), KEY_END),
		KS_END);

	Plugin *plugin = elektraPluginOpen(ELEKTRA_PLUGIN_NAME, modules, conf, 0);
	exit_if_fail (plugin != NULL, "unable to load python plugin"); \

	Plugin *plugin2 = elektraPluginOpen(ELEKTRA_PLUGIN_NAME, modules, conf2, 0);
	exit_if_fail (plugin2 != NULL, "unable to load python plugin again");

	elektraPluginClose(plugin2, 0);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel(modules);
}

// simple return value test
static void test_fail()
{
	init_python();

	KeySet *conf = ksNew(1,
		keyNew("user/script", KEY_VALUE, srcdir_file("python/python_plugin_fail.py"), KEY_END),
		KS_END);
	PLUGIN_OPEN(ELEKTRA_PLUGIN_NAME);

	Key *parentKey = keyNew("user/tests/from_c", KEY_END);
	KeySet *ks = ksNew(0, KS_END);

	succeed_if (plugin->kdbGet(plugin, ks, parentKey) == -1,   "call to kdbGet didn't fail");
	succeed_if (plugin->kdbSet(plugin, ks, parentKey) == -1,   "call to kdbSet didn't fail");
	succeed_if (plugin->kdbError(plugin, ks, parentKey) == -1, "call to kdbError didn't fail");

	ksDel (ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

// test script with wrong class name
static void test_wrong()
{
	init_python();

	KeySet *modules = ksNew(0, KS_END);
	elektraModulesInit(modules, 0);

	KeySet *conf = ksNew(1,
		keyNew("user/script", KEY_VALUE, srcdir_file("python/python_plugin_wrong.py"), KEY_END),
		KS_END);

	Plugin *plugin = elektraPluginOpen(ELEKTRA_PLUGIN_NAME, modules, conf, 0);
	succeed_if (plugin == NULL, "python plugin shouldn't be loadable"); \

	elektraModulesClose(modules, 0);
	ksDel(modules);
}

int main(int argc, char** argv)
{
	printf("MOUNT       TESTS\n");
	printf("==================\n\n");

	init(argc, argv);

	test_variable_passing();
	test_two_scripts();

	printf("NOTE: The following errors are intended. We're testing error conditions!\n");
	test_fail();
	test_wrong();

	printf("\ntest_hosts RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
