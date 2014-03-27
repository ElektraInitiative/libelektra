/*
 * \copydoc python.c
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <tests_plugin.h>

void test_simple()
{
	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);

	KeySet *conf = ksNew(1,
		keyNew("user/script", KEY_VALUE, srcdir_file("test_elektra.py"), KEY_END),
		KS_END);

	Plugin *plugin = elektraPluginOpen("python", modules, conf, 0);

	Key *parentKey = keyNew(0);
	keyDel(parentKey);

	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel(modules);
}

void test_two_scripts()
{
	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);

	KeySet *conf = ksNew(1,
		keyNew("user/script", KEY_VALUE, srcdir_file("test_elektra.py"), KEY_END),
		KS_END);
	KeySet *conf2 = ksNew(1,
		keyNew("user/script", KEY_VALUE, srcdir_file("test_elektra2.py"), KEY_END),
		KS_END);

	Plugin *plugin = elektraPluginOpen("python", modules, conf, 0);
	Plugin *plugin2 = elektraPluginOpen("python", modules, conf2, 0);

	Key *parentKey = keyNew(0);
	keyDel(parentKey);

	elektraPluginClose(plugin2, 0);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel(modules);
}

int main(int argc, char** argv)
{
	printf("MOUNT       TESTS\n");
	printf("==================\n\n");

	init(argc, argv);

	test_simple();
	test_two_scripts();

	printf("\ntest_hosts RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
