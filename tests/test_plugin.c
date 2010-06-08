/*************************************************************************** 
 *      test_plugin.c  -  Test cases for how to build
*        a backend out of system/elektra/mountpoints/<name>
 *                  -------------------
 *  begin                : Wed 19 May, 2010
 *  copyright            : (C) 2010 by Markus Raab
 *  email                : elektra@markus-raab.org
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif


#include <tests.h>

int elektraProcessPlugin(Key *cur, int *pluginNumber, char **pluginName, char **referenceName);

void test_process(void)
{
	printf ("Test processing of plugin name\n");

	Key *k = keyNew ("system/elektra/#0name", KEY_END);
	int pluginNumber = -1;
	char *pluginName = 0;
	char *referenceName = 0;

	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 1, "process plugin error");
	succeed_if (pluginNumber == 0, "number not correct");
	succeed_if (!strcmp(pluginName, "name"), "plugin name not correct");
	succeed_if (referenceName == 0, "reference name not correct");
	free (pluginName); pluginName = 0;

	keySetName (k, "system/e/#2dump");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 1, "process plugin error");
	succeed_if (pluginNumber == 2, "number not correct");
	succeed_if (!strcmp(pluginName, "dump"), "plugin name not correct");
	succeed_if (referenceName == 0, "reference name not correct");
	free (pluginName); pluginName = 0;

	keySetName (k, "system/e/#9tracer");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 1, "process plugin error");
	succeed_if (pluginNumber == 9, "number not correct");
	succeed_if (!strcmp(pluginName, "tracer"), "plugin name not correct");
	succeed_if (referenceName == 0, "reference name not correct");
	free (pluginName); pluginName = 0;

	keySetName (k, "system/e/1tracer");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == -1, "should be error");

	keySetName (k, "system/e/#xtracer");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == -1, "should be error");

	keySetName (k, "system/e/#1#name");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 2, "process plugin error");
	succeed_if (pluginNumber == 1, "number not correct");
	succeed_if (pluginName == 0, "plugin name not correct");
	succeed_if (!strcmp(referenceName, "system/elektra/plugins/name"), "reference name not correct");
	free (referenceName); referenceName = 0;

	keySetName (k, "system/e/#5#dump");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 2, "process plugin error");
	succeed_if (pluginNumber == 5, "number not correct");
	succeed_if (pluginName == 0, "plugin name not correct");
	succeed_if (!strcmp(referenceName, "system/elektra/plugins/dump"), "reference name not correct");
	free (referenceName); referenceName = 0;

	keySetName (k, "system/e/#0#very_long_name with space");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 2, "process plugin error");
	succeed_if (pluginNumber == 0, "number not correct");
	succeed_if (pluginName == 0, "plugin name not correct");
	succeed_if (!strcmp(referenceName, "system/elektra/plugins/very_long_name with space"), "reference name not correct");
	free (referenceName); referenceName = 0;

	keySetName (k, "system/e/#1#plugname#refname#");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 3, "process plugin error");
	succeed_if (pluginNumber == 1, "number not correct");
	succeed_if (!strcmp(pluginName, "plugname"), "plugin name not correct");
	succeed_if (!strcmp(referenceName, "system/elektra/plugins/refname"), "reference name not correct");
	free (pluginName); pluginName = 0;
	free (referenceName); referenceName = 0;

	keySetName (k, "system/e/#0#dump#dumpy#");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 3, "process plugin error");
	succeed_if (pluginNumber == 0, "number not correct");
	succeed_if (!strcmp(pluginName, "dump"), "plugin name not correct");
	succeed_if (!strcmp(referenceName, "system/elektra/plugins/dumpy"), "reference name not correct");
	free (pluginName); pluginName = 0;
	free (referenceName); referenceName = 0;

	keySetName (k, "system/e/#9#tracer#tracer#");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 3, "process plugin error");
	succeed_if (pluginNumber == 9, "number not correct");
	succeed_if (!strcmp(pluginName, "tracer"), "plugin name not correct");
	succeed_if (!strcmp(referenceName, "system/elektra/plugins/tracer"), "reference name not correct");
	free (pluginName); pluginName = 0;
	free (referenceName); referenceName = 0;

	keySetName (k, "system/e/#8#a_very long name with $ sthg#also a long name_()#");
	succeed_if (elektraProcessPlugin(k, &pluginNumber, &pluginName, &referenceName) == 3, "process plugin error");
	succeed_if (pluginNumber == 8, "number not correct");
	succeed_if (!strcmp(pluginName, "a_very long name with $ sthg"), "plugin name not correct");
	succeed_if (!strcmp(referenceName, "system/elektra/plugins/also a long name_()"), "reference name not correct");
	free (pluginName); pluginName = 0;
	free (referenceName); referenceName = 0;

	keyDel (k);
}

KeySet *set_pluginconf()
{
	return ksNew( 10 ,
		keyNew ("system/anything", KEY_VALUE, "backend", KEY_END),
		keyNew ("system/more", KEY_END),
		keyNew ("system/more/config", KEY_END),
		keyNew ("system/more/config/below", KEY_END),
		keyNew ("system/path", KEY_END),
		keyNew ("user/anything", KEY_VALUE, "plugin", KEY_END),
		keyNew ("user/more", KEY_END),
		keyNew ("user/more/config", KEY_END),
		keyNew ("user/more/config/below", KEY_END),
		keyNew ("user/path", KEY_END),
		KS_END);
}

void test_simple()
{
	printf ("Test plugin\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit (modules, 0);

	Plugin *plugin = elektraPluginOpen("tracer", modules, set_pluginconf());

	KeySet *test_config = set_pluginconf();
	KeySet *config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset(config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");

	succeed_if (!strcmp(plugin->name, "tracer"), "got wrong name");

	elektraPluginClose(plugin);
	elektraModulesClose(modules, 0);
	ksDel (modules);
}

int main(int argc, char** argv)
{
	printf(" PLUGINS  TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_process();
	test_simple();

	printf("\ntest_plugin RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

