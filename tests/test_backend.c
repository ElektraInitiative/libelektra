/*************************************************************************** 
 *      test_backend.c  -  Test cases for how to build
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


KeySet *set_simple()
{
	return ksNew(50,
		keyNew("system/elektra/mountpoints/simple", KEY_END),

		keyNew("system/elektra/mountpoints/simple/config", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/more", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/more/config", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/more/config/below", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/path", KEY_END),

		keyNew("system/elektra/mountpoints/simple/getplugins", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/anything", KEY_VALUE, "plugin", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config/below", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/path", KEY_END),

		keyNew("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END),

		keyNew("system/elektra/mountpoints/simple/setplugins", KEY_END),
		keyNew("system/elektra/mountpoints/simple/setplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		KS_END);

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
	printf ("Test simple building of backend");

	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);

	Backend *backend = elektraBackendOpen(set_simple(), modules);
	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->getplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");

	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->setplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");

	Key *mp;
	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
	succeed_if (!strcmp(keyName(mp), "user/tests/backend/simple"), "wrong mountpoint for backend");
	succeed_if (!strcmp(keyString(mp), "simple"), "wrong name for backend");

	Plugin *plugin = backend->getplugins[1];

	KeySet *test_config = set_pluginconf();
	KeySet *config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset(config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");

	elektraBackendClose (backend);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

void test_default()
{
	printf ("Test default\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);


	Plugin *plugin = elektraPluginOpen("default", modules, set_pluginconf());

	KeySet *test_config = set_pluginconf();
	KeySet *config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset(config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");

	/*
	   Depends on the which is the plugin
	succeed_if (!strcmp(plugin->name, "tracer"), "got wrong name");
	succeed_if (!strcmp(plugin->author, "Markus Raab <elektra@markus-raab.org>"), "got wrong author");
	succeed_if (!strcmp(plugin->licence, "BSD"), "got wrong licence");
	succeed_if (!strcmp(plugin->description, "The first plugin"), "got wrong description");
	succeed_if (!strcmp(plugin->capability, ""), "got wrong capability (tracer can do nothing)");
	*/

	elektraPluginClose(plugin);

	Backend *backend = elektraBackendOpenDefault(modules);

	Key *mp;
	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
	succeed_if (!strcmp(keyName(mp), ""), "wrong mountpoint for default backend");
	succeed_if (!strcmp(keyString(mp), "default"), "wrong name for default backend");

	elektraBackendClose(backend);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

void test_trie()
{
	printf ("Test trie\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);

	KeySet *config = set_simple();
	ksAppendKey(config, keyNew("system/elektra/mountpoints", KEY_END));
	Trie *trie = elektraTrieOpen(config, modules);

	Key *key = keyNew("user/tests/backend/simple", KEY_END);
	Backend *backend = elektraTrieLookup(trie, key);

	keyAddBaseName(key, "somewhere"); keyAddBaseName(key, "deep"); keyAddBaseName(key, "below");
	Backend *backend2 = elektraTrieLookup(trie, key);
	succeed_if (backend == backend2, "should be same backend");

	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->getplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");

	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->setplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");

	Key *mp;
	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
	succeed_if (!strcmp(keyName(mp), "user/tests/backend/simple"), "wrong mountpoint for backend");
	succeed_if (!strcmp(keyString(mp), "simple"), "wrong name for backend");

	Plugin *plugin = backend->getplugins[1];

	KeySet *test_config = set_pluginconf();
	KeySet *cconfig = elektraPluginGetConfig (plugin);
	succeed_if (cconfig != 0, "there should be a config");
	compare_keyset(cconfig, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");

	elektraTrieClose(trie);
	keyDel (key);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}


KeySet *set_two()
{
	return ksNew(50,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/simple", KEY_END),

		keyNew("system/elektra/mountpoints/simple/config", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/more", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/more/config", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/more/config/below", KEY_END),
		keyNew("system/elektra/mountpoints/simple/config/path", KEY_END),

		keyNew("system/elektra/mountpoints/simple/getplugins", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/anything", KEY_VALUE, "plugin", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config/below", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1tracer/config/path", KEY_END),

		keyNew("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END),

		keyNew("system/elektra/mountpoints/simple/setplugins", KEY_END),
		keyNew("system/elektra/mountpoints/simple/setplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),


		keyNew("system/elektra/mountpoints/two", KEY_END),

		keyNew("system/elektra/mountpoints/two/config", KEY_END),
		keyNew("system/elektra/mountpoints/two/config/anything", KEY_VALUE, "backend", KEY_END),
		keyNew("system/elektra/mountpoints/two/config/more", KEY_END),
		keyNew("system/elektra/mountpoints/two/config/more/config", KEY_END),
		keyNew("system/elektra/mountpoints/two/config/more/config/below", KEY_END),
		keyNew("system/elektra/mountpoints/two/config/path", KEY_END),

		keyNew("system/elektra/mountpoints/two/getplugins", KEY_END),
		keyNew("system/elektra/mountpoints/two/getplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		keyNew("system/elektra/mountpoints/two/getplugins/#1tracer/config", KEY_END),
		keyNew("system/elektra/mountpoints/two/getplugins/#1tracer/config/anything", KEY_VALUE, "plugin", KEY_END),
		keyNew("system/elektra/mountpoints/two/getplugins/#1tracer/config/more", KEY_END),
		keyNew("system/elektra/mountpoints/two/getplugins/#1tracer/config/more/config", KEY_END),
		keyNew("system/elektra/mountpoints/two/getplugins/#1tracer/config/more/config/below", KEY_END),
		keyNew("system/elektra/mountpoints/two/getplugins/#1tracer/config/path", KEY_END),

		keyNew("system/elektra/mountpoints/two/mountpoint", KEY_VALUE, "user/tests/backend/two", KEY_END),

		keyNew("system/elektra/mountpoints/two/setplugins", KEY_END),
		keyNew("system/elektra/mountpoints/two/setplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		keyNew("system/elektra/mountpoints/two/setplugins/#2tracer", KEY_VALUE, "tracer", KEY_END),
		KS_END);
}

void test_two()
{
	printf ("Test trie two\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);

	KeySet *config = set_two();
	ksAppendKey(config, keyNew("system/elektra/mountpoints", KEY_END));
	Trie *trie = elektraTrieOpen(config, modules);

	Key *key = keyNew("user/tests/backend/simple", KEY_END);
	Backend *backend = elektraTrieLookup(trie, key);

	keyAddBaseName(key, "somewhere"); keyAddBaseName(key, "deep"); keyAddBaseName(key, "below");
	Backend *backend2 = elektraTrieLookup(trie, key);
	succeed_if (backend == backend2, "should be same backend");

	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->getplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");

	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->setplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");

	Key *mp;
	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
	succeed_if (!strcmp(keyName(mp), "user/tests/backend/simple"), "wrong mountpoint for backend");
	succeed_if (!strcmp(keyString(mp), "simple"), "wrong name for backend");

	Plugin *plugin = backend->getplugins[1];

	KeySet *test_config = set_pluginconf();
	KeySet *cconfig = elektraPluginGetConfig (plugin);
	succeed_if (cconfig != 0, "there should be a config");
	compare_keyset(cconfig, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");




	keySetName(key, "user/tests/backend/two");
	Backend *two = elektraTrieLookup(trie, key);
	succeed_if (two != backend, "should be differnt backend");

	succeed_if ((mp = two->mountpoint) != 0, "no mountpoint found");
	succeed_if (!strcmp(keyName(mp), "user/tests/backend/two"), "wrong mountpoint for backend two");
	succeed_if (!strcmp(keyString(mp), "two"), "wrong name for backend");

	elektraTrieClose(trie);
	keyDel (key);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

KeySet *set_backref()
{
	return ksNew(50,
		keyNew("system/elektra/mountpoints/backref", KEY_END),

		keyNew("system/elektra/mountpoints/backref/config", KEY_END),
		keyNew("system/elektra/mountpoints/backref/config/anything", KEY_VALUE, "backend", KEY_END),
		keyNew("system/elektra/mountpoints/backref/config/more", KEY_END),
		keyNew("system/elektra/mountpoints/backref/config/more/config", KEY_END),
		keyNew("system/elektra/mountpoints/backref/config/more/config/below", KEY_END),
		keyNew("system/elektra/mountpoints/backref/config/path", KEY_END),

		keyNew("system/elektra/mountpoints/backref/getplugins", KEY_END),
		keyNew("system/elektra/mountpoints/backref/getplugins/#1#tracer#tracer#", KEY_VALUE, "tracer", KEY_END),
		keyNew("system/elektra/mountpoints/backref/getplugins/#1#tracer#tracer#/config", KEY_END),
		keyNew("system/elektra/mountpoints/backref/getplugins/#1#tracer#tracer#/config/anything", KEY_VALUE, "plugin", KEY_END),
		keyNew("system/elektra/mountpoints/backref/getplugins/#1#tracer#tracer#/config/more", KEY_END),
		keyNew("system/elektra/mountpoints/backref/getplugins/#1#tracer#tracer#/config/more/config", KEY_END),
		keyNew("system/elektra/mountpoints/backref/getplugins/#1#tracer#tracer#/config/more/config/below", KEY_END),
		keyNew("system/elektra/mountpoints/backref/getplugins/#1#tracer#tracer#/config/path", KEY_END),

		keyNew("system/elektra/mountpoints/backref/mountpoint", KEY_VALUE, "user/tests/backend/backref", KEY_END),

		keyNew("system/elektra/mountpoints/backref/setplugins", KEY_END),
		keyNew("system/elektra/mountpoints/backref/setplugins/#1#tracer", KEY_VALUE, "reference to other tracer", KEY_END),
		KS_END);

}

void test_backref()
{
	printf ("Test back references\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);

	Backend *backend = elektraBackendOpen(set_backref(), modules);
	succeed_if (backend != 0, "there should be a backend");
	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->getplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");

	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->setplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");

	Key *mp;
	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
	succeed_if (!strcmp(keyName(mp), "user/tests/backend/backref"), "wrong mountpoint for backend");
	succeed_if (!strcmp(keyString(mp), "backref"), "wrong name for backend");

	Plugin *plugin1 = backend->getplugins[1];
	Plugin *plugin2 = backend->setplugins[1];

	succeed_if (plugin1 != 0, "there should be a plugin");
	succeed_if (plugin2 != 0, "there should be a plugin");
	succeed_if (plugin1 == plugin2, "it should be the same plugin");
	succeed_if (plugin1->refcounter == 2, "ref counter should be 2");

	KeySet *test_config = set_pluginconf();
	KeySet *config = elektraPluginGetConfig (plugin2);
	succeed_if (config != 0, "there should be a config");
	compare_keyset(config, test_config);
	ksDel (test_config);

	succeed_if (plugin1->kdbOpen != 0, "no open pointer");
	succeed_if (plugin2->kdbOpen != 0, "no open pointer");
	succeed_if (plugin2->kdbClose != 0, "no open pointer");
	succeed_if (plugin2->kdbGet != 0, "no open pointer");
	succeed_if (plugin2->kdbSet != 0, "no open pointer");

	elektraBackendClose (backend);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}



int main(int argc, char** argv)
{
	printf("  BACKEND   TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_simple();
	test_default();
	test_trie();
	test_two();
	test_backref();

	printf("\ntest_backend RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

