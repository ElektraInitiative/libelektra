/*************************************************************************** 
 *  test_backendhelpers.c  - Test suite for helper functions of resolver
 *                  -------------------
 *  begin                : Fri 21 Mar 2008
 *  copyright            : (C) 2008 by Markus Raab
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


#include <langinfo.h>

#include <tests.h>
#include "resolver.h"

KeySet *set_pluginconf()
{
	return ksNew( 10 ,
		keyNew ("system/path", KEY_VALUE, "default.ecf", KEY_END),
		keyNew ("user/path", KEY_VALUE, "elektra.ecf", KEY_END),
		KS_END);
}


void test_resolve()
{
	printf ("Resolve Filename\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit (modules, 0);

	Plugin *plugin = elektraPluginOpen("resolver", modules, set_pluginconf(), 0);
	exit_if_fail (plugin, "could not load resolver plugin");

	KeySet *test_config = set_pluginconf();
	KeySet *config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset(config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");
	succeed_if (plugin->kdbError!= 0, "no open pointer");

	succeed_if (!strcmp(plugin->name, "resolver"), "got wrong name");

	resolverHandles *h = elektraPluginGetData(plugin);
	succeed_if (h != 0, "no plugin handle");
	resolverClose(&h->system);
	resolverClose(&h->user);

	Key *forKey = keyNew("system", KEY_END);
	succeed_if (resolveFilename(forKey, &h->system) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->system.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->system.filename, KDB_DB_SYSTEM "/elektra.ecf"), "resulting filename not correct");
	resolverClose(&h->system);


	keySetName(forKey, "user");
	succeed_if (resolveFilename(forKey, &h->user) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->user.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->user.filename, KDB_DB_HOME "/test/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
	resolverClose(&h->user);

	keySetMeta(forKey, "owner", "other");
	/* so that it will resolve the filename */
	succeed_if (resolveFilename(forKey, &h->user) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->user.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->user.filename, KDB_DB_HOME "/other/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
	resolverClose(&h->user);

	keyDel (forKey);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);
}

void test_name()
{
	printf ("Resolve Name\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit (modules, 0);

	Plugin *plugin = elektraPluginOpen("resolver", modules, set_pluginconf(), 0);
	exit_if_fail (plugin, "could not load resolver plugin");

	KeySet *test_config = set_pluginconf();
	KeySet *config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset(config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");
	succeed_if (plugin->kdbError!= 0, "no open pointer");

	succeed_if (!strcmp(plugin->name, "resolver"), "got wrong name");

	resolverHandles *h = elektraPluginGetData(plugin);
	succeed_if (h != 0, "no plugin handle");

	Key *parentKey= keyNew("system", KEY_END);
	plugin->kdbGet(plugin, 0, parentKey);
	succeed_if (!strcmp(keyString(parentKey), KDB_DB_SYSTEM "/elektra.ecf"),
			"resulting filename not correct");

	keySetName(parentKey, "user");
	plugin->kdbGet(plugin, 0, parentKey);
	succeed_if (!strcmp(keyString(parentKey), KDB_DB_HOME "/test/" KDB_DB_USER "/elektra.ecf"),
			"resulting filename not correct");

	keyDel (parentKey);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);
}

void test_lockname()
{
	printf ("Resolve Lockname\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit (modules, 0);

	Plugin *plugin = elektraPluginOpen("resolver", modules, set_pluginconf(), 0);
	exit_if_fail (plugin, "could not load resolver plugin");

	KeySet *test_config = set_pluginconf();
	KeySet *config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset(config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");
	succeed_if (plugin->kdbError!= 0, "no open pointer");

	succeed_if (!strcmp(plugin->name, "resolver"), "got wrong name");

	resolverHandles *h = elektraPluginGetData(plugin);
	succeed_if (h != 0, "no plugin handle");

	Key *parentKey= keyNew("system", KEY_END);
	plugin->kdbGet(plugin, 0, parentKey);
	succeed_if (!strcmp(h->system.lockfile, KDB_DB_SYSTEM "/elektra.ecf.lck"),
			"resulting filename not correct");

	keySetName(parentKey, "user");
	plugin->kdbGet(plugin, 0, parentKey);
	succeed_if (!strcmp(h->user.lockfile, KDB_DB_HOME "/test/" KDB_DB_USER "/elektra.ecf.lck"),
			"resulting filename not correct");

	keyDel (parentKey);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);
}

void test_tempname()
{
	printf ("Resolve Tempname\n");

	KeySet *modules = ksNew(0);
	elektraModulesInit (modules, 0);

	Plugin *plugin = elektraPluginOpen("resolver", modules, set_pluginconf(), 0);
	exit_if_fail (plugin, "could not load resolver plugin");

	KeySet *test_config = set_pluginconf();
	KeySet *config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset(config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");
	succeed_if (plugin->kdbError!= 0, "no open pointer");

	succeed_if (!strcmp(plugin->name, "resolver"), "got wrong name");

	resolverHandles *h = elektraPluginGetData(plugin);
	succeed_if (h != 0, "no plugin handle");

	Key *parentKey= keyNew("system", KEY_END);
	plugin->kdbGet(plugin, 0, parentKey);
	succeed_if (!strcmp(h->system.tempfile, KDB_DB_SYSTEM "/elektra.ecf.tmp"),
			"resulting filename not correct");

	keySetName(parentKey, "user");
	plugin->kdbGet(plugin, 0, parentKey);
	succeed_if (!strcmp(h->user.tempfile, KDB_DB_HOME "/test/" KDB_DB_USER "/elektra.ecf.tmp"),
			"resulting filename not correct");

	keyDel (parentKey);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);
}

void test_checkfile()
{
	succeed_if (elektraResolverCheckFile("valid") == 1, "valid file not recogniced");
	succeed_if (elektraResolverCheckFile("/valid") == 0, "valid absolute file not recogniced");
	succeed_if (elektraResolverCheckFile("/absolute/valid") == 0, "valid absolute file not recogniced");
	succeed_if (elektraResolverCheckFile("../valid") == -1, "invalid file not recogniced");
	succeed_if (elektraResolverCheckFile("valid/..") == -1, "invalid file not recogniced");
	succeed_if (elektraResolverCheckFile("/../valid") == -1, "invalid absolute file not recogniced");
	succeed_if (elektraResolverCheckFile("/valid/..") == -1, "invalid absolute file not recogniced");
	succeed_if (elektraResolverCheckFile("very..strict") == -1, "resolver is currently very strict");
	succeed_if (elektraResolverCheckFile("very/..strict") == -1, "resolver is currently very strict");
	succeed_if (elektraResolverCheckFile("very../strict") == -1, "resolver is currently very strict");
	succeed_if (elektraResolverCheckFile("very/../strict") == -1, "resolver is currently very strict");
	succeed_if (elektraResolverCheckFile("/") == -1, "invalid absolute file not recogniced");
	succeed_if (elektraResolverCheckFile(".") == -1, "invalid file not recogniced");
	succeed_if (elektraResolverCheckFile("..") == -1, "invalid file not recogniced");
}


int main(int argc, char** argv)
{
	printf("  RESOLVER  TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	putenv("USER=test");

	test_resolve();
	test_name();
	test_lockname();
	test_tempname();
	test_checkfile();


	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

