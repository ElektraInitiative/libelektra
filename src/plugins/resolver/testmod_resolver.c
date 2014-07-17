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

#include <tests_internal.h>

#include <kdbinternal.h>

#include <langinfo.h>

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
	succeed_if (resolveFilename(forKey, &h->system, forKey) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->system.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->system.filename, KDB_DB_SYSTEM "/elektra.ecf"), "resulting filename not correct");
	resolverClose(&h->system);


	keySetName(forKey, "user");
	succeed_if (resolveFilename(forKey, &h->user, forKey) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->user.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->user.filename, "/tmp/elektra-test/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
	resolverClose(&h->user);

#ifdef HAVE_SETENV
	unsetenv("USER");
	unsetenv("HOME");
	keySetName(forKey, "system");
	succeed_if (resolveFilename(forKey, &h->system, forKey) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->system.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->system.filename, KDB_DB_SYSTEM "/elektra.ecf"), "resulting filename not correct");
	resolverClose(&h->system);


	keySetName(forKey, "user");
	succeed_if (resolveFilename(forKey, &h->user, forKey) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->user.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->user.filename, KDB_DB_HOME "/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
	resolverClose(&h->user);


	setenv("USER","other",1);
	succeed_if (resolveFilename(forKey, &h->user, forKey) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->user.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->user.filename, KDB_DB_HOME "/other/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
	resolverClose(&h->user);

	setenv("HOME","/nfshome//max//",1);
	succeed_if (resolveFilename(forKey, &h->user, forKey) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->user.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->user.filename, "/nfshome/max/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
	resolverClose(&h->user);
	unsetenv("HOME");
	unsetenv("USER");
#endif

	keySetName(forKey, "user");
	succeed_if (resolveFilename(forKey, &h->user, forKey) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->user.path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->user.filename, KDB_DB_HOME "/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
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
	succeed_if (!strcmp(keyString(parentKey), KDB_DB_HOME "/" KDB_DB_USER "/elektra.ecf"),
			"resulting filename not correct");

	keyDel (parentKey);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);
}

void test_lockname()
{
	printf ("Resolve Dirname\n");

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
	succeed_if (!strcmp(h->system.dirname, KDB_DB_SYSTEM),
			"resulting filename not correct");

	keySetName(parentKey, "user");
	plugin->kdbGet(plugin, 0, parentKey);
	succeed_if (!strcmp(h->user.dirname, KDB_DB_HOME "/" KDB_DB_USER),
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
	succeed_if (!strncmp(h->system.tempfile, KDB_DB_SYSTEM "/elektra.ecf", sizeof(KDB_DB_SYSTEM)),
			"resulting filename not correct");

	keySetName(parentKey, "user");
	plugin->kdbGet(plugin, 0, parentKey);
	succeed_if (!strncmp(h->user.tempfile, KDB_DB_HOME "/" KDB_DB_USER "/elektra.ecf.tmp", sizeof(KDB_DB_HOME "/" KDB_DB_USER)),
			"resulting filename not correct");

	keyDel (parentKey);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);
}

void test_checkfile()
{
	succeed_if (elektraResolverCheckFile("valid") == 1, "valid file not recognised");
	succeed_if (elektraResolverCheckFile("/valid") == 0, "valid absolute file not recognised");
	succeed_if (elektraResolverCheckFile("/absolute/valid") == 0, "valid absolute file not recognised");
	succeed_if (elektraResolverCheckFile("../valid") == -1, "invalid file not recognised");
	succeed_if (elektraResolverCheckFile("valid/..") == -1, "invalid file not recognised");
	succeed_if (elektraResolverCheckFile("/../valid") == -1, "invalid absolute file not recognised");
	succeed_if (elektraResolverCheckFile("/valid/..") == -1, "invalid absolute file not recognised");
	succeed_if (elektraResolverCheckFile("very..strict") == -1, "resolver is currently very strict");
	succeed_if (elektraResolverCheckFile("very/..strict") == -1, "resolver is currently very strict");
	succeed_if (elektraResolverCheckFile("very../strict") == -1, "resolver is currently very strict");
	succeed_if (elektraResolverCheckFile("very/../strict") == -1, "resolver is currently very strict");
	succeed_if (elektraResolverCheckFile("/") == -1, "invalid absolute file not recognised");
	succeed_if (elektraResolverCheckFile(".") == -1, "invalid file not recognised");
	succeed_if (elektraResolverCheckFile("..") == -1, "invalid file not recognised");
}


int main(int argc, char** argv)
{
	printf("  RESOLVER  TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_resolve();
	test_name();
	test_lockname();
	test_tempname();
	test_checkfile();


	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

