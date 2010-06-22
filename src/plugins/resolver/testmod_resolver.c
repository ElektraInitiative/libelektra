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


void test_resolveFilename()
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

	succeed_if (!strcmp(plugin->name, "resolver"), "got wrong name");

	resolverHandle *h = elektraPluginGetHandle(plugin);
	succeed_if (h != 0, "no plugin handle");

	Key *forKey = keyNew("system", KEY_END);
	succeed_if (resolveFilename(forKey, elektraPluginGetHandle(plugin)) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->filename, KDB_DB_SYSTEM "/elektra.ecf"), "resulting filename not correct");
	succeed_if (!strcmp(h->systemFilename, KDB_DB_SYSTEM "/elektra.ecf"), "resulting filename not correct");

	keySetName(forKey, "user");
	succeed_if (resolveFilename(forKey, elektraPluginGetHandle(plugin)) == -1,
			"should fail because USER is not set");

	putenv("USER=test");

	succeed_if (resolveFilename(forKey, elektraPluginGetHandle(plugin)) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->filename, KDB_DB_HOME "/test/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
	succeed_if (!strcmp(h->userFilename, KDB_DB_HOME "/test/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");

	keySetMeta(forKey, "owner", "other");

	/* so that it will resolve the filename */
	free (h->userFilename); h->userFilename = 0;
	succeed_if (resolveFilename(forKey, elektraPluginGetHandle(plugin)) != -1,
			"could not resolve filename");

	succeed_if (!strcmp(h->path, "elektra.ecf"), "path not set correctly");
	succeed_if (!strcmp(h->filename, KDB_DB_HOME "/other/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");
	succeed_if (!strcmp(h->userFilename, KDB_DB_HOME "/other/" KDB_DB_USER "/elektra.ecf"), "filename not set correctly");

	keyDel (forKey);
	elektraPluginClose(plugin, 0);
	elektraModulesClose(modules, 0);
	ksDel (modules);
}


int main(int argc, char** argv)
{
	printf("  RESOLVER  TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_resolveFilename();


	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

