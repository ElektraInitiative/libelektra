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
	Key *k = keyNew ("system/elektra/#0name");
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

int main(int argc, char** argv)
{
	printf(" PLUGINS  TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_process();

	printf("\ntest_plugin RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

