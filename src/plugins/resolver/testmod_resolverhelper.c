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

void test_calc_rel_filename()
{
	Key *k;
	char buffer [MAX_PATH_LENGTH];

	printf ("Test encoding\n");

	k = keyNew ("user/key", KEY_END);
	elektraKeyNameToRelativeFilename (keyName(k), buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key\\", KEY_END);
	elektraKeyNameToRelativeFilename (keyName(k), buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%5C") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key%", KEY_END);
	elektraKeyNameToRelativeFilename (keyName(k), buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%25") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key+", KEY_END);
	elektraKeyNameToRelativeFilename (keyName(k), buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%2B") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key ", KEY_END);
	elektraKeyNameToRelativeFilename (keyName(k), buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key+") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key\\/", KEY_END);
	elektraKeyNameToRelativeFilename (keyName(k), buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%5C%2F") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key\\/ls\\\\tt", KEY_END);
	elektraKeyNameToRelativeFilename (keyName(k), buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%5C%2Fls%5C%5Ctt") == 0, "did not encode correctly with backslash");
	keyDel (k);
}

void test_filename()
{
	Key *k = keyNew ("user/key", KEY_OWNER, "max", KEY_END);
	Key *i = keyNew (KEY_END);
	char buffer [MAX_PATH_LENGTH];

	printf ("Test filenames\n");

#ifdef HAVE_CLEARENV
	clearenv();
#else
	unsetenv("HOME");
	unsetenv("KDB_HOME");
#endif

	succeed_if (elektraGetFullFilename(i, buffer, MAX_PATH_LENGTH) == -1, "how did elektraGetFullFilename found path for invalid key?");

	succeed_if (elektraGetFullFilename(k, buffer, 5) == -1, "case 1: not even /home/markus fits in buffer");

	succeed_if (elektraGetFullFilename(k, buffer, 16) == -1, "case 2: elektraKeyNameToRelativeFilename");

	elektraGetFullFilename(k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "/home/max/.kdb/user/key") == 0, "step 4: got wrong filename without environment, no users");


#ifdef HAVE_SETENV
	setenv ("HOME","/home/owner",1);
#else
	putenv ("HOME=/home/owner");
#endif
	elektraGetFullFilename(k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "/home/owner/.kdb/user/key") == 0, "step 3: got wrong filename with environment HOME");

#ifdef HAVE_SETENV
	setenv ("KDB_HOME","/home/else",1);
#else
	putenv("KDB_HOME=/home/else");
#endif
	elektraGetFullFilename(k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "/home/else/.kdb/user/key") == 0, "got wrong filename with environment KDB_HOME");

	keyDel (k);
	keyDel (i);
}

void test_filename_to_keyname()
{
	char buffer [MAX_PATH_LENGTH];

	printf ("Test filename to keyname\n");

	elektraFilenameToKeyName("folder/key", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key") == 0, "did not encode correctly");

	elektraFilenameToKeyName("folder/key+", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key ") == 0, "did not encode correctly");

	elektraFilenameToKeyName("folder/key%5C", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key\\") == 0, "did not encode correctly");

	elektraFilenameToKeyName("folder/key%25", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key%") == 0, "did not encode correctly");

	elektraFilenameToKeyName("folder/key ", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key ") == 0, "did not encode correctly");

	elektraFilenameToKeyName("folder/key\\", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key\\") == 0, "did not encode correctly");

	elektraFilenameToKeyName("folder/key%5C%2F", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key\\/") == 0, "did not encode correctly");

	elektraFilenameToKeyName("folder/key%5C+", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key\\ ") == 0, "did not encode correctly");
}


int main(int argc, char** argv)
{
	printf("BACKENDHELPERS TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_calc_rel_filename();
	test_filename();
	test_keyname();
	test_filename_to_keyname();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

