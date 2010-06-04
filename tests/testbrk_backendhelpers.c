/*************************************************************************** 
 *        test_backendhelpers.c  - Test suite for small plugins
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

/*Needs private declarations*/
#include <kdbbackend.h>

void test_calc_rel_filename()
{
	Key *k;
	char buffer [MAX_PATH_LENGTH];

	printf ("Test encoding\n");

	k = keyNew ("user/key", KEY_END);
	kdbbKeyCalcRelativeFilename (k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key\\", KEY_END);
	kdbbKeyCalcRelativeFilename (k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%5C") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key%", KEY_END);
	kdbbKeyCalcRelativeFilename (k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%25") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key+", KEY_END);
	kdbbKeyCalcRelativeFilename (k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%2B") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key ", KEY_END);
	kdbbKeyCalcRelativeFilename (k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key+") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key\\/", KEY_END);
	kdbbKeyCalcRelativeFilename (k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%5C%2F") == 0, "did not encode correctly with backslash");
	keyDel (k);

	k = keyNew ("user/key\\/ls\\\\tt", KEY_END);
	kdbbKeyCalcRelativeFilename (k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "user/key%5C%2Fls%5C%5Ctt") == 0, "did not encode correctly with backslash");
	keyDel (k);
}

void test_filename()
{
	Key *mnt;
	KeySet *conf;
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

	KDB *handle = kdbOpen();

	kdbMount (handle, mnt=keyNew("system/users", KEY_VALUE, "filesys", KEY_END),
		conf=ksNew (0));

	succeed_if (kdbbGetFullFilename(handle, i, buffer, MAX_PATH_LENGTH) == -1, "how did kdbbGetFullFilename found path for invalid key?");

	succeed_if (kdbbGetFullFilename(handle, k, buffer, 5) == -1, "case 1: not even /home/markus fits in buffer");

	succeed_if (kdbbGetFullFilename(handle, k, buffer, 16) == -1, "case 2: kdbbKeyCalcRelativeFilename");

	kdbbGetFullFilename(handle, k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "/home/max/.kdb/user/key") == 0, "step 4: got wrong filename without environment, no users");


#ifdef HAVE_SETENV
	setenv ("HOME","/home/owner",1);
#else
	putenv ("HOME=/home/owner");
#endif
	kdbbGetFullFilename(handle, k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "/home/owner/.kdb/user/key") == 0, "step 3: got wrong filename with environment HOME");

	kdbSetString (handle, "system/users/max/home", "/usr/homes");
	kdbbGetFullFilename(handle, k, buffer, MAX_PATH_LENGTH);
	// TODO failing
	// printf ("%s\n", buffer);
	// succeed_if (strcmp (buffer, "/usr/homes/.kdb/user/key") == 0, "step 2b: got wrong filename with home in elektra users db set");
	kdbRemove(handle, "system/users/max/home");

	kdbSetString (handle, "system/users/max/kdb", "/storage/kdb");
	kdbbGetFullFilename(handle, k, buffer, MAX_PATH_LENGTH);
	// TODO failing
	// printf ("%s\n", buffer);
	// succeed_if (strcmp (buffer, "/storage/kdb/user/key") == 0, "step 2a: got wrong filename with kdb in elektra users db set");
	kdbRemove(handle, "system/users/max/kdb");

#ifdef HAVE_SETENV
	setenv ("KDB_HOME","/home/else",1);
#else
	putenv("KDB_HOME=/home/else");
#endif
	kdbbGetFullFilename(handle, k, buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "/home/else/.kdb/user/key") == 0, "got wrong filename with environment KDB_HOME");

	keyDel (mnt);
	ksDel (conf);

	keyDel (k);
	keyDel (i);
	kdbClose (handle);
}

void test_filename_to_keyname()
{
	char buffer [MAX_PATH_LENGTH];

	printf ("Test filename to keyname\n");

	kdbbFilenameToKeyName("folder/key", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key") == 0, "did not encode correctly");

	kdbbFilenameToKeyName("folder/key+", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key ") == 0, "did not encode correctly");

	kdbbFilenameToKeyName("folder/key%5C", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key\\") == 0, "did not encode correctly");

	kdbbFilenameToKeyName("folder/key%25", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key%") == 0, "did not encode correctly");

	kdbbFilenameToKeyName("folder/key ", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key ") == 0, "did not encode correctly");

	kdbbFilenameToKeyName("folder/key\\", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key\\") == 0, "did not encode correctly");

	kdbbFilenameToKeyName("folder/key%5C%2F", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key\\/") == 0, "did not encode correctly");

	kdbbFilenameToKeyName("folder/key%5C+", buffer, MAX_PATH_LENGTH);
	succeed_if (strcmp (buffer, "folder/key\\ ") == 0, "did not encode correctly");
}

void test_keyname()
{
	Key *k = keyNew(0);
	Key *parentKey = keyNew ("user/absolute/reference", KEY_END);
	KDB *h = kdbOpen();

	printf ("Test keynames\n");

	kdbbGetFullKeyName(h, "folder/relative/key", parentKey, k);
	succeed_if (strcmp (keyName(k), "user/absolute/reference/folder/relative/key") == 0, "could not get correct keyname");

	kdbbGetFullKeyName(h, "folder/relative%5C%2Fkey%25+", parentKey, k);
	succeed_if (strcmp (keyName(k), "user/absolute/reference/folder/relative\\/key% ") == 0, "could not get correct keyname");

	keyDel (parentKey);
	keyDel (k);
	kdbClose (h);
}

void test_utf8_needed()
{
#if defined(HAVE_ICONV) && defined(HAVE_NL_LANGINFO) && defined(CODESET)
	printf ("Test if utf8 conversation is needed\n");

	printf ("setlocale %s\n",setlocale(LC_CTYPE,""));
	printf ("langinfo %s\n", nl_langinfo(CODESET));
	warn_if_fail (kdbbNeedsUTF8Conversion() == 0, "Your default needs conversation, use utf8 to avoid that");

	printf ("setlocale %s\n",setlocale (LC_CTYPE, "C"));
	printf ("langinfo %s\n", nl_langinfo(CODESET));
	warn_if_fail (kdbbNeedsUTF8Conversion() != 0, "C needs conversation (you maybe disabled iconv)");

	/*
	printf ("%s\n",setlocale (LC_CTYPE, "de_AT.utf8"));
	printf ("%s\n", nl_langinfo(CODESET));
	succeed_if (kdbbNeedsUTF8Conversion() == 0, "UTF-8 does not need conversation");
	*/
#else
	printf ("Dont test utf8 needed, iconv disabled\n");
	succeed_if (kdbbNeedsUTF8Conversion() == 0, "should never need to convert");
#endif
}

void set_str (char **str, size_t *len, char *newstr)
{
	*len = strlen (newstr)+1;
	elektraRealloc ((void**)str, *len);
	strcpy (*str, newstr);
}

void test_utf8_conversation()
{
#if defined(HAVE_ICONV) && defined(HAVE_NL_LANGINFO) && defined(CODESET)
	char * str = malloc (MAX_PATH_LENGTH);
	size_t len;

	printf ("Test utf8 conversation\n");

	printf ("setlocale %s\n",setlocale(LC_ALL,"C"));


	set_str (&str, &len, "only ascii");
	succeed_if (kdbbUTF8Engine (UTF8_FROM, &str, &len) != -1, "could not use utf8engine");
	succeed_if (strcmp ("only ascii", str) == 0, "ascii conversation incorrect");

	/* leads to EILSEQ, means illegal byte sequence */
	set_str (&str, &len, "Ug.ly:St@ri€n.g Key");
	succeed_if (kdbbUTF8Engine (UTF8_FROM, &str, &len) == -1, "could use utf8engine");
	/*succeed_if (errno == EILSEQ, "errno not set correctly");*/

	free (str);
#else
	printf ("Dont test utf8 conversations, iconv disabled\n");
#endif
}


int main(int argc, char** argv)
{
	printf("BACKENDHELPERS TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_utf8_needed();
	test_utf8_conversation();
	test_calc_rel_filename();
	test_filename();
	test_filename_to_keyname();
	test_keyname();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

