/**
 * \file
 *
 * \brief Tests for the ini plugin
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests_plugin.h>


void test_plainIniRead(char *fileName)
{
	Key *parentKey = keyNew ("user/tests/ini-read", KEY_VALUE,
			srcdir_file(fileName), KEY_END);

	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet *ks = ksNew(0, KS_END);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	succeed_if(output_error (parentKey), "error in kdbGet");
	succeed_if(output_warnings (parentKey), "warnings in kdbGet");

	Key *key = ksLookupByName (ks, "user/tests/ini-read/nosectionkey", KDB_O_NONE);
	exit_if_fail(key, "nosectionkey not found");
	succeed_if (!strcmp ("nosectionvalue", keyString(key)), "nosectionkey contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-read/section1", KDB_O_NONE);
	exit_if_fail(key, "section1 not found");
	succeed_if (keyIsDir(key), "section1 is not a directory key");
	succeed_if (!strcmp ("", keyString(key)), "section value was not empty");

	key = ksLookupByName (ks, "user/tests/ini-read/section1/key1", KDB_O_NONE);
	exit_if_fail(key, "key1 not found");
	succeed_if (!strcmp ("value1", keyString(key)), "key1 contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-read/section2/emptykey", KDB_O_NONE);
	exit_if_fail(key, "emptykey not found");
	succeed_if (!strcmp ("", keyString(key)), "emptykey contained invalid data");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ()
	;
}

void test_plainIniWrite(char *fileName)
{
	Key *parentKey = keyNew ("user/tests/ini-write", KEY_VALUE,
			elektraFilename(), KEY_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("ini");

	KeySet *ks = ksNew (30,
			keyNew ("user/tests/ini-write/nosectionkey",
					KEY_VALUE, "nosectionvalue",
					KEY_END),
			keyNew ("user/tests/ini-write/section1", KEY_DIR, KEY_END),
			keyNew ("user/tests/ini-write/section1/key1",
					KEY_VALUE, "value1",
					KEY_END),
			keyNew ("user/tests/ini-write/section1/key2",
					KEY_VALUE, "value2",
					KEY_END),
			keyNew ("user/tests/ini-write/section2", KEY_DIR, KEY_END),
			keyNew ("user/tests/ini-write/section2/key3",
					KEY_VALUE, "value3",
					KEY_END),
			keyNew ("user/tests/ini-write/section2/emptykey", KEY_END),
			KS_END);

	succeed_if(plugin->kdbSet (plugin, ks, parentKey) >= 1,
			"call to kdbSet was not successful");
	succeed_if(output_error (parentKey), "error in kdbSet");
	succeed_if(output_warnings (parentKey), "warnings in kdbSet");

	succeed_if(
			compare_line_files (srcdir_file (fileName), keyString (parentKey)),
			"files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ()
	;
}


void test_commentIniRead(char *fileName)
{
	Key *parentKey = keyNew ("user/tests/ini-read", KEY_VALUE,
			srcdir_file(fileName), KEY_END);

	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet *ks = ksNew(0, KS_END);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	succeed_if(output_error (parentKey), "error in kdbGet");
	succeed_if(output_warnings (parentKey), "warnings in kdbGet");

	Key *key = ksLookupByName (ks, "user/tests/ini-read/nosectionkey", KDB_O_NONE);
	exit_if_fail(key, "nosectionkey not found");
	const Key *noSectionComment = keyGetMeta(key, "comment");
	exit_if_fail(noSectionComment, "nosectionkey contained no comment");
	succeed_if (!strcmp ("nosection comment1\nnosection comment2", keyString(noSectionComment)), "nosectionkey contained an invalid comment");

	key = ksLookupByName (ks, "user/tests/ini-read/section1", KDB_O_NONE);
	exit_if_fail(key, "section1 not found");
	const Key *sectionComment = keyGetMeta(key, "comment");
	exit_if_fail(sectionComment, "nosectionkey contained no comment");
	succeed_if (!strcmp ("section comment1\nsection comment2", keyString(sectionComment)), "section1 contained an invalid comment");

	key = ksLookupByName (ks, "user/tests/ini-read/section1/key1", KDB_O_NONE);
	exit_if_fail(key, "key1 not found");
	const Key *keyComment = keyGetMeta(key, "comment");
	exit_if_fail(keyComment, "key1 contained no comment");
	succeed_if (!strcmp ("key comment1\nkey comment2", keyString(keyComment)), "key1 contained an invalid comment");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ()
	;
}

void test_commentIniWrite(char *fileName)
{
	Key *parentKey = keyNew ("user/tests/ini-write", KEY_VALUE,
			elektraFilename(), KEY_END);
	KeySet *conf = ksNew(0, KS_END);
	PLUGIN_OPEN("ini");

	KeySet *ks = ksNew (30,
			keyNew ("user/tests/ini-write/nosectionkey",
					KEY_VALUE, "nosectionvalue",
					KEY_COMMENT, "nosection comment1\nnosection comment2",
					KEY_END),
			keyNew ("user/tests/ini-write/section1",
					KEY_DIR,
					KEY_COMMENT, "section comment1\nsection comment2",
					KEY_END),
			keyNew ("user/tests/ini-write/section1/key1",
					KEY_VALUE, "value1",
					KEY_COMMENT, "key comment1\nkey comment2",
					KEY_END),
			KS_END);

	succeed_if(plugin->kdbSet (plugin, ks, parentKey) >= 1,
			"call to kdbSet was not successful");
	succeed_if(output_error (parentKey), "error in kdbSet");
	succeed_if(output_warnings (parentKey), "warnings in kdbSet");

	succeed_if(
			compare_line_files (srcdir_file (fileName), keyString (parentKey)),
			"files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ()
	;
}

int main(int argc, char** argv)
{
	printf ("INI       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_plainIniRead ("ini/plainini");
	test_plainIniWrite ("ini/plainini");
	test_commentIniRead ("ini/commentini");
	test_commentIniWrite ("ini/commentini");

	printf ("\ntest_hosts RESULTS: %d test(s) done. %d error(s).\n", nbTest,
			nbError);

	return nbError;
}

