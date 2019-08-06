/**
 * @file
 *
 * @brief Tests for the ini plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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


static void test_plainIniRead (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-read", KEY_VALUE, srcdir_file (fileName), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	Key * key = ksLookupByName (ks, "user/tests/ini-read/nosectionkey", KDB_O_NONE);
	exit_if_fail (key, "nosectionkey not found");
	succeed_if (!strcmp ("nosectionvalue", keyString (key)), "nosectionkey contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-read/section1", KDB_O_NONE);
	exit_if_fail (key, "section1 not found");

	key = ksLookupByName (ks, "user/tests/ini-read/section1/key1", KDB_O_NONE);
	exit_if_fail (key, "key1 not found");
	succeed_if (!strcmp ("value1", keyString (key)), "key1 contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-read/section2/emptykey", KDB_O_NONE);
	exit_if_fail (key, "emptykey not found");
	succeed_if (!strcmp ("", keyString (key)), "emptykey contained invalid data");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_plainIniWrite (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (30, keyNew ("user/tests/ini-write/nosectionkey", KEY_VALUE, "nosectionvalue", KEY_END),
			     keyNew ("user/tests/ini-write/section1", KEY_META, "internal/ini/section", "", KEY_END),
			     keyNew ("user/tests/ini-write/section1/key1", KEY_VALUE, "value1", KEY_END),
			     keyNew ("user/tests/ini-write/section1/key2", KEY_VALUE, "value2", KEY_END),
			     keyNew ("user/tests/ini-write/section2", KEY_META, "internal/ini/section", "", KEY_END),
			     keyNew ("user/tests/ini-write/section2/key3", KEY_VALUE, "value3", KEY_END),
			     keyNew ("user/tests/ini-write/section2/emptykey", KEY_END), KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	keyDel (parentKey);
	ksDel (ks);

	PLUGIN_CLOSE ();
}

static void test_plainIniEmptyWrite (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (30, keyNew ("user/tests/ini-write/nosectionkey", KEY_VALUE, "nosectionvalue", KEY_END),
			     keyNew ("user/tests/ini-write/section1", KEY_META, "internal/ini/section", "",

				     KEY_END),
			     keyNew ("user/tests/ini-write/section1/key1", KEY_VALUE, "value1", KEY_END),
			     keyNew ("user/tests/ini-write/section1/key2", KEY_VALUE, "value2", KEY_END),
			     keyNew ("user/tests/ini-write/section2", KEY_META, "internal/ini/section", "", KEY_END),
			     keyNew ("user/tests/ini-write/section2/key3", KEY_VALUE, "value3", KEY_END),
			     keyNew ("user/tests/ini-write/section2/emptykey", KEY_META, "internal/ini/empty", "", KEY_END), KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	KeySet * readKS = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, readKS, parentKey) >= 0, "kdbGet failed");
	const Key * meta;
	Key * searchKey = keyNew ("user/tests/ini-write/section2/emptykey", KEY_META, "internal/ini/empty", "", KEY_END);
	Key * key = ksLookup (readKS, searchKey, KDB_O_NONE);
	meta = keyGetMeta (key, "internal/ini/empty");
	succeed_if (meta != NULL, "reading empty key again failed");
	ksDel (readKS);
	keyDel (parentKey);
	keyDel (searchKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}
static void test_commentIniRead (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-read", KEY_VALUE, srcdir_file (fileName), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	Key * key = ksLookupByName (ks, "user/tests/ini-read/nosectionkey", KDB_O_NONE);
	exit_if_fail (key, "nosectionkey not found");
	const Key * noSectionComment = keyGetMeta (key, "comments/#0");
	exit_if_fail (noSectionComment, "nosectionkey contained no comment");
	succeed_if (!strcmp (";nosection comment1", keyString (noSectionComment)), "nosectionkey contained an invalid comment");
	noSectionComment = keyGetMeta (key, "comments/#1");
	exit_if_fail (noSectionComment, "nosectionkey contained no comment");
	succeed_if (!strcmp (";nosection comment2", keyString (noSectionComment)), "nosectionkey contained an invalid comment");


	key = ksLookupByName (ks, "user/tests/ini-read/section1", KDB_O_NONE);
	exit_if_fail (key, "section1 not found");
	const Key * sectionComment = keyGetMeta (key, "comments/#0");
	exit_if_fail (sectionComment, "sectionkey contained no comment");
	succeed_if (!strcmp (";section comment1", keyString (sectionComment)), "sectionkey contained an invalid comment");
	sectionComment = keyGetMeta (key, "comments/#1");
	exit_if_fail (sectionComment, "sectionkey contained no comment");
	succeed_if (!strcmp (";section comment2", keyString (sectionComment)), "sectionkey contained an invalid comment");


	key = ksLookupByName (ks, "user/tests/ini-read/section1/key1", KDB_O_NONE);
	exit_if_fail (key, "key1 not found");
	const Key * keyComment_ = keyGetMeta (key, "comments/#0");
	exit_if_fail (keyComment_, "key1 contained no comment");
	succeed_if (!strcmp (";key comment1", keyString (keyComment_)), "key1 contained an invalid comment");
	keyComment_ = keyGetMeta (key, "comments/#1");
	exit_if_fail (keyComment_, "key1 contained no comment");
	succeed_if (!strcmp (";key comment2", keyString (keyComment_)), "key1 contained an invalid comment");


	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_commentIniWrite (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (30,
			     keyNew ("user/tests/ini-write/nosectionkey", KEY_VALUE, "nosectionvalue", KEY_META, "comments", "#1", KEY_META,
				     "comments/#0", ";nosection comment1", KEY_META, "comments/#1", ";nosection comment2", KEY_END),
			     keyNew ("user/tests/ini-write/section1", KEY_META, "internal/ini/section", "", KEY_META, "comments", "#1",
				     KEY_META, "comments/#0", ";section comment1", KEY_META, "comments/#1", ";section comment2", KEY_END),
			     keyNew ("user/tests/ini-write/section1/key1", KEY_VALUE, "value1", KEY_META, "comments", "#1", KEY_META,
				     "comments/#0", ";key comment1", KEY_META, "comments/#1", ";key comment2", KEY_END),
			     KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_multilineIniRead (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-multiline-read", KEY_VALUE, srcdir_file (fileName), KEY_END);

	KeySet * conf = ksNew (30, keyNew ("system/multiline", KEY_VALUE, "1", KEY_END),
			       keyNew ("system/linecont", KEY_VALUE, "\t", KEY_END), KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	Key * key = ksLookupByName (ks, "user/tests/ini-multiline-read/multilinesection/key1", KDB_O_NONE);
	exit_if_fail (key, "key1 not found");
	succeed_if (!strcmp ("value1\nwith continuation\nlines", keyString (key)), "key1 contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-multiline-read/singlelinesection/key2", KDB_O_NONE);
	exit_if_fail (key, "key2 not found");
	succeed_if (!strcmp ("", keyString (key)), "key2 contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-multiline-read/singlelinesection/key3", KDB_O_NONE);
	exit_if_fail (key, "key3 not found");
	succeed_if (!strcmp ("value3", keyString (key)), "key3 contained invalid data");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_multilineIniWrite (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-multiline-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (30, keyNew ("system/multiline", KEY_VALUE, "1", KEY_END),
			       keyNew ("system/linecont", KEY_VALUE, "\t", KEY_END), KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (
		30, keyNew ("user/tests/ini-multiline-write/multilinesection", KEY_META, "internal/ini/section", "", KEY_END),
		keyNew ("user/tests/ini-multiline-write/multilinesection/key1", KEY_VALUE, "value1\nwith continuation\nlines", KEY_END),
		keyNew ("user/tests/ini-multiline-write/singlelinesection", KEY_META, "internal/ini/section", "", KEY_END),
		keyNew ("user/tests/ini-multiline-write/singlelinesection/key2", KEY_VALUE, "", KEY_END),
		keyNew ("user/tests/ini-multiline-write/singlelinesection/key3", KEY_VALUE, "value3", KEY_END), KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}
static void test_multilineIniInvalidConfigWrite (void)
{
	Key * parentKey = keyNew ("user/tests/ini-multiline-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (30, keyNew ("system/multiline", KEY_VALUE, "0", KEY_END),
			       keyNew ("system/linecont", KEY_VALUE, "\t", KEY_END), KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (
		30, keyNew ("user/tests/ini-multiline-write/multilinesection", KEY_META, "internal/ini/section", "", KEY_END),
		keyNew ("user/tests/ini-multiline-write/multilinesection/key1", KEY_VALUE, "value1\nwith continuation\nlines", KEY_END),
		KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) < 0, "call to kdbSet was successful, but should fail");

	const Key * metaError = keyGetMeta (parentKey, "error");
	exit_if_fail (metaError, "No error was produced on the parentKey");

	succeed_if (!strcmp (keyString (keyGetMeta (parentKey, "error/number")), ELEKTRA_ERROR_INSTALLATION),
		    "The plugin threw the wrong error");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_sectionRead (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-section-read", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	Key * key = ksLookupByName (ks, "user/tests/ini-section-read/akey/looking/like/sections", KDB_O_NONE);
	exit_if_fail (key, "section like key not found not found");
	succeed_if (!strcmp ("value", keyString (key)), "section like key contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-section-read/emptysection", KDB_O_NONE);
	exit_if_fail (key, "empty section key not found");
	succeed_if (keyGetMeta (key, "internal/ini/section"), "empty section key is not a section key");

	key = ksLookupByName (ks, "user/tests/ini-section-read/section1", KDB_O_NONE);
	exit_if_fail (key, "section1 key not found");
	succeed_if (keyGetMeta (key, "internal/ini/section"), "section1 key is not a section key");

	key = ksLookupByName (ks, "user/tests/ini-section-read/section1/key1", KDB_O_NONE);
	exit_if_fail (key, "key1 not found not found");
	succeed_if (!strcmp ("value1", keyString (key)), "key1 contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-section-read/section1/key/with/subkey", KDB_O_NONE);
	exit_if_fail (key, "key with subkey not found not found");
	succeed_if (!strcmp ("value2", keyString (key)), "key with subkey contained invalid data");

	key = ksLookupByName (ks, "user/tests/ini-section-read/section2/with/subkey", KDB_O_NONE);
	exit_if_fail (key, "section2 key not found");
	succeed_if (keyGetMeta (key, "internal/ini/section"), "section2 key is not a section key");

	key = ksLookupByName (ks, "user/tests/ini-section-read/section2/with/subkey/key2", KDB_O_NONE);
	exit_if_fail (key, "key2 not found not found");
	succeed_if (!strcmp ("value2", keyString (key)), "key2 contained invalid data");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_sectionWrite (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-section-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system/section", KEY_VALUE, "NULL", KEY_END), KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (30, keyNew ("user/tests/ini-section-write/akey/looking/like/sections", KEY_VALUE, "value", KEY_END),
			     keyNew ("user/tests/ini-section-write/emptysection", KEY_META, "internal/ini/section", "", KEY_END),
			     keyNew ("user/tests/ini-section-write/section1", KEY_META, "internal/ini/section", "", KEY_END),
			     keyNew ("user/tests/ini-section-write/section1/key1", KEY_VALUE, "value1", KEY_END),
			     keyNew ("user/tests/ini-section-write/section1/key/with/subkey", KEY_VALUE, "value2", KEY_END),
			     keyNew ("user/tests/ini-section-write/section2/with/subkey", KEY_META, "internal/ini/section", "", KEY_END),
			     keyNew ("user/tests/ini-section-write/section2/with/subkey/key2", KEY_VALUE, "value2", KEY_END), KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_emptySectionBug (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (30, keyNew ("user/tests/ini-write/MyApp/mykey", KEY_VALUE, "new_value", KEY_END),
			     keyNew ("user/tests/ini-write/binarytest", KEY_META, "internal/ini/section", "", KEY_END),
			     keyNew ("user/tests/ini-write/debienna/test", KEY_VALUE, "value", KEY_END), KS_END);

	PLUGIN_OPEN ("ini");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_sectionMerge (char * inFile, char * cmpFile)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, srcdir_file (inFile), KEY_END);
	Key * writeParentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system/mergesections", KEY_VALUE, "1", KEY_END), KS_END);
	KeySet * ks = ksNew (30, KS_END);
	PLUGIN_OPEN ("ini");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");
	keyDel (ksLookup (ks, parentKey, KDB_O_POP));
	keyDel (parentKey);
	succeed_if (plugin->kdbSet (plugin, ks, writeParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (compare_line_files (srcdir_file (cmpFile), keyString (writeParentKey)), "files do not match as expected");
	keyDel (ksLookup (ks, writeParentKey, KDB_O_POP));
	keyDel (writeParentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_array (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-read", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system/array", KEY_VALUE, "1", KEY_END), KS_END);
	KeySet * ks = ksNew (30, KS_END);
	PLUGIN_OPEN ("ini");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");
	Key * lookupKey;
	lookupKey = ksLookupByName (ks, "user/tests/ini-read/sec/a/#0", KDB_O_NONE);
	succeed_if (!strcmp (keyString (lookupKey), "1"), "key sec/a/#0 has the wrong value");
	lookupKey = ksLookupByName (ks, "user/tests/ini-read/sec/a/#3", KDB_O_NONE);
	succeed_if (!strcmp (keyString (lookupKey), "4"), "key sec/a/#3 has the wrong value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_preserveEmptyLines (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, srcdir_file (fileName), KEY_END);
	Key * writeParentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (30, KS_END);
	PLUGIN_OPEN ("ini");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");
	keyDel (ksLookup (ks, parentKey, KDB_O_POP));
	keyDel (parentKey);
	succeed_if (plugin->kdbSet (plugin, ks, writeParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (compare_line_files (srcdir_file (fileName), keyString (writeParentKey)), "files do not match as expected");
	keyDel (ksLookup (ks, writeParentKey, KDB_O_POP));
	keyDel (writeParentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_insertOrder (char * source, char * compare)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, srcdir_file (source), KEY_END);
	Key * writeParentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (30, KS_END);
	KeySet * appendKS = ksNew (10, keyNew ("user/tests/ini-write/1", KEY_META, "internal/ini/section", "", KEY_END),
				   keyNew ("user/tests/ini-write/1/testkey1_0", KEY_VALUE, "testval1_0", KEY_END),
				   keyNew ("user/tests/ini-write/1/testkey1_1", KEY_VALUE, "testval1_1", KEY_END), KS_END);

	PLUGIN_OPEN ("ini");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");
	keyDel (ksLookup (ks, parentKey, KDB_O_POP));
	keyDel (parentKey);
	ksAppend (ks, appendKS);
	succeed_if (plugin->kdbSet (plugin, ks, writeParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (compare_line_files (srcdir_file (compare), keyString (writeParentKey)), "files do not match as expected");
	keyDel (ksLookup (ks, writeParentKey, KDB_O_POP));
	keyDel (writeParentKey);
	ksDel (appendKS);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_complexInsert (char * source, char * compare)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, srcdir_file (source), KEY_END);
	Key * writeParentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (30, KS_END);
	KeySet * appendKS = ksNew (10, keyNew ("user/tests/ini-write/section/subsection", KEY_META, "internal/ini/section", "", KEY_END),
				   keyNew ("user/tests/ini-write/section/subsection/subkey", KEY_VALUE, "subval", KEY_END),
				   keyNew ("user/tests/ini-write/section/zkey3", KEY_VALUE, "3", KEY_END), KS_END);

	PLUGIN_OPEN ("ini");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");
	keyDel (ksLookup (ks, parentKey, KDB_O_POP));
	keyDel (parentKey);
	ksAppend (ks, appendKS);
	succeed_if (plugin->kdbSet (plugin, ks, writeParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (compare_line_files (srcdir_file (compare), keyString (writeParentKey)), "files do not match as expected");
	keyDel (ksLookup (ks, writeParentKey, KDB_O_POP));
	keyDel (writeParentKey);
	ksDel (appendKS);
	ksDel (ks);
	PLUGIN_CLOSE ();
}
static void test_arrayInsert (char * source, char * compare)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, srcdir_file (source), KEY_END);
	Key * writeParentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system/array", KEY_VALUE, "1", KEY_END), KS_END);
	KeySet * ks = ksNew (30, KS_END);
	KeySet * appendKS = ksNew (10, keyNew ("user/tests/ini-write/a/section/array/#0", KEY_VALUE, "0", KEY_END),
				   keyNew ("user/tests/ini-write/a/section/array/#1", KEY_VALUE, "1", KEY_END),
				   keyNew ("user/tests/ini-write/a/section/array/#2", KEY_VALUE, "2", KEY_END),
				   keyNew ("user/tests/ini-write/a/section/array/#3", KEY_VALUE, "3", KEY_END), KS_END);

	PLUGIN_OPEN ("ini");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");
	keyDel (ksLookup (ks, parentKey, KDB_O_POP));
	keyDel (parentKey);
	ksAppend (ks, appendKS);
	succeed_if (plugin->kdbSet (plugin, ks, writeParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (compare_line_files (srcdir_file (compare), keyString (writeParentKey)), "files do not match as expected");
	keyDel (ksLookup (ks, writeParentKey, KDB_O_POP));
	keyDel (writeParentKey);
	ksDel (appendKS);
	ksDel (ks);
	PLUGIN_CLOSE ();
}
static void test_readCommentMeta (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-read", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (30, KS_END);
	PLUGIN_OPEN ("ini");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");
	Key * lookupKey;
	lookupKey = ksLookupByName (ks, "user/tests/ini-read/section", KDB_O_NONE);
	succeed_if (!strcmp (keyString (keyGetMeta (lookupKey, "sectionmeta")), "sectionvalue"), "failed reading section meta comment");
	lookupKey = ksLookupByName (ks, "user/tests/ini-read/section/key2", KDB_O_NONE);
	succeed_if (!strcmp (keyString (keyGetMeta (lookupKey, "key2meta")), "keyvalue"), "failed reading section meta comment");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_writeMeta (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (30, keyNew ("user/tests/ini-write/key", KEY_VALUE, "value", KEY_META, "METAKEY", "METAVALUE", KEY_END),
			     keyNew ("user/tests/ini-write/section/key2", KEY_VALUE, "value2", KEY_META, "METAKEY2", "METAVALUE2", KEY_END),

			     KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	keyDel (parentKey);
	ksDel (ks);

	PLUGIN_CLOSE ();
}


static void test_commentDefaultChar (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system/comment", KEY_VALUE, ";", KEY_END), KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (30,
			     keyNew ("user/tests/ini-write/nosectionkey", KEY_VALUE, "nosectionvalue", KEY_META, "comments", "#1", KEY_META,
				     "comments/#0", "nosection comment1", KEY_META, "comments/#1", "nosection comment2", KEY_END),
			     keyNew ("user/tests/ini-write/section1", KEY_BINARY, KEY_META, "comments", "#1", KEY_META, "comments/#0",
				     "section comment1", KEY_META, "comments/#1", "section comment2", KEY_END),
			     keyNew ("user/tests/ini-write/section1/key1", KEY_VALUE, "value1", KEY_META, "comments", "#1", KEY_META,
				     "comments/#0", "key comment1", KEY_META, "comments/#1", "key comment2", KEY_END),
			     KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_dontquotebracketvalues (char * fileName)
{
	Key * parentKey = keyNew ("user/tests/ini-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ini");

	KeySet * ks = ksNew (30, keyNew ("user/tests/ini-write/key", KEY_VALUE, "[1, 2, 3, 4, 5, 6]", KEY_END), KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	keyDel (parentKey);
	ksDel (ks);

	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("INI         TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_plainIniRead ("ini/plainini");
	test_plainIniWrite ("ini/plainini");
	test_plainIniRead ("ini/emptyval");
	test_plainIniEmptyWrite ("ini/emptyval");
	test_commentIniRead ("ini/commentini");
	test_commentIniWrite ("ini/commentini");
	test_multilineIniRead ("ini/multilineini");
	test_multilineIniWrite ("ini/multilineini");
	test_multilineIniInvalidConfigWrite ();
	test_sectionRead ("ini/sectionini");
	test_sectionWrite ("ini/sectionini");
	test_emptySectionBug ("ini/emptySectionBugTest");
	test_sectionMerge ("ini/sectionmerge.input", "ini/sectionmerge.output");
	test_array ("ini/array.ini");
	test_preserveEmptyLines ("ini/emptyLines");
	test_insertOrder ("ini/insertTest.input.ini", "ini/insertTest.output.ini");
	test_complexInsert ("ini/complexIn.ini", "ini/complexOut.ini");
	test_arrayInsert ("ini/arrayInsertIn.ini", "ini/arrayInsertOut.ini");
	test_dontquotebracketvalues ("ini/bracketQuoteOut.ini");
	test_commentDefaultChar ("ini/commentini");
	test_readCommentMeta ("ini/testCommentMeta.ini");
	test_writeMeta ("ini/writeCommentMeta.ini");
	print_result ("test_ini");

	return nbError;
}
