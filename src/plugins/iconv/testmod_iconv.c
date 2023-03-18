/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "conv.h"

#include <langinfo.h>

#include <tests_internal.h>

#define NR_KEYS 1

void test_latin1_to_utf8 (void)
{
	KeySet * latin1 = 0;
	KeySet * utf8 = 0;

	KeySet * conf =
		ksNew (2, keyNew ("user:/from", KEY_VALUE, "ISO8859-1", KEY_END), keyNew ("user:/to", KEY_VALUE, "UTF-8", KEY_END), KS_END);

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * parentKey = keyNew ("/", KEY_END);

	Plugin * plugin = elektraPluginOpen ("iconv", modules, conf, 0);

	exit_if_fail (plugin != 0, "could not open plugin");

	latin1 =
#include "data_latin1.c"
		utf8 =
#include "data_utf8.c"
			succeed_if (plugin->kdbSet (plugin, latin1, 0) == NR_KEYS, "not the correct number of keys");
	compare_keyset (latin1, utf8);
	ksDel (latin1);
	ksDel (utf8);

	latin1 =
#include "data_latin1.c"
		utf8 =
#include "data_utf8.c"
			succeed_if (plugin->kdbGet (plugin, utf8, parentKey) == NR_KEYS, "not the correct number of keys");
	compare_keyset (utf8, latin1);
	ksDel (latin1);
	ksDel (utf8);

	keyDel (parentKey);
	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

void test_utf8_to_latin1 (void)
{
	KeySet * latin1 = 0;
	KeySet * utf8 = 0;
	Key * parentKey = keyNew ("/", KEY_END);

	KeySet * conf =
		ksNew (2, keyNew ("user:/from", KEY_VALUE, "UTF-8", KEY_END), keyNew ("user:/to", KEY_VALUE, "ISO8859-1", KEY_END), KS_END);

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);


	Plugin * plugin = elektraPluginOpen ("iconv", modules, conf, 0);

	exit_if_fail (plugin != 0, "could not open plugin");

	latin1 =
#include "data_latin1.c"
		utf8 =
#include "data_utf8.c"
			succeed_if (plugin->kdbGet (plugin, latin1, parentKey) == NR_KEYS, "not the correct number of keys");
	compare_keyset (latin1, utf8);
	ksDel (latin1);
	ksDel (utf8);

	latin1 =
#include "data_latin1.c"
		utf8 =
#include "data_utf8.c"
			succeed_if (plugin->kdbSet (plugin, utf8, 0) == NR_KEYS, "not the correct number of keys");
	compare_keyset (utf8, latin1);
	ksDel (latin1);
	ksDel (utf8);

	keyDel (parentKey);
	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

void test_utf8_needed (void)
{
	printf ("Test if utf8 conversation is needed\n");
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * conf =
		ksNew (2, keyNew ("user:/from", KEY_VALUE, "UTF-8", KEY_END), keyNew ("user:/to", KEY_VALUE, "UTF-8", KEY_END), KS_END);


	Plugin * plugin = elektraPluginOpen ("iconv", modules, conf, 0);

	printf ("setlocale %s\n", setlocale (LC_CTYPE, ""));
	printf ("langinfo %s\n", nl_langinfo (CODESET));
	warn_if_fail (kdbbNeedsUTF8Conversion (plugin) == 0, "Your default needs conversation, use utf8 to avoid that");

	/*
	printf ("setlocale %s\n",setlocale (LC_CTYPE, "C"));
	printf ("langinfo %s\n", nl_langinfo(CODESET));
	warn_if_fail (kdbbNeedsUTF8Conversion(plugin) != 0, "C needs conversation");
	*/

	/*
	printf ("%s\n",setlocale (LC_CTYPE, "de_AT.utf8"));
	printf ("%s\n", nl_langinfo(CODESET));
	succeed_if (kdbbNeedsUTF8Conversion(plugin) == 0, "UTF-8 does not need conversation");
	*/

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

static void set_str (char ** str, size_t * len, char * newstr)
{
	*len = strlen (newstr) + 1;
	elektraRealloc ((void **) str, *len);
	strcpy (*str, newstr);
}

void test_utf8_conversation (void)
{
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * conf =
		ksNew (2, keyNew ("user:/from", KEY_VALUE, "UTF-8", KEY_END), keyNew ("user:/to", KEY_VALUE, "UTF-8", KEY_END), KS_END);


	Plugin * plugin = elektraPluginOpen ("iconv", modules, conf, 0);
	char * str = elektraMalloc (KDB_MAX_PATH_LENGTH);
	size_t len;

	printf ("Test utf8 conversation\n");

	printf ("setlocale %s\n", setlocale (LC_ALL, "C"));


	set_str (&str, &len, "only ascii");
	succeed_if (kdbbUTF8Engine (plugin, UTF8_FROM, &str, &len) != -1, "could not use utf8engine");
	succeed_if (strcmp ("only ascii", str) == 0, "ascii conversation incorrect");

	/* leads to EILSEQ, means illegal byte sequence */
	set_str (&str, &len, "Ug.ly:St@ri€n.g Key");
	/* succeed_if (kdbbUTF8Engine (plugin, UTF8_FROM, &str, &len) != -1, "could use utf8engine"); */
	/*succeed_if (errno == EILSEQ, "errno not set correctly");*/

	elektraFree (str);

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}


int main (int argc, char ** argv)
{
	printf ("   ICONV   TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_latin1_to_utf8 ();
	test_utf8_to_latin1 ();
	test_utf8_needed ();
	test_utf8_conversation ();

	print_result ("test_iconv");

	return nbError;
}
