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

#define NR_KEYS 4

void test_latin1_to_utf8()
{
	KeySet *latin1 = 0;
	KeySet *utf8 = 0;

	KeySet *conf = ksNew (2,
			keyNew ("user/from", KEY_VALUE, "ISO8859-1", KEY_END),
			keyNew ("user/to", KEY_VALUE, "UTF-8", KEY_END),
			KS_END);


	Plugin *plugin = elektraPluginOpen("iconv", conf);

	plugin->kdbOpen(plugin);

	exit_if_fail (plugin != 0, "could not open plugin");

	latin1 =
#include "data_latin1.c"
	utf8 =
#include "data_utf8.c"
	succeed_if (plugin->kdbSet(plugin, latin1, 0) == NR_KEYS, "not the correct number of keys");
	succeed_if (compare_keyset(latin1, utf8) == 0, "keysets not equal");
	ksDel (latin1);
	ksDel (utf8);

	latin1 =
#include "data_latin1.c"
	utf8 =
#include "data_utf8.c"
	succeed_if (plugin->kdbGet(plugin, utf8, 0) == NR_KEYS, "not the correct number of keys");
	succeed_if (compare_keyset(utf8, latin1) == 0, "keysets not equal");
	ksDel (latin1);
	ksDel (utf8);

	elektraPluginClose (plugin);
}

void test_utf8_to_latin1()
{
	KeySet *latin1 = 0;
	KeySet *utf8 = 0;

	KeySet *conf = ksNew (2,
			keyNew ("user/from", KEY_VALUE, "UTF-8", KEY_END),
			keyNew ("user/to", KEY_VALUE, "ISO8859-1", KEY_END),
			KS_END);


	Plugin *plugin = elektraPluginOpen("iconv", conf);

	plugin->kdbOpen(plugin);

	exit_if_fail (plugin != 0, "could not open plugin");

	latin1 =
#include "data_latin1.c"
	utf8 =
#include "data_utf8.c"
	succeed_if (plugin->kdbGet(plugin, latin1, 0) == NR_KEYS, "not the correct number of keys");
	succeed_if (compare_keyset(latin1, utf8) == 0, "keysets not equal");
	ksDel (latin1);
	ksDel (utf8);

	latin1 =
#include "data_latin1.c"
	utf8 =
#include "data_utf8.c"
	succeed_if (plugin->kdbSet(plugin, utf8, 0) == NR_KEYS, "not the correct number of keys");
	succeed_if (compare_keyset(utf8, latin1) == 0, "keysets not equal");
	ksDel (latin1);
	ksDel (utf8);

	elektraPluginClose (plugin);
}


int main(int argc, char** argv)
{
	printf("   ICONV   TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_latin1_to_utf8();
	test_utf8_to_latin1();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

