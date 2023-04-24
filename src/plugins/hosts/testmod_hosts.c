/**
 * @file
 *
 * @brief Tests for the hosts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
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

#include "./keymetaformatting.h"
#include <tests_plugin.h>

void test_readHostsSimple (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/hosts", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("hosts");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key * key = ksLookupByName (ks, "user:/tests/hosts/ipv4/localhost", 0);
	exit_if_fail (key, "hostname localhost not found");
	succeed_if (strcmp ("127.0.0.1", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv4/gateway.markus-raab.org", 0);
	exit_if_fail (key, "hostname gateway.markus-raab.org not found");
	succeed_if (strcmp ("192.168.0.1", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv4/kirabyte.markus-raab.org/kira", 0);
	exit_if_fail (key, "hostname alias kira not found");
	succeed_if (strcmp ("192.168.0.5", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv6/wikipedia-sample", 0);
	exit_if_fail (key, "hostname wikipedia-sample not found");
	succeed_if (strcmp ("fd9e:21a7:a92c:2323::1", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv6/wikipedia-sample/wikipedia-alias", 0);
	exit_if_fail (key, "hostname alias wikipedia-alias not found");
	succeed_if (strcmp ("fd9e:21a7:a92c:2323::1", keyValue (key)) == 0, "address not correct");


	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_readInvalidIpAddress (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/hosts", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("hosts");

	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key * key = ksLookupByName (ks, "user:/tests/hosts/ipv4/localhost", KDB_O_NONE);
	exit_if_fail (key, "hostname localhost not found");
	succeed_if (strcmp ("noipaddress", keyValue (key)) == 0, "address not correct");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_mixedAddresses (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/hosts", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("hosts");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key * key = ksLookupByName (ks, "user:/tests/hosts/ipv4/ipv4host", KDB_O_NONE);
	exit_if_fail (key, "hostname ipv4host not found");
	succeed_if (strcmp ("127.0.0.1", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv4/ipv4host/ipv4alias1", KDB_O_NONE);
	succeed_if (key, "ipv4alias1 not found");
	key = ksLookupByName (ks, "user:/tests/hosts/ipv4/ipv4host/ipv4alias2", KDB_O_NONE);
	succeed_if (key, "ipv4alias2 not found");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv6/ipv6host", KDB_O_NONE);
	exit_if_fail (key, "hostname ipv6host not found");
	succeed_if (strcmp ("::1", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv6/ipv6host/ipv6alias1", KDB_O_NONE);
	succeed_if (key, "ipv6alias1 not found");
	key = ksLookupByName (ks, "user:/tests/hosts/ipv6/ipv6host/ipv6alias2", KDB_O_NONE);
	succeed_if (key, "ipv6alias2 not found");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_duplicateEntries (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/hosts", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("hosts");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key * key = ksLookupByName (ks, "user:/tests/hosts/ipv4/localhost", KDB_O_NONE);
	exit_if_fail (key, "hostname localhost not found");
	succeed_if (strcmp ("127.0.0.1", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv4/host", KDB_O_NONE);
	exit_if_fail (key, "hostname host not found");
	succeed_if (strcmp ("192.168.0.1", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user:/tests/hosts/ipv4/host/alias1", KDB_O_NONE);
	succeed_if (key, "alias1 not found");
	key = ksLookupByName (ks, "user:/tests/hosts/ipv4/host/alias2", KDB_O_NONE);
	succeed_if (key, "alias2 not found");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_duplicateOrder (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/hosts", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("hosts");

	// clang-format off
	KeySet *ks = ksNew (20,
			keyNew ("user:/tests/hosts/ipv4/host1",
					KEY_VALUE, "192.168.0.1",
					KEY_META, "order", "10",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv4/host2",
					KEY_VALUE, "192.168.0.2",
					KEY_META, "order", "20",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv4/host3",
					KEY_VALUE, "192.168.0.3",
					KEY_META, "order", "20",
					KEY_END),
			KS_END);
	// clang-format on

	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	elektraUnlink (keyString (parentKey));
	ksDel (ks);

	PLUGIN_CLOSE ();
}

void test_writeHostsSimple (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/hosts", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("hosts");

	// clang-format off
	KeySet *ks = ksNew (20,
			keyNew ("user:/tests/hosts/ipv4/localhost",
					KEY_VALUE, "127.0.0.1",
					KEY_META, "order", "10",
					KEY_META, "comment/#0", "",
					KEY_META, "comment/#1", " these are for ipv4",
					KEY_META, "comment/#1/start", "#",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv4/testhost",
					KEY_VALUE, "127.0.1.1",
					KEY_META, "order", "20",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv4/testhost/testhostalias",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv6/localhost",
					KEY_VALUE, "::1",
					KEY_META, "order", "30",
					KEY_META, "comment/#0", "",
					KEY_META, "comment/#1", " The following lines are desirable for IPv6 capable hosts",
					KEY_META, "comment/#1/start", "#",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv6/localhost/ip6-localhost",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv6/localhost/ip6-loopback",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv6/ip6-allnodes",
					KEY_VALUE, "ff02::1",
					KEY_META, "order", "40",
					KEY_END),
			KS_END);
	// clang-format on

	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	elektraUnlink (keyString (parentKey));
	ksDel (ks);

	PLUGIN_CLOSE ();
}

void test_readHostsComments (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/hosts", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("hosts");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	/* FIRST ENTRY */
	Key * key = ksLookupByName (ks, "user:/tests/hosts/ipv4/localhost", 0);
	exit_if_fail (key, "hostname localhost not found");

	/* inline comment */
	const Key * inlineComment1 = keyGetMeta (key, "comment/#0");
	succeed_if (inlineComment1, "inline comment for first host does not exist");
	succeed_if (!strcmp (keyString (inlineComment1), "inline comment0"), "inline comment for first host contains wrong text");

	const Key * inlineComment1Start = keyGetMeta (key, "comment/#0/start");
	succeed_if (inlineComment1Start, "start key for inline  of first host does not exist");
	succeed_if (!strcmp (keyString (inlineComment1Start), "#"), "start key for inline comment of first host contains wrong text");

	const Key * inlineComment1Space = keyGetMeta (key, "comment/#0/space");
	succeed_if (inlineComment1Space, "space key for inline comment of first host does not exist");
	succeed_if (!strcmp (keyString (inlineComment1Space), "3"),
		    "space key for inline comment of first host contains wrong number of spaces");


	/* empty lines */
	const Key * lineComment1 = keyGetMeta (key, "comment/#1");
	succeed_if (lineComment1, "comment for first empty line does not exist");

	const Key * lineComment2 = keyGetMeta (key, "comment/#2");
	succeed_if (lineComment2, "comment for second empty line does not exist");


	/* line comment */
	const Key * lineComment3 = keyGetMeta (key, "comment/#3");
	succeed_if (lineComment3, "comment key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment3), " comment for localhost"), "comment key for line comment contains wrong text");

	const Key * lineComment3Start = keyGetMeta (key, "comment/#3/start");
	succeed_if (lineComment3Start, "start key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment3Start), "#"), "start key for line comment contains wrong text");

	const Key * lineComment3Spaces = keyGetMeta (key, "comment/#3/space");
	succeed_if (lineComment3Spaces, "space key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment3Spaces), "0"), "space key for line comment contains wrong number of spaces");

	/* empty line */
	const Key * emptyLine = keyGetMeta (key, "comment/#4");
	succeed_if (emptyLine, "comment key for line comment does not exist");
	succeed_if (!strcmp ("", keyString (emptyLine)), "line comment key contains data although it shouldn't")

		/* SECOND ENTRY */
		Key * key2 = ksLookupByName (ks, "user:/tests/hosts/ipv4/testentry", 0);
	exit_if_fail (key2, "hostname localhost not found");

	/* inline comment */
	const Key * inlineComment2 = keyGetMeta (key2, "comment/#0");
	succeed_if (inlineComment2, "inline comment for second host does not exist");
	succeed_if (!strcmp (keyString (inlineComment2), " inline comment1"), "inline comment for second host contains wrong text");

	const Key * inlineComment2Start = keyGetMeta (key2, "comment/#0/start");
	succeed_if (inlineComment2Start, "start key for inline  of second host does not exist");
	succeed_if (!strcmp (keyString (inlineComment2Start), "#"), "start key for inline comment of second host contains wrong text");

	const Key * inlineComment2Space = keyGetMeta (key2, "comment/#0/space");
	succeed_if (inlineComment2Space, "space key for inline comment of second host does not exist");
	succeed_if (!strcmp (keyString (inlineComment2Space), "1"),
		    "space key for inline comment of second host contains wrong number of spaces");


	/* empty line */
	const Key * lineComment4 = keyGetMeta (key2, "comment/#1");
	succeed_if (lineComment4, "comment key for line comment does not exist");

	const Key * lineComment4Spaces = keyGetMeta (key2, "comment/#1/space");
	succeed_if (lineComment4Spaces, "space key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment4Spaces), "2"), "space key for line comment contains wrong number of spaces");


	/* line comment */
	const Key * lineComment5 = keyGetMeta (key2, "comment/#2");
	succeed_if (lineComment5, "comment key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment5), " comment for testentry"), "comment key for line comment contains wrong text");

	const Key * lineComment5Start = keyGetMeta (key2, "comment/#2/start");
	succeed_if (lineComment5Start, "start key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment5Start), "#"), "start key for line comment contains wrong text");

	const Key * lineComment5Spaces = keyGetMeta (key2, "comment/#2/space");
	succeed_if (lineComment5Spaces, "space key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment5Spaces), "2"), "space key for line comment contains wrong number of spaces");


	/* NO ENTRY */

	/* empty line */
	const Key * lineComment6 = keyGetMeta (parentKey, "comment/#1");
	succeed_if (lineComment6, "comment key for line comment does not exist");


	/* line comment */
	const Key * lineComment7 = keyGetMeta (parentKey, "comment/#2");
	succeed_if (lineComment7, "comment key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment7), " comment without entry"), "comment key for line comment contains wrong text");

	const Key * lineComment7Start = keyGetMeta (parentKey, "comment/#2/start");
	succeed_if (lineComment7Start, "start key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment7Start), "#"), "start key for line comment contains wrong text");

	const Key * lineComment7Spaces = keyGetMeta (parentKey, "comment/#2/space");
	succeed_if (lineComment7Spaces, "space key for line comment does not exist");
	succeed_if (!strcmp (keyString (lineComment7Spaces), "0"), "space key for line comment contains wrong number of spaces");


	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_writeHostsComments (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/hosts", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("hosts");

	// clang-format off
	KeySet *ks = ksNew (20,
			keyNew ("user:/tests/hosts/ipv4/localhost",
					KEY_VALUE, "127.0.0.1",
					KEY_META, "order", "10",
					KEY_META, "comment/#0", "inline comment0",
					KEY_META, "comment/#0/space", "3",
					KEY_META, "comment/#0/start", "#",
					KEY_META, "comment/#1", "",
					KEY_META, "comment/#2", "",
					KEY_META, "comment/#3", " comment for localhost",
					KEY_META, "comment/#3/start", "#",
					KEY_META, "comment/#4", "",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv4/testentry",
					KEY_VALUE, "192.168.0.1",
					KEY_META, "order", "20",
					KEY_META, "comment/#0", " inline comment1",
					KEY_META, "comment/#0/space", "1",
					KEY_META, "comment/#0/start", "#",
					KEY_META, "comment/#1", "",
					KEY_META, "comment/#1/space", "2",
					KEY_META, "comment/#2", " comment for testentry",
					KEY_META, "comment/#2/space", "2",
					KEY_META, "comment/#2/start", "#",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv4/testentry/alias1",
					KEY_END),
			keyNew ("user:/tests/hosts/ipv4/testentry/alias2",
					KEY_END),
			KS_END);
	// clang-format on

	keySetMeta (parentKey, "comment/#1", "");
	keySetMeta (parentKey, "comment/#2", " comment without entry");
	keySetMeta (parentKey, "comment/#2/start", "#");

	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	elektraUnlink (keyString (parentKey));
	ksDel (ks);

	PLUGIN_CLOSE ();
}

static void test_format (void)
{
	printf ("Test key format\n");

	Key * k = keyNew ("/", KEY_END);
	keySetString (k, "huhu");
	succeed_if_same_string (keyString (k), "huhu");

	keySetStringF (k, "huhu");
	succeed_if_same_string (keyString (k), "huhu");

	keySetStringF (k, "huhu %d", 20);
	succeed_if_same_string (keyString (k), "huhu 20");

	char c1[] = "huhu %d something";
	keySetStringF (k, c1, 20);
	c1[5] = '2';
	c1[6] = '0';
	succeed_if_same_string (keyString (k), c1);
	succeed_if (keyGetValueSize (k) == sizeof (c1), "size wrong");


	char c2[] =
		"An extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something!";
	keySetStringF (k, c2);
	succeed_if_same_string (keyString (k), c2);
	succeed_if (keyGetValueSize (k) == sizeof (c2), "size wrong");


	char c3[] =
		"%s extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something!";
	keySetStringF (k, c3, "AN");
	c3[0] = 'A';
	c3[1] = 'N';
	succeed_if_same_string (keyString (k), c3);
	// printf ("%s\n\nXXX\n%s\n", keyString(k), c3);
	// printf ("%d - %d\n", keyGetValueSize(k), sizeof(c3));
	succeed_if (keyGetValueSize (k) == sizeof (c3), "size wrong");


	char c4[] =
		"%d extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something."
		"an extremely long string that is way longer then default capture size of 512 or something!";
	keySetStringF (k, c4, 20);
	c4[0] = '2';
	c4[1] = '0';
	succeed_if_same_string (keyString (k), c4);
	succeed_if (keyGetValueSize (k) == sizeof (c4), "size wrong");

	keyDel (k);
}

int main (int argc, char ** argv)
{
	printf ("MOUNT       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_readHostsSimple ("hosts/hosts-read-simple");
	test_readInvalidIpAddress ("hosts/hosts-invalid");
	test_mixedAddresses ("hosts/hosts-mixed");
	test_duplicateEntries ("hosts/hosts-duplicate");
	test_duplicateOrder ("hosts/hosts-duporder");
	test_writeHostsSimple ("hosts/hosts-write-simple");
	test_readHostsComments ("hosts/hosts-comments");
	test_writeHostsComments ("hosts/hosts-comments");

	test_format ();

	print_result ("test_hosts");

	return nbError;
}
