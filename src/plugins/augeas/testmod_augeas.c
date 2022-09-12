/**
 * @file
 *
 * @brief A plugin that makes use of libaugeas to read and write configuration files
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

#include <kdbprivate.h>

#include <tests_plugin.h>

#define return_if_fail(expression, message)                                                                                                \
	do                                                                                                                                 \
	{                                                                                                                                  \
		nbTest++;                                                                                                                  \
		if (!(expression))                                                                                                         \
		{                                                                                                                          \
			yield_error (message);                                                                                             \
			return;                                                                                                            \
		}                                                                                                                          \
	} while (0)

static void test_hostLensRead (char * fileName)
{
	ElektraKey * parentKey = keyNew ("user:/tests/augeas-hosts", ELEKTRA_KEY_VALUE, srcdir_file (fileName), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (20, keyNew ("system:/lens", ELEKTRA_KEY_VALUE, "Hosts.lns", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("augeas");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKey * key = ksLookupByName (ks, "user:/tests/augeas-hosts/1/ipaddr", 0);
	return_if_fail (key, "ip address of localhost not found");
	succeed_if (strcmp ("127.0.0.1", keyValue (key)) == 0, "address of localhost not correct");

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/1/canonical", 0);
	return_if_fail (key, "name of localhost not found");
	succeed_if (strcmp ("localhost", keyValue (key)) == 0, "name of localhost not correct");

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/2/ipaddr", 0);
	return_if_fail (key, "ip address of host1 not found");
	succeed_if (strcmp ("192.168.0.1", keyValue (key)) == 0, "address of host1 not correct");

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/2/canonical", 0);
	return_if_fail (key, "name of host1 not found");
	succeed_if (strcmp ("host1", keyValue (key)) == 0, "name of host1 not correct");

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/2/alias[1]", 0);
	return_if_fail (key, "alias1 of host1 not found");
	succeed_if (strcmp ("alias1", keyValue (key)) == 0, "name of alias1 of host1 not correct");

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/2/alias[2]", 0);
	return_if_fail (key, "alias2 of host1 not found");
	succeed_if (strcmp ("alias2", keyValue (key)) == 0, "name of alias2 of host1 not correct");

	PLUGIN_CLOSE ();

	ksDel (ks);
	keyDel (parentKey);
}

static void test_hostLensWrite (char * fileName)
{
	ElektraKey * parentKey = keyNew ("user:/tests/augeas-hosts", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (20, keyNew ("system:/lens", ELEKTRA_KEY_VALUE, "Hosts.lns", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("augeas");

	// clang-format off
	ElektraKeyset *ks = ksNew (30, keyNew ("user:/tests/augeas-hosts/1", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/1/ipaddr", ELEKTRA_KEY_VALUE, "127.0.0.1",
					ELEKTRA_KEY_META, "order", "10", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/1/canonical", ELEKTRA_KEY_VALUE,
					"localhost", ELEKTRA_KEY_META, "order", "20", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/1/#comment", ELEKTRA_KEY_VALUE,
					"hostcomment", ELEKTRA_KEY_META, "order", "21", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/#comment", ELEKTRA_KEY_VALUE,
					"linecomment", ELEKTRA_KEY_META, "order", "22", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/2/ipaddr", ELEKTRA_KEY_VALUE,
					"192.168.0.1", ELEKTRA_KEY_META, "order", "30", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/2/canonical", ELEKTRA_KEY_VALUE, "host1",
					ELEKTRA_KEY_META, "order", "40", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/2/alias[1]", ELEKTRA_KEY_VALUE,
					"host1alias1", ELEKTRA_KEY_META, "order", "50", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/2/alias[2]", ELEKTRA_KEY_VALUE,
					"host1alias2", ELEKTRA_KEY_META, "order", "60", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/3/ipaddr", ELEKTRA_KEY_VALUE,
					"fd00::4711:4712:2::1", ELEKTRA_KEY_META, "order", "70", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/3/canonical", ELEKTRA_KEY_VALUE, "host2",
					ELEKTRA_KEY_META, "order", "80", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/3/alias[1]", ELEKTRA_KEY_VALUE,
					"host2alias1", ELEKTRA_KEY_META, "order", "90", ELEKTRA_KEY_END),
			keyNew ("user:/tests/augeas-hosts/3/alias[2]", ELEKTRA_KEY_VALUE,
					"host2alias2", ELEKTRA_KEY_META, "order", "100", ELEKTRA_KEY_END), ELEKTRA_KS_END);
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

static void test_hostLensDelete (char * sourceFile, char * compFile)
{
	ElektraKey * parentKey = keyNew ("user:/tests/augeas-hosts", ELEKTRA_KEY_VALUE, srcdir_file (sourceFile), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (20, keyNew ("system:/lens", ELEKTRA_KEY_VALUE, "Hosts.lns", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("augeas");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKey * key = ksLookupByName (ks, "user:/tests/augeas-hosts/1", 0);
	return_if_fail (key, "localhost not found");
	elektraKsPopAtCursor (ks, ksGetCursor (ks));
	keyDel (key);

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/1/ipaddr", 0);
	return_if_fail (key, "ip address of localhost not found");
	elektraKsPopAtCursor (ks, ksGetCursor (ks));
	keyDel (key);

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/1/canonical", 0);
	return_if_fail (key, "canonical of localhost not found");
	elektraKsPopAtCursor (ks, ksGetCursor (ks));
	keyDel (key);

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/1/#comment", 0);
	return_if_fail (key, "comment of localhost not found");
	elektraKsPopAtCursor (ks, ksGetCursor (ks));
	keyDel (key);

	keySetString (parentKey, elektraFilename ());

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (compFile), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);

	elektraUnlink (keyString (parentKey));
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_hostLensModify (char * sourceFile, char * compFile)
{
	ElektraKey * parentKey = keyNew ("user:/tests/augeas-hosts", ELEKTRA_KEY_VALUE, srcdir_file (sourceFile), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (20, keyNew ("system:/lens", ELEKTRA_KEY_VALUE, "Hosts.lns", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("augeas");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKey * key = ksLookupByName (ks, "user:/tests/augeas-hosts/1/ipaddr", 0);
	return_if_fail (key, "ip address of localhost not found");
	keySetString (key, "127.0.0.2");

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/3/ipaddr", 0);
	return_if_fail (key, "ip address of host2 not found");
	keySetString (key, "fd00::4711:4712:2::2");

	key = ksLookupByName (ks, "user:/tests/augeas-hosts/#comment", 0);
	return_if_fail (key, "line comment not found");
	keySetString (key, "line comment modified");

	keySetString (parentKey, elektraFilename ());

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (compFile), keyString (parentKey)), "files do not match as expected");

	PLUGIN_CLOSE ();

	elektraUnlink (keyString (parentKey));

	ksDel (ks);
	keyDel (parentKey);
}

static void test_order (char * fileName)
{
	ElektraKey * parentKey = keyNew ("user:/tests/augeas-hosts", ELEKTRA_KEY_VALUE, srcdir_file (fileName), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (20, keyNew ("system:/lens", ELEKTRA_KEY_VALUE, "Hosts.lns", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("augeas");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKey * key;
	size_t currentIndex = 0;
	size_t numKeys = ksGetSize (ks);
	long * usedOrders = elektraMalloc (numKeys * sizeof (long));

	return_if_fail (usedOrders, "unable to allocate memory for order array");

	/* as 0 is a legit order we have to initialize the array manually */
	for (size_t index = 0; index < numKeys; index++)
	{
		usedOrders[index] = -1;
	}

	ksRewind (ks);
	while ((key = ksNext (ks)) != 0)
	{
		if (strcmp (keyName (key), keyName (parentKey)))
		{
			char errorMessage[150];
			const ElektraKey * orderKey = keyGetMeta (key, "order");

			snprintf (errorMessage, 150, "key %s has no order", keyName (key));

			succeed_if (orderKey, errorMessage);

			char * orderString = (char *) keyValue (orderKey);
			long order;
			char * end;
			order = strtol (orderString, &end, 10);
			snprintf (errorMessage, 150, "key %s has an unparseable order", keyName (key));

			succeed_if (*end == 0, errorMessage);

			snprintf (errorMessage, 150, "key %s has a negative order", keyName (key));
			succeed_if (order >= 0, errorMessage);

			snprintf (errorMessage, 150, "the order %ld exists more than once. Duplicate found in %s.", order, keyName (key));

			// TODO: this is in O(n^2) where n is the number of keys
			for (size_t i = 0; i < currentIndex; i++)
			{
				succeed_if (usedOrders[i] != order, errorMessage);
			}

			usedOrders[currentIndex] = order;
			++currentIndex;
		}
	}

	elektraFree (usedOrders);
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_hostLensFormatting (char * fileName)
{
	ElektraKey * parentKey = keyNew ("user:/tests/augeas-hosts", ELEKTRA_KEY_VALUE, srcdir_file (fileName), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (20, keyNew ("system:/lens", ELEKTRA_KEY_VALUE, "Hosts.lns", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("augeas");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	keySetString (parentKey, elektraFilename ());

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	elektraUnlink (keyString (parentKey));
	keyDel (parentKey);
	ksDel (ks);


	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("AUGEAS       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	/* output all lenses:
	KeySet * ks = ksNew (5, KS_END);
	elektraAugeasGenConf (ks, 0);
	output_keyset (ks);
	ksDel (ks);
	*/

	test_hostLensRead ("augeas/hosts-read");
	test_hostLensWrite ("augeas/hosts-write");
	test_hostLensModify ("augeas/hosts-modify-in", "augeas/hosts-modify");
	test_hostLensDelete ("augeas/hosts-delete-in", "augeas/hosts-delete");
	test_hostLensFormatting ("augeas/hosts-formatting");
	test_order ("augeas/hosts-big");

	print_result ("test_augeas");

	return nbError;
}
