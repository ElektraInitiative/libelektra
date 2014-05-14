/**
 * \file
 *
 * \brief A plugin that makes use of libaugeas to read and write configuration files
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

void test_readhosts()
{
	Key *parentKey = keyNew ("user/tests/augeas-hosts", KEY_VALUE, srcdir_file ("hosts"), KEY_END);
	KeySet *conf = ksNew (20, keyNew ("system/lens", KEY_VALUE, "Hosts.lns", KEY_END), KS_END);
	PLUGIN_OPEN("augeas");

	KeySet *ks = ksNew (0);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key *key = ksLookupByName (ks, "user/tests/augeas-hosts/1/ipaddr", 0);
	exit_if_fail(key, "ip address of localhost not found");
	succeed_if(strcmp ("127.0.0.1", keyValue (key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user/tests/augeas-hosts/1/canonical", 0);
	exit_if_fail(key, "name of localhost not found");
	succeed_if(strcmp ("localhost", keyValue (key)) == 0, "name not correct");

	ksDel (ks);

	PLUGIN_CLOSE();

}

void test_order()
{
	Key * parentKey = keyNew ("user/tests/augeas-hosts", KEY_VALUE, srcdir_file ("hosts-big"), KEY_END);
	KeySet *conf = ksNew (20, keyNew ("system/lens", KEY_VALUE, "Hosts.lns", KEY_END), KS_END);
	PLUGIN_OPEN("augeas");

	KeySet *ks = ksNew (0);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key *key;
	size_t currentIndex = 0;
	size_t numKeys = ksGetSize (ks);
	long *usedOrders = calloc (numKeys, sizeof(long));
	memset (usedOrders, -1, sizeof(usedOrders));

	ksRewind (ks);
	while ((key = ksNext (ks)) != 0)
	{
		if (strcmp (keyName (key), keyName (parentKey)))
		{
			char errorMessage[150];
			const Key *orderKey = keyGetMeta (key, "order");

			snprintf (errorMessage, 150, "key %s has no order", keyName (key));

			succeed_if(orderKey, errorMessage);

			char *orderString = (char *) keyValue (orderKey);
			long order;
			char *end;
			order = strtol (orderString, &end, 10);
			snprintf (errorMessage, 150, "key %s has an unparseable order", keyName (key));

			succeed_if(*end == 0, errorMessage);

			snprintf (errorMessage, 150, "key %s has a negative order", keyName (key));
			succeed_if(order >= 0, errorMessage);

			snprintf (errorMessage, 150, "the order %ld exists more than once. Duplicate found in %s.", order, keyName(key));

			// TODO: this is in O(n^2) where n is the number of keys
			for (size_t i = 0; i < currentIndex; i++)
			{
				succeed_if(usedOrders[i] != order, errorMessage);
			}

			usedOrders[currentIndex] = order;
			++currentIndex;
		}
	}

	free (usedOrders);
	ksDel (ks);

	PLUGIN_CLOSE();
}

int main(int argc, char** argv)
{
	printf ("MOUNT       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_readhosts ();
	test_order ();

	printf ("\ntest_hosts RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

