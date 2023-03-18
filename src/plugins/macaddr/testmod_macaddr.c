/**
 * @file
 *
 * @brief Tests for macaddr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <internal/kdb/config.h>

#include <tests_plugin.h>

#define PLUGIN_NAME "macaddr"
#define META "check/macaddr"
#define MAXMACINT 281474976710655

static void convertLong (char * returned, unsigned long long i)
{
	sprintf (returned, "%llu", i);
}

static int setKey (KeySet * testKs)
{
	Key * parent = keyNew ("user:/tests/mac", KEY_VALUE, "", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	int ret = plugin->kdbSet (plugin, testKs, parent);
	keyDel (parent);
	PLUGIN_CLOSE ();
	return ret;
}

static const char * getKeyString (KeySet * ks, char * keyName)
{
	Key * parent = keyNew ("user:/tests/mac", KEY_VALUE, "", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	plugin->kdbGet (plugin, ks, parent);
	keyDel (parent);
	PLUGIN_CLOSE ();
	return keyString (ksLookupByName (ks, keyName, 0));
}

static void testAddressSet (const char * keyValue, int retValue)
{
	KeySet * testKs = ksNew (10, keyNew ("user:/tests/mac/addr", KEY_VALUE, keyValue, KEY_META, META, "", KEY_END), KS_END);
	succeed_if (setKey (testKs) == retValue, "error");
	ksDel (testKs);
}

static void testAddressesSetGet (const char * keyValue, unsigned long long longValue)
{
	char intChar[21];
	Key * key = keyNew ("user:/tests/mac/addr", KEY_VALUE, keyValue, KEY_META, META, "", KEY_END);
	KeySet * testKs = ksNew (10, key, KS_END);
	setKey (testKs);
	convertLong (intChar, longValue);
	succeed_if (!strcmp (getKeyString (testKs, "user:/tests/mac/addr"), intChar), "error");
	succeed_if (!strcmp (keyString (keyGetMeta (key, "origvalue")), keyValue), "error");
	ksDel (testKs);
}

static void testAddressesReturn (void)
{
	testAddressesSetGet ("00:00:00:00:00:00", 0);
	testAddressesSetGet ("FF:FF:FF:FF:FF:FF", 281474976710655);
	testAddressesSetGet ("0d:b6:8c:44:cc:f9", 15077688528121);
	testAddressesSetGet ("aB-Cd-8f-f3-e5-d7", 188899371771351);
	testAddressesSetGet ("A1B2C3-4D5E6F", 177789152878191);
}

static void testAddressesStandardColons (void)
{
	testAddressSet ("00:00:00:00:00:00", 1);
	testAddressSet ("FF:FF:FF:FF:FF:FF", 1);
	testAddressSet ("AA:BB:CC:DD:EE:FF", 1);
	testAddressSet ("99:99:99:99:99:99", 1);
	testAddressSet ("A1:B2:C3:4D:5E:6F", 1);
	testAddressSet ("aB:Cd:8f:f3:e5:d7", 1);
	testAddressSet ("aB:Cd:8f:f3:e5:g7", -1);
	testAddressSet ("aB:Cd:8f:f3:e5e:d7", -1);
	testAddressSet ("aB:Cd:8f:f3:e5", -1);
	testAddressSet ("aB:Cd:8f:f3:e5", -1);
}

static void testAddressesStandardHyphens (void)
{

	testAddressSet ("00-00-00-00-00-00", 1);
	testAddressSet ("FF-FF-FF-FF-FF-FF", 1);
	testAddressSet ("AA-BB-CC-DD-EE-FF", 1);
	testAddressSet ("99-99-99-99-99-99", 1);
	testAddressSet ("A1-B2-C3-4D-5E-6F", 1);
	testAddressSet ("aB-Cd-8f-f3-e5-d7", 1);
	testAddressSet ("aB-Cd-8f-f3-e5-g7", -1);
	testAddressSet ("aB-Cd-8f-f3-e5e-d7", -1);
	testAddressSet ("aB-Cd-8f-f3-e5", -1);
	testAddressSet ("aB-Cd-8f-f3-e5", -1);
}

static void testAddressesSingleHyphen (void)
{
	testAddressSet ("000000-000000", 1);
	testAddressSet ("FFFFFF-FFFFFF", 1);
	testAddressSet ("AABBCC-DDEEFF", 1);
	testAddressSet ("999999-999999", 1);
	testAddressSet ("A1B2C3-4D5E6F", 1);
	testAddressSet ("aBCd8f-f3e5d7", 1);
	testAddressSet ("aBCd8f-f3e5g7", -1);
	testAddressSet ("aBCd8f-f3e5ed7", -1);
	testAddressSet ("aBCd8f-f3e5", -1);
	testAddressSet ("aBCd8f-f3e5", -1);
}

static void testAddressesNumber (void)
{
	char intChar[21];
	convertLong (intChar, 0);
	testAddressSet (intChar, 1);

	convertLong (intChar, MAXMACINT);
	testAddressSet (intChar, 1);

	convertLong (intChar, -1);
	testAddressSet (intChar, -1);

	convertLong (intChar, MAXMACINT + 1);
	testAddressSet (intChar, -1);
}

static void testRestoreValue (void)
{
	char * val = "00:11:55:AA:FF:CC";
	Key * key = keyNew ("user:/tests/mac/addr", KEY_VALUE, val, KEY_META, META, "", KEY_END);
	KeySet * testKs = ksNew (10, key, KS_END);
	setKey (testKs);
	getKeyString (testKs, "user:/tests/mac/addr");
	setKey (testKs);
	succeed_if (!strcmp (keyString (key), val), "error");
	ksDel (testKs);
}

static void testAll (void)
{
	testAddressesStandardColons ();
	testAddressesStandardHyphens ();
	testAddressesSingleHyphen ();
	testAddressesNumber ();
	testAddressesReturn ();
	testRestoreValue ();
}


int main (int argc, char ** argv)
{
	printf ("MACADDR     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testAll ();

	print_result ("testmod_macaddr");

	return nbError;
}
