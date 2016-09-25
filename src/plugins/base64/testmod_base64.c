/**
 * @file
 *
 * @brief test suite for the fcrypt plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <kdb.h>
#include <kdbinternal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tests_internal.h>
#include <tests_plugin.h>

#include "base64.h"

// test vectors are defined in RFC4648
// see https://www.ietf.org/rfc/rfc4648.txt
static const char * decoded[] = { "", "f", "fo", "foo", "foob", "fooba", "foobar" };
static const char * encoded[] = { "", "Zg==", "Zm8=", "Zm9v", "Zm9vYg==", "Zm9vYmE=", "Zm9vYmFy" };
static const size_t testcaseCounter = sizeof (decoded) / sizeof (const char *);

static inline KeySet * newPluginConfiguration ()
{
	return ksNew (0, KS_END);
}

static void test_init ()
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * configKs = newPluginConfiguration ();
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (ELEKTRA_PLUGIN_NAME, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		succeed_if (!strcmp (plugin->name, ELEKTRA_PLUGIN_NAME), "got wrong name");

		KeySet * config = elektraPluginGetConfig (plugin);
		succeed_if (config != 0, "there should be a config");

		succeed_if (plugin->kdbGet != 0, "no get pointer");
		succeed_if (plugin->kdbSet != 0, "no set pointer");

		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static inline char testcase2char (size_t i)
{
	return '0' + i + 1;
}

static void test_base64_encoding ()
{
	char errorAlloc[] = "Encoding #.: Memory allocation failed";
	char errorMismatch[] = "Encoding #.: returned unexpected result";

	for (size_t i = 0; i < testcaseCounter; i++)
	{
		errorAlloc[10] = testcase2char (i);
		errorMismatch[10] = testcase2char (i);

		char * e = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) ((kdb_octet_t *)decoded[i], strlen (decoded[i]));
		succeed_if (e, errorAlloc);
		if (e)
		{
			succeed_if (strcmp (e, encoded[i]) == 0, errorMismatch);
			elektraFree (e);
		}
	}
}

static void test_base64_decoding ()
{
	char errorFail[] = "Decoding #.: operation failed";
	char errorMismatch[] = "Decoding #.: returned unexpected result vector";
	char errorLength[] = "Decoding #.: returned unexpected result length";

	kdb_octet_t * buffer = NULL;
	size_t bufferLen = 0;

	// first test case is a little special because we expect NULL on success here
	succeed_if (ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (encoded[0], &buffer, &bufferLen) == 1,
		    "decoding of test vector 1 failed");
	succeed_if (buffer == NULL, "decoding of test vector 1 returned unexpected result vector");
	succeed_if (bufferLen == 0, "decoding of test vector 1 returned unexpected result length");
	if (buffer)
	{
		elektraFree (buffer);
	}

	for (size_t i = 1; i < testcaseCounter; i++)
	{
		errorMismatch[10] = testcase2char (i);
		errorFail[10] = testcase2char (i);
		errorLength[10] = testcase2char (i);

		succeed_if (ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (encoded[i], &buffer, &bufferLen) == 1,
			    errorFail);
		if (buffer)
		{
			succeed_if (bufferLen == strlen (decoded[i]), errorLength);
			if (bufferLen == strlen (decoded[i]))
			{
				succeed_if (memcmp (buffer, decoded[i], bufferLen) == 0, errorMismatch);
			}
			elektraFree (buffer);
			buffer = NULL;
		}
	}
}

static void test_base64_plugin_regular ()
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * config = newPluginConfiguration ();

	elektraModulesInit (modules, 0);
	plugin = elektraPluginOpen (ELEKTRA_PLUGIN_NAME, modules, config, 0);
	succeed_if (plugin, "failed to open plugin handle");
	if (plugin)
	{
		Key * k;
		const kdb_octet_t sampleValue[] = { 0x31, 0x32, 0x33 };

		KeySet * data = ksNew (4, keyNew ("/t/k1", KEY_VALUE, "Hello World", KEY_END),
				       keyNew ("/t/k2", KEY_BINARY, KEY_SIZE, sizeof (sampleValue), KEY_VALUE, sampleValue, KEY_END),
				       keyNew ("/t/k3", KEY_BINARY, KEY_SIZE, 0, KEY_END),
				       keyNew ("/t/k4", KEY_VALUE, ELEKTRA_PLUGIN_BASE64_PREFIX, KEY_END), KS_END);

		// test encoding
		succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");

		k = ksLookupByName (data, "/t/k1", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (strcmp (keyString (k), "Hello World") == 0, "changed string value that does not require encoding");
		}

		k = ksLookupByName (data, "/t/k2", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (strcmp (keyString (k), ELEKTRA_PLUGIN_BASE64_PREFIX "MTIz") == 0,
				    "encoding binary key failed during kdb set");
		}

		k = ksLookupByName (data, "/t/k3", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (strcmp (keyString (k), ELEKTRA_PLUGIN_BASE64_PREFIX "") == 0,
				    "encoding NULL-key failed during kdb set");
		}

		k = ksLookupByName (data, "/t/k4", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (strcmp (keyString (k), ELEKTRA_PLUGIN_BASE64_ESCAPE ELEKTRA_PLUGIN_BASE64_PREFIX) == 0,
				    "encoding string starting with prefix " ELEKTRA_PLUGIN_BASE64_ESCAPE " failed during kdb set");
		}

		// test decoding
		succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get (pregetstorage) failed");

		k = ksLookupByName (data, "/t/k1", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (strcmp (keyString (k), "Hello World") == 0, "changed string value that does not require decoding");
		}

		k = ksLookupByName (data, "/t/k2", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (keyGetValueSize (k) == sizeof (sampleValue), "decoding binary key failed during kdb get");
			if (keyGetValueSize (k) == sizeof (sampleValue))
			{
				succeed_if (memcmp (sampleValue, keyValue (k), sizeof (sampleValue)) == 0,
					    "decoding binary key failed during kdb get");
			}
		}

		k = ksLookupByName (data, "/t/k3", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (keyGetValueSize (k) <= 0, "decoding NULL-key failed during kdb get");
		}

		k = ksLookupByName (data, "/t/k4", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (strcmp (keyString (k), ELEKTRA_PLUGIN_BASE64_PREFIX) == 0,
				    "decoding string starting with prefix " ELEKTRA_PLUGIN_BASE64_ESCAPE " failed during kdb get");
		}

		ksDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_base64_plugin_decoding_error ()
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * config = newPluginConfiguration ();

	elektraModulesInit (modules, 0);
	plugin = elektraPluginOpen (ELEKTRA_PLUGIN_NAME, modules, config, 0);
	succeed_if (plugin, "failed to open plugin handle");
	if (plugin)
	{
		Key * k;
		KeySet * data = ksNew (1, keyNew ("/t/k1", KEY_VALUE, ELEKTRA_PLUGIN_BASE64_PREFIX "_$..", KEY_END), KS_END);

		// test failing decoding
		succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get failed");

		k = ksLookupByName (data, "/t/k1", 0);
		succeed_if (k, "lost key in data KeySet");
		if (k)
		{
			succeed_if (strcmp (keyString (k), ELEKTRA_PLUGIN_BASE64_PREFIX "_$..") == 0,
				    "decoded string value that should have failed");
		}

		ksDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

int main (int argc, char ** argv)
{
	printf ("BASE64       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	// test the encoding and decoding process
	test_base64_encoding ();
	test_base64_decoding ();

	// test the plugin functionality
	test_init ();
	test_base64_plugin_regular ();
	test_base64_plugin_decoding_error ();

	printf ("\n" ELEKTRA_PLUGIN_NAME " RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	return nbError;
}
