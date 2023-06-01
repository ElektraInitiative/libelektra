/**
 * @file
 *
 * @brief test suite for the gpgme plugin.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 * @author Peter Nirschl
 *
 */

#include <elektra/core.h>
#include <elektra/core/errors.h>
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/meta.h>
#include <elektra/plugin/plugin.h>
#include <gpgme.h>
#include <internal/config.h>
#include <internal/kdbprivate.h>
#include <internal/pluginload/module.h>
#include <internal/utility/logger.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tests_internal.h>
#include <tests_plugin.h>
#include <unistd.h>

#include "./gpgme.h"

#include "../crypto/gpgagent_teardown.h"

// GPG private key for importing and testing
#include "../crypto/test_key.h"

#define TEST_KEY_ID "DDEBEF9EE2DC931701338212DAF635B17F230E8D"
#define GPGME_PLUGIN_NAME "gpgme"
#define KEYNAME_UNCHANGED "user:/crypto/test/nochange"
#define KEYNAME_NULL "user:/crypto/test/mynull"
#define KEYNAME_STRING "user:/crypto/test/mystring"
#define KEYNAME_BIN "user:/crypto/test/mybin"

static const char strVal[] = "gpgme test value";
static const kdb_octet_t binVal[] = { 0xCA, 0xFE, 0xBA, 0xBE };

static inline ssize_t MIN (ssize_t a, ssize_t b)
{
	return (a < b) ? a : b;
}

static void init_gpgme (void)
{
	gpgme_error_t err;

	gpgme_check_version (NULL);
	// NOTE the code below is recommended by the gpgme manual
	//	gpgme_set_locale (NULL, LC_CTYPE, setlocale (LC_CTYPE, NULL));
	// #ifndef HAVE_W32_SYSTEM
	//	gpgme_set_locale (NULL, LC_MESSAGES, setlocale (LC_MESSAGES, NULL));
	// #endif

	err = gpgme_engine_check_version (GPGME_PROTOCOL_OpenPGP);
	succeed_if (!err, "failed to initialize gpgme");
}

static KeySet * newPluginConfiguration (void)
{
	return ksNew (3, keyNew (ELEKTRA_RECIPIENT_KEY, KEY_VALUE, TEST_KEY_ID, KEY_END),
		      keyNew (ELEKTRA_GPGME_CONFIG_TEXTMODE, KEY_VALUE, "0", KEY_END),
		      keyNew (ELEKTRA_GPGME_UNIT_TEST, KEY_VALUE, "1", KEY_END), KS_END);
}

static KeySet * newTestdataKeySet (void)
{
	Key * kUnchanged = keyNew (KEYNAME_UNCHANGED, KEY_END);
	Key * kNull = keyNew (KEYNAME_NULL, KEY_END);
	Key * kString = keyNew (KEYNAME_STRING, KEY_END);
	Key * kBin = keyNew (KEYNAME_BIN, KEY_END);

	keySetString (kUnchanged, strVal);

	keySetBinary (kNull, NULL, 0);
	keySetMeta (kNull, ELEKTRA_GPGME_META_ENCRYPT, "1");

	keySetString (kString, strVal);
	keySetMeta (kString, ELEKTRA_GPGME_META_ENCRYPT, "1");

	keySetBinary (kBin, binVal, sizeof (binVal));
	keySetMeta (kBin, ELEKTRA_GPGME_META_ENCRYPT, "1");

	return ksNew (4, kUnchanged, kNull, kString, kBin, KS_END);
}

static void test_import_key (void)
{
	gpgme_error_t err;
	gpgme_data_t keydata;
	gpgme_import_result_t result;
	gpgme_ctx_t ctx;

	err = gpgme_new (&ctx);
	succeed_if (!err, "failed to initialize gpgme handle");

	err = gpgme_data_new_from_mem (&keydata, (const char *) test_key_asc, test_key_asc_len, 1);
	succeed_if (!err, "failed to transform the test key to gpgme data structure");

	err = gpgme_op_import (ctx, keydata);
	succeed_if (!err, "failed to import GPG test key");

	if (!err)
	{
		result = gpgme_op_import_result (ctx);
		succeed_if (result->imported + result->unchanged == 1, "the GPG import failed");
	}

	gpgme_data_release (keydata);
	gpgme_release (ctx);
}

static void test_init (void)
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system:/", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * configKs = newPluginConfiguration ();
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (GPGME_PLUGIN_NAME, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		succeed_if (!strcmp (plugin->name, GPGME_PLUGIN_NAME), "got wrong name");

		KeySet * config = elektraPluginGetConfig (plugin);
		succeed_if (config != 0, "there should be a config");

		succeed_if (plugin->kdbOpen != 0, "no open pointer");
		succeed_if (plugin->kdbClose != 0, "no close pointer");
		succeed_if (plugin->kdbGet != 0, "no get pointer");
		succeed_if (plugin->kdbSet != 0, "no set pointer");

		// try re-opening the plugin
		succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "kdb close failed");
		succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "re-opening the plugin failed");
		succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "kdb close failed");

		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_incomplete_config (void)
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system:/", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * configKs = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (GPGME_PLUGIN_NAME, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		KeySet * data = newTestdataKeySet ();
		succeed_if (plugin->kdbSet (plugin, data, parentKey) == -1, "kdb set succeeded with incomplete configuration");
		ksDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_encryption_decryption (void)
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system:/", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * config = newPluginConfiguration ();

	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (GPGME_PLUGIN_NAME, modules, config, 0);
	if (plugin)
	{
		Key * k;
		KeySet * data = newTestdataKeySet ();
		KeySet * original = ksDup (data);

		// test encryption with kdb set
		succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");

		// - unchanged
		k = ksLookupByName (data, KEYNAME_UNCHANGED, 0);
		succeed_if (k, "missing key " KEYNAME_UNCHANGED " in dataset");
		if (k)
		{
			succeed_if (keyIsString (k), "key type has been modified but was not marked for encryption");
			succeed_if (strcmp (keyString (k), strVal) == 0, "Key value changed but was not marked for encryption");
		}

		// - string value
		k = ksLookupByName (data, KEYNAME_STRING, 0);
		succeed_if (k, "missing key " KEYNAME_STRING " in dataset");
		if (k)
		{
			succeed_if (keyIsBinary (k), "key type not set to binary during encryption");
			succeed_if (memcmp (keyValue (k), strVal, MIN (keyGetValueSize (k), strlen (strVal))) != 0,
				    "key content did not change during encryption");
		}

		// - binary value
		k = ksLookupByName (data, KEYNAME_BIN, 0);
		succeed_if (k, "missing key " KEYNAME_BIN " in dataset");
		if (k)
		{
			succeed_if (keyIsBinary (k), "key type not set to binary during encryption");
			succeed_if (memcmp (keyValue (k), binVal, MIN (keyGetValueSize (k), sizeof (binVal))) != 0,
				    "key content did not change during encryption");
		}

		// - null value
		k = ksLookupByName (data, KEYNAME_NULL, 0);
		succeed_if (k, "missing key " KEYNAME_NULL " in dataset");
		if (k)
		{
			succeed_if (keyGetValueSize (k) == 0, "null value was changed unexpectedly");
		}

		// test decryption with kdb get
		succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get failed");

		// - unchanged
		k = ksLookupByName (data, KEYNAME_UNCHANGED, 0);
		succeed_if (k, "missing key " KEYNAME_UNCHANGED " in dataset");
		if (k)
		{
			succeed_if (keyIsString (k), "key type has been modified but was not marked for decryption");
			succeed_if (strcmp (keyString (k), strVal) == 0, "Key value changed but was not marked for decryption");
		}

		// - string value
		k = ksLookupByName (data, KEYNAME_STRING, 0);
		succeed_if (k, "missing key " KEYNAME_STRING " in dataset");
		if (k)
		{
			succeed_if (keyIsString (k), "key type not restored to string during decryption");
			succeed_if (strcmp (keyString (k), strVal) == 0, "key content not restored during decryption");
		}

		// - binary value
		k = ksLookupByName (data, KEYNAME_BIN, 0);
		succeed_if (k, "missing key " KEYNAME_BIN " in dataset");
		if (k)
		{
			succeed_if (keyIsBinary (k), "key type not restored to binary during decryption");
			succeed_if (memcmp (keyValue (k), binVal, MIN (keyGetValueSize (k), sizeof (binVal))) == 0,
				    "key content not restored during decryption");
		}

		// - null value
		k = ksLookupByName (data, KEYNAME_NULL, 0);
		succeed_if (k, "missing key " KEYNAME_NULL " in dataset");
		if (k)
		{
			succeed_if (keyGetValueSize (k) == 0, "null value was changed unexpectedly");
		}

		ksDel (original);
		ksDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

int main (int argc, char ** argv)
{
	printf ("GPGME        TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	init_gpgme ();
	test_import_key ();
	test_init ();
	test_incomplete_config ();
	test_encryption_decryption ();
	test_teardown ();

	print_result ("gpgme");
	return nbError;
}
