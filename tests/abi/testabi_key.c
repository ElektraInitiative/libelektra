/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.h>

#define NUMBER_OF_NAMESPACES 6

char * namespaces[] = { "/", "spec:/", "proc:/", "dir:/", "user:/", "system:/", 0 };

struct test
{
	char * testName;
	char * keyName;

	char * expectedKeyName;
	char * expectedBaseName;
	char * expectedFRootName;
};

struct test tstKeyName[] = { { "Normal key", "system:/foo/bar", "system:/foo/bar", "bar", "system" },

			     {
				     "Key containing redundant & trailing separator", "system://foo//bar//",
				     "system:/foo/bar", /* keyName 	*/
				     "bar",		/* keyBaseName	*/
				     "system",		/* keyGetFullRootName	*/
			     },

			     {
				     "Normal user key", "user:/key", "user:/key", /* keyName 	*/
				     "key",					  /* keyBaseName 	*/
				     "user",					  /* keyGetFullRootName 	*/
			     },

			     {
				     "Key with empty part", "user:///%", "user:/%", /* keyName 	*/
				     "",					    /* keyBaseName 	*/
				     "",					    /* keyGetFullRootName 	*/

			     },

			     {
				     "Key with escaped %", "user:///\\%", "user:/\\%", /* keyName 	*/
				     "%",					       /* keyBaseName 	*/
				     "",					       /* keyGetFullRootName 	*/

			     },

			     {
				     "Key with multi escaped %", "user:///\\\\%", "user:/\\\\%", /* keyName 	*/
				     "\\%",							 /* keyBaseName 	*/
				     "",							 /* keyGetFullRootName 	*/

			     },

			     {
				     NULL, NULL, NULL, /* keyName 	*/
				     NULL,	       /* keyBaseName 	*/
				     NULL,	       /* keyGetFullRootName 	*/
			     } };

static void test_keyNewSpecial (void)
{
	printf ("Test special key creation\n");

	ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (k), "/");
	elektraKeyDel (k);

	k = elektraKeyNew (0, ELEKTRA_KEY_END);
	succeed_if (k == NULL, "should be invalid");
	elektraKeyDel (k);

	k = elektraKeyNew ("", ELEKTRA_KEY_END);
	succeed_if (k == NULL, "should be invalid");
	elektraKeyDel (k);

	k = elektraKeyNew ("invalid", ELEKTRA_KEY_END);
	succeed_if (k == NULL, "should be invalid");
	elektraKeyDel (k);

	k = elektraKeyNew ("other invalid", ELEKTRA_KEY_END);
	succeed_if (k == NULL, "should be invalid");
	elektraKeyDel (k);

	k = elektraKeyNew ("system spaces", ELEKTRA_KEY_END);
	succeed_if (k == NULL, "should be invalid");
	elektraKeyDel (k);

	k = elektraKeyNew ("system:/bin", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_VALUE, "a 2d\0b", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyValue (k), "a 2d");
	succeed_if (elektraKeyGetValueSize (k) == sizeof ("a 2d"), "no KEY_SIZE given, so bin is truncated");
	succeed_if (elektraKeyIsBinary (k), "not a binary key");
	elektraKeyDel (k);
}

static void test_keyNewSystem (void)
{
	ElektraKey * key;
	char array[] = "here is some data stored";
	ElektraKey * k1;
	ElektraKey * k2;
	ElektraKey * k3;
	char * getBack;

	printf ("Test system key creation\n");

	// Empty key
	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (key != NULL, "keyNew: Unable to create a new empty key");
	succeed_if (elektraKeyDel (key) == 0, "keyDel: Unable to delete empty key");

	// Key with name
	key = elektraKeyNew ("system:/sw/test", ELEKTRA_KEY_END);
	succeed_if (key != NULL, "keyNew: Unable to create a key with name");
	succeed_if_same_string (elektraKeyName (key), "system:/sw/test");
	elektraKeyCopy (key, 0, ELEKTRA_KEY_CP_NAME);
	succeed_if_same_string (elektraKeyName (key), "/");
	succeed_if (elektraKeyDel (key) == 0, "keyDel: Unable to delete key with name");

	// Key with name
	key = elektraKeyNew ("system:/sw/test", ELEKTRA_KEY_END);
	succeed_if (key != NULL, "keyNew: Unable to create a key with name");
	succeed_if_same_string (elektraKeyName (key), "system:/sw/test");
	succeed_if (elektraKeyDel (key) == 0, "keyDel: Unable to delete key with name");

	// Key with name + value
	key = elektraKeyNew ("system:/sw/test", ELEKTRA_KEY_VALUE, "test", ELEKTRA_KEY_END);
	succeed_if (key != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if_same_string (elektraKeyValue (key), "test");
	succeed_if (elektraKeyDel (key) == 0, "keyDel: Unable to delete key with name + value");
	key = elektraKeyNew ("system:/valid/there", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (array), ELEKTRA_KEY_VALUE, array, ELEKTRA_KEY_END);
	succeed_if (key != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if (elektraKeyIsBinary (key), "Could not set type to binary");
	succeed_if (elektraKeyGetValueSize (key) == sizeof (array), "Value size not correct");
	succeed_if (memcmp ((char *) elektraKeyValue (key), array, sizeof (array)) == 0, "could not get correct binary value");
	getBack = elektraMalloc (elektraKeyGetValueSize (key));
	elektraKeyGetBinary (key, getBack, elektraKeyGetValueSize (key));
	succeed_if (memcmp (getBack, array, sizeof (array)) == 0, "could not get correct value with keyGetBinary");
	elektraFree (getBack);
	succeed_if (elektraKeyDel (key) == 0, "keyDel: Unable to delete key with name + owner");

	key = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "system:/");
	succeed_if (elektraKeyGetNameSize (key) == 9, "empty name size");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "owner")) == 0, "owner not null");
	elektraKeyDel (key);

	// testing multiple values at once
	k1 = elektraKeyNew ("system:/1", ELEKTRA_KEY_VALUE, "singlevalue", ELEKTRA_KEY_END);
	k2 = elektraKeyNew ("system:/2", ELEKTRA_KEY_VALUE, "myvalue", ELEKTRA_KEY_END);
	k3 = elektraKeyNew ("system:/3", ELEKTRA_KEY_VALUE, "syskey", ELEKTRA_KEY_END);
	succeed_if (k1 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if (elektraKeyIsString (k1), "keyNew: Default key value isn't set to string");
	succeed_if_same_string (elektraKeyValue (k1), "singlevalue");

	succeed_if (k2 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if (elektraKeyIsString (k2), "keyNew: Default key value isn't set to string");
	succeed_if_same_string (elektraKeyValue (k2), "myvalue");

	succeed_if (k3 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if (elektraKeyIsString (k3), "keyNew: Default key value isn't set to string");
	succeed_if_same_string (elektraKeyValue (k3), "syskey");

	succeed_if (elektraKeyDel (k1) == 0, "keyDel: Unable to delete key with name + value");
	succeed_if (elektraKeyDel (k2) == 0, "keyDel: Unable to delete key with name + value");
	succeed_if (elektraKeyDel (k3) == 0, "keyDel: Unable to delete key with name + value");
}

static void test_keyNewUser (void)
{
	ElektraKey * k1;
	ElektraKey * k2;
	ElektraKey * k3;

	printf ("Test user key creation\n");
	// testing multiple values at once
	k1 = elektraKeyNew ("user:/1", ELEKTRA_KEY_VALUE, "singlevalue", ELEKTRA_KEY_END);
	k2 = elektraKeyNew ("user:/2", ELEKTRA_KEY_VALUE, "myvalue", ELEKTRA_KEY_END);
	k3 = elektraKeyNew ("user:/3", ELEKTRA_KEY_VALUE, "syskey", ELEKTRA_KEY_END);
	succeed_if (k1 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if (elektraKeyIsString (k1), "keyNew: Default key value isn't set to string");
	succeed_if_same_string (elektraKeyValue (k1), "singlevalue");

	succeed_if (k2 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if (elektraKeyIsString (k2), "keyNew: Default key value isn't set to string");
	succeed_if_same_string (elektraKeyValue (k2), "myvalue");

	succeed_if (k3 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if (elektraKeyIsString (k3), "keyNew: Default key value isn't set to string");
	succeed_if_same_string (elektraKeyValue (k3), "syskey");

	succeed_if (elektraKeyDel (k1) == 0, "keyDel: Unable to delete key with name + value");
	succeed_if (elektraKeyDel (k2) == 0, "keyDel: Unable to delete key with name + value");
	succeed_if (elektraKeyDel (k3) == 0, "keyDel: Unable to delete key with name + value");

	k1 = elektraKeyNew ("invalid", ELEKTRA_KEY_END);
	succeed_if (k1 == 0, "should not construct key on invalid names");
}

static void test_keyReference (void)
{
	printf ("Test key reference\n");

	ElektraKey * key = elektraKeyNew ("user:/key", ELEKTRA_KEY_END);
	ElektraKey * c = elektraKeyNew ("user:/c", ELEKTRA_KEY_END);
	ElektraKey * d;
	ElektraKeyset *ks1, *ks2;

	succeed_if (elektraKeyGetRef (0) == UINT16_MAX, "No error on getting refcount of NULL Key");
	succeed_if (elektraKeyDecRef (0) == UINT16_MAX, "No error on decrementing NULL Key");
	succeed_if (elektraKeyIncRef (0) == UINT16_MAX, "No error on incrementing NULL Key");

	succeed_if (elektraKeyGetRef (key) == 0, "New created key reference");

	succeed_if (elektraKeyIncRef (key) == 1, "keyIncRef return value");
	succeed_if (elektraKeyGetRef (key) == 1, "After keyIncRef key reference");
	succeed_if (elektraKeyIncRef (key) == 2, "keyIncRef return value");
	succeed_if (elektraKeyGetRef (key) == 2, "After keyIncRef key reference");
	succeed_if (elektraKeyIncRef (key) == 3, "keyIncRef return value");
	succeed_if (elektraKeyGetRef (key) == 3, "After keyIncRef key reference");
	succeed_if (elektraKeyIncRef (key) == 4, "keyIncRef return value");
	succeed_if (elektraKeyGetRef (key) == 4, "After keyIncRef key reference");

	d = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if (elektraKeyGetRef (d) == 0, "After keyDup key reference");
	succeed_if (elektraKeyIncRef (d) == 1, "keyIncRef return value");
	succeed_if (elektraKeyGetRef (key) == 4, "Reference should not change");
	succeed_if (elektraKeyDecRef (d) == 0, "decrement key");
	succeed_if (elektraKeyDel (d) == 0, "last keyDel d, key exist");

	elektraKeyCopy (c, key, ELEKTRA_KEY_CP_ALL);
	succeed_if (elektraKeyGetRef (c) == 0, "After keyCopy key reference");
	succeed_if (elektraKeyIncRef (c) == 1, "keyIncRef return value");
	succeed_if (elektraKeyGetRef (key) == 4, "Reference should not change");

	elektraKeyCopy (c, key, ELEKTRA_KEY_CP_ALL);
	succeed_if (elektraKeyGetRef (c) == 1, "After keyCopy key reference");
	succeed_if (elektraKeyDecRef (c) == 0, "keyDecRef return value");
	succeed_if (elektraKeyGetRef (key) == 4, "Reference should not change");

	succeed_if (elektraKeyIncRef (c) == 1, "keyIncRef return value");
	succeed_if (elektraKeyIncRef (c) == 2, "keyIncRef return value");
	elektraKeyCopy (c, key, ELEKTRA_KEY_CP_ALL);
	succeed_if (elektraKeyGetRef (c) == 2, "After keyCopy key reference");
	succeed_if (elektraKeyDecRef (c) == 1, "keyDecRef return value");
	succeed_if (elektraKeyDecRef (c) == 0, "keyDecRef return value");
	succeed_if (elektraKeyDel (c) == 0, "could not delete copy");

	succeed_if (elektraKeyGetRef (key) == 4, "After keyIncRef key reference");
	succeed_if (elektraKeyDecRef (key) == 3, "keyDel return value");
	succeed_if (elektraKeyDel (key) == 3, "should not do anything");
	succeed_if (elektraKeyGetRef (key) == 3, "After keyIncRef key reference");
	succeed_if (elektraKeyDecRef (key) == 2, "keyDel return value");
	succeed_if (elektraKeyDel (key) == 2, "should not do anything");
	succeed_if (elektraKeyGetRef (key) == 2, "After keyIncRef key reference");
	succeed_if (elektraKeyDecRef (key) == 1, "keyDel return value");
	succeed_if (elektraKeyDel (key) == 1, "should not do anything");
	succeed_if (elektraKeyGetRef (key) == 1, "Should have no more reference");
	succeed_if (elektraKeyDecRef (key) == 0, "last keyDel key, key exist");
	succeed_if (elektraKeyDel (key) == 0, "last keyDel key, key exist");

	/* From examples in ksNew () */
	key = elektraKeyNew ("/", ELEKTRA_KEY_END); // ref counter 0
	succeed_if (elektraKeyGetRef (key) == 0, "reference counter");
	elektraKeyIncRef (key); // ref counter of key 1
	succeed_if (elektraKeyGetRef (key) == 1, "reference counter");
	elektraKeyDel (key); // has no effect
	succeed_if (elektraKeyGetRef (key) == 1, "reference counter");
	elektraKeyDecRef (key); // ref counter back to 0
	succeed_if (elektraKeyGetRef (key) == 0, "reference counter");
	elektraKeyDel (key); // key is now deleted

	ks1 = elektraKeysetNew (0, ELEKTRA_KS_END);
	ks2 = elektraKeysetNew (0, ELEKTRA_KS_END);
	key = elektraKeyNew ("user:/key", ELEKTRA_KEY_END); // ref counter 0
	succeed_if (elektraKeyGetRef (key) == 0, "reference counter");
	elektraKeysetAppendKey (ks1, key); // ref counter of key 1
	succeed_if (elektraKeyGetRef (key) == 1, "reference counter");
	elektraKeysetAppendKey (ks2, key); // ref counter of key 2
	succeed_if (elektraKeyGetRef (key) == 2, "reference counter");
	elektraKeysetDel (ks1); // ref counter of key 1
	succeed_if (elektraKeyGetRef (key) == 1, "reference counter");
	elektraKeysetDel (ks2); // key is now deleted

	key = elektraKeyNew ("/", ELEKTRA_KEY_END); // ref counter 0
	succeed_if (elektraKeyGetRef (key) == 0, "reference counter");
	elektraKeyIncRef (key); // ref counter of key 1
	succeed_if (elektraKeyGetRef (key) == 1, "reference counter");
	elektraKeyDel (key); // has no effect
	succeed_if (elektraKeyGetRef (key) == 1, "reference counter");
	elektraKeyIncRef (key); // ref counter of key 2
	succeed_if (elektraKeyGetRef (key) == 2, "reference counter");
	elektraKeyDel (key); // has no effect
	succeed_if (elektraKeyGetRef (key) == 2, "reference counter");
	elektraKeyDecRef (key); // ref counter of key 1
	succeed_if (elektraKeyGetRef (key) == 1, "reference counter");
	elektraKeyDel (key); // has no effect
	succeed_if (elektraKeyGetRef (key) == 1, "reference counter");
	elektraKeyDecRef (key); // ref counter is now 0
	succeed_if (elektraKeyGetRef (key) == 0, "reference counter");
	elektraKeyDel (key); // key is now deleted

	ElektraKey * k = elektraKeyNew ("system:/proper_name", ELEKTRA_KEY_END); // ref counter = 0
	succeed_if (elektraKeyGetRef (k) == 0, "ref should be zero");
	ElektraKeyset * ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);
	succeed_if (elektraKeyGetRef (k) == 1, "ref should be one");
	succeed_if (elektraKeyDel (k) == 1, "key will not be deleted, because its in the keyset");
	succeed_if (elektraKeyGetRef (k) == 1, "ref should be one");
	succeed_if (elektraKeysetDel (ks) == 0, "could not del"); // now the key will be deleted

	key = elektraKeyNew ("/", ELEKTRA_KEY_END); // ref counter 0
	while (elektraKeyGetRef (key) < UINT16_MAX - 1)
		elektraKeyIncRef (key);
	succeed_if (elektraKeyGetRef (key) == UINT16_MAX - 1, "reference counter");
	succeed_if (elektraKeyIncRef (key) == UINT16_MAX, "should report error");
	succeed_if (elektraKeyGetRef (key) == UINT16_MAX - 1, "reference counter");
	succeed_if (elektraKeyIncRef (key) == UINT16_MAX, "should report error");
	while (elektraKeyGetRef (key) > 0)
		elektraKeyDecRef (key);
	elektraKeyDel (key);
}

static void test_keyName (void)
{
	ElektraKey * key;
	char ret[1000];
	size_t i;
	char testName[] = "user:/name";
	char testBaseName[] = "name";

#ifdef HAVE_CLEARENV
	clearenv ();
#else
	unsetenv ("USER");
#endif

	printf ("Test Key Name\n");

	key = elektraKeyNew (testName, ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetName (0, ret, 100) == -1, "null pointer");
	succeed_if (elektraKeyGetName (key, 0, 100) == -1, "string null pointer");
	succeed_if (elektraKeyGetName (key, ret, 0) == -1, "length checking");
	for (i = 1; i < sizeof (testName); i++)
	{
		succeed_if (elektraKeyGetName (key, ret, i) == -1, "length checking too short");
	}
	for (i = sizeof (testName); i < sizeof (testName) * 2; i++)
	{
		succeed_if (elektraKeyGetName (key, ret, i) == sizeof (testName), "length checking longer");
	}
	succeed_if (elektraKeyGetName (key, ret, (size_t) -1) == -1, "maxSize exceeded");
	elektraKeyDel (key);

	succeed_if (elektraKeyName (0) == 0, "null pointer");

	succeed_if (elektraKeySetName (0, ret) == -1, "Null pointer");


	printf ("Test Key Base Name\n");

	key = elektraKeyNew (testName, ELEKTRA_KEY_END);

	succeed_if (elektraKeyGetBaseName (0, ret, 100) == -1, "null pointer");
	succeed_if (elektraKeyGetBaseName (key, 0, 100) == -1, "string null pointer");
	succeed_if (elektraKeyGetBaseName (key, ret, 0) == -1, "length checking");

	succeed_if (elektraKeyGetBaseNameSize (0) == -1, "no error on passing NULL pointer");

	for (i = 1; i < sizeof (testBaseName); i++)
	{
		succeed_if (elektraKeyGetBaseName (key, ret, i) == -1, "length checking too short");
	}

	for (i = sizeof (testBaseName); i < sizeof (testBaseName) * 2; i++)
	{
		succeed_if (elektraKeyGetBaseName (key, ret, i) == sizeof (testBaseName), "length checking longer");
	}

	succeed_if (elektraKeyGetBaseName (key, ret, (size_t) -1) == -1, "maxSize exceeded");
	elektraKeyDel (key);


	succeed_if (elektraKeyBaseName (0) == 0, "null pointer");

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyBaseName (key), "");
	succeed_if (elektraKeyGetBaseName (key, ret, 1000) == 1, "get empty name");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 0) == -1, "get empty name");
	elektraKeyDel (key);

	succeed_if (elektraKeySetName (0, ret) == -1, "Null pointer");

	key = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "user:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "user:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("user://", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "user:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "system:/");
	succeed_if (elektraKeyGetNameSize (key) == 9, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "system:/");
	succeed_if (elektraKeyGetNameSize (key) == 9, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("dir:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "dir:/");
	succeed_if (elektraKeyGetNameSize (key) == 6, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("dir:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "dir:/");
	succeed_if (elektraKeyGetNameSize (key) == 6, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("proc:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "proc:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("proc:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "proc:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("spec:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "spec:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("spec:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "spec:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("meta:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "meta:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("default:/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "default:/");
	succeed_if (elektraKeyGetNameSize (key) == 10, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "/");
	succeed_if (elektraKeyGetNameSize (key) == 2, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);

	key = elektraKeyNew ("//", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (key), "/");
	succeed_if (elektraKeyGetNameSize (key) == 2, "name length checking");
	succeed_if (elektraKeyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (elektraKeyGetBaseName (key, ret, 1) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	succeed_if (elektraKeyGetBaseName (key, ret, 2) == 1, "GetBaseName for root key");
	succeed_if_same_string (ret, "");
	elektraKeyDel (key);
}


static void test_keyNameSlashes (void)
{
	printf ("Test Slashes in Key Name\n");
	char * buf;
	char * getBack;
	ElektraKey * key = 0;
	int i;


	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNameSize (key) == 2, "empty name size");
	elektraKeyDel (key);

	key = elektraKeyNew ("", ELEKTRA_KEY_END);
	succeed_if (key == 0, "key should be null!");

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetName (key, "user:/");
	succeed_if_same_string (elektraKeyName (key), "user:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "empty name size");

	elektraKeySetName (key, "system:/");
	succeed_if_same_string (elektraKeyName (key), "system:/");
	succeed_if (elektraKeyGetNameSize (key) == 9, "empty name size");
	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetName (key, "system:/");
	succeed_if_same_string (elektraKeyName (key), "system:/");
	succeed_if (elektraKeyGetNameSize (key) == 9, "empty name size");

	elektraKeySetName (key, "user:/");
	succeed_if_same_string (elektraKeyName (key), "user:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "empty name size");
	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetName (key, "user:/") == 7, "setting user:/ generates error");
	succeed_if_same_string (elektraKeyName (key), "user:/");
	succeed_if (elektraKeyGetNameSize (key) == 7, "empty name size");
	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetName (key, "no") == -1, "no error code setting invalid name");
	succeed_if_same_string (elektraKeyName (key), "/");
	succeed_if (elektraKeyGetNameSize (key) == 2, "empty name size");
	elektraKeyDel (key);

	key = elektraKeyNew ("user:/noname", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNameSize (key) == 13, "size not correct after keyNew");
	getBack = elektraMalloc (13);
	succeed_if (elektraKeyGetName (key, getBack, 13), "could not get name");
	succeed_if_same_string (getBack, "user:/noname");
	elektraFree (getBack);

	elektraKeySetName (key, "user:/noname");
	succeed_if (elektraKeyGetNameSize (key) == 13, "size not correct after keySetName");
	getBack = elektraMalloc (13);
	succeed_if (elektraKeyGetName (key, getBack, 13), "could not get name");
	succeed_if_same_string (getBack, "user:/noname");
	elektraFree (getBack);

	elektraKeySetName (key, "no");
	succeed_if (elektraKeyGetNameSize (key) == 13, "size not correct after keySetName");
	getBack = elektraMalloc (13);
	succeed_if (elektraKeyGetName (key, getBack, 13), "could not get name");
	succeed_if_same_string (getBack, "user:/noname");
	elektraFree (getBack);
	elektraKeyDel (key);

	key = elektraKeyNew ("user:/noname", ELEKTRA_KEY_END);

	elektraKeySetName (key, "user://hidden");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");
	succeed_if (elektraKeyGetNameSize (key) == 13, "name size minus slashes");

	elektraKeySetName (key, "user:///hidden");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");
	succeed_if (elektraKeyGetNameSize (key) == 13, "name size minus slashes");

	elektraKeySetName (key, "user:////////////////////////////////////hidden");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");
	succeed_if (elektraKeyGetNameSize (key) == 13, "name size minus slashes");

	printf ("Test trailing Slashes in Key Name\n");
	elektraKeySetName (key, "user://hidden/");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");
	succeed_if (elektraKeyGetNameSize (key) == 13, "name size minus slashes");

	elektraKeySetName (key, "user://hidden//");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");
	succeed_if (elektraKeyGetNameSize (key) == 13, "name size minus slashes");

	elektraKeySetName (key, "user://hidden///////");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");
	succeed_if (elektraKeyGetNameSize (key) == 13, "name size minus slashes");

	elektraKeySetName (key, "user:/");
	succeed_if_same_string (elektraKeyName (key), "user:/");

	elektraKeySetName (key, "user:/");
	succeed_if_same_string (elektraKeyName (key), "user:/");

	elektraKeySetName (key, "user:/a");
	succeed_if_same_string (elektraKeyName (key), "user:/a");

	elektraKeySetName (key, "user://");
	succeed_if_same_string (elektraKeyName (key), "user:/");

	elektraKeySetName (key, "user://///////");
	succeed_if_same_string (elektraKeyName (key), "user:/");

	printf ("Test Dots in Key Name\n");
	elektraKeySetName (key, "user:/hidden/.");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");

	elektraKeySetName (key, "user:/.");
	succeed_if_same_string (elektraKeyName (key), "user:/");

	elektraKeySetName (key, "user:/./hidden");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");

	elektraKeySetName (key, "user:/.valid/.");
	succeed_if_same_string (elektraKeyName (key), "user:/.valid");

	elektraKeySetName (key, "user:/./.valid");
	succeed_if_same_string (elektraKeyName (key), "user:/.valid");

	elektraKeySetName (key, "user:/./.valid/.");
	succeed_if_same_string (elektraKeyName (key), "user:/.valid");

	elektraKeySetName (key, "user:/././././.valid/././././.");
	succeed_if_same_string (elektraKeyName (key), "user:/.valid");

	printf ("Test Double Dots in Key Name\n");
	elektraKeySetName (key, "user:/hidden/parent/..");
	succeed_if_same_string (elektraKeyName (key), "user:/hidden");

	elektraKeySetName (key, "user:/hidden/..");
	succeed_if_same_string (elektraKeyName (key), "user:/");

	elektraKeySetName (key, "user:/..");
	succeed_if_same_string (elektraKeyName (key), "user:/");

	elektraKeySetName (key, "user:/hidden/../..");
	// printf ("Name: %s\n", keyName(key));
	succeed_if_same_string (elektraKeyName (key), "user:/");

	succeed_if (elektraKeySetName (key, "user:///sw/../sw//././MyApp") == sizeof ("user:/sw/MyApp"), "could not set keySet example");
	// printf("%s %d\n", keyName(key), keyGetNameSize(key));
	succeed_if_same_string (elektraKeyName (key), "user:/sw/MyApp");
	succeed_if (elektraKeyGetNameSize (key) == sizeof ("user:/sw/MyApp"), "incorrect length for keySet example");

	printf ("Test Mixed Dots and Slashes in Key Name\n");
	elektraKeySetName (key, "user:/hidden/../.");

	elektraKeyDel (key);


	printf ("Test failure key creation\n");

	key = elektraKeyNew ("invalid", ELEKTRA_KEY_END);
	succeed_if (key == 0, "should be null");
	succeed_if (elektraKeyDel (key) == -1, "keyDel: should fail");

	key = elektraKeyNew ("nonhere/valid/there", ELEKTRA_KEY_END);
	succeed_if (key == 0, "should be null");
	succeed_if (elektraKeyDel (key) == -1, "keyDel: should fail");

	key = elektraKeyNew ("nonhere:y/valid/there", ELEKTRA_KEY_END);
	succeed_if (key == 0, "should be null");
	succeed_if (elektraKeyDel (key) == -1, "keyDel: should fail");

	key = elektraKeyNew ("user:/validname", ELEKTRA_KEY_END);
	succeed_if (key != NULL, "keyNew: Unable to create a key with name");
	succeed_if_same_string (elektraKeyName (key), "user:/validname");

	elektraKeySetName (key, "user:/validname");
	succeed_if_same_string (elektraKeyName (key), "user:/validname");

	elektraKeySetName (key, "user:/validname\\/t");
	succeed_if_same_string (elektraKeyName (key), "user:/validname\\/t");

#ifdef COMPAT
	elektraKeySetName (key, "user:/validname\\");
	succeed_if_same_string (elektraKeyName (key), "user:/validname\\");
#endif

	elektraKeySetName (key, "user:/validname\\/");
	succeed_if_same_string (elektraKeyName (key), "user:/validname\\/");
	succeed_if (elektraKeyDel (key) == 0, "keyDel: Unable to delete key with name");

	printf ("Test key's name manipulation\n");

	ElektraKey * copy = elektraKeyNew ("/", ELEKTRA_KEY_END);

	for (i = 0; tstKeyName[i].testName != NULL; i++)
	{
		key = elektraKeyNew (tstKeyName[i].keyName, ELEKTRA_KEY_END);

		succeed_if (elektraKeyGetRef (copy) == 0, "reference of copy not correct");
		elektraKeyCopy (copy, key, ELEKTRA_KEY_CP_ALL);
		succeed_if (elektraKeyGetRef (copy) == 0, "reference of copy not correct");

		ElektraKey * dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
		succeed_if (elektraKeyGetRef (dup) == 0, "reference of dup not correct");

		compare_key (copy, key);
		compare_key (copy, dup);
		compare_key (dup, key);

		/* keyName */
		succeed_if_same_string (elektraKeyName (key), tstKeyName[i].expectedKeyName);
		succeed_if_same_string (elektraKeyName (copy), tstKeyName[i].expectedKeyName);
		succeed_if_same_string (elektraKeyName (dup), tstKeyName[i].expectedKeyName);

		/* keyBaseName */
		succeed_if_same_string (elektraKeyBaseName (key), tstKeyName[i].expectedBaseName);
		succeed_if_same_string (elektraKeyBaseName (copy), tstKeyName[i].expectedBaseName);
		succeed_if_same_string (elektraKeyBaseName (dup), tstKeyName[i].expectedBaseName);

		/* keyGetBaseNameSize */
		size_t size = elektraKeyGetBaseNameSize (key);
		succeed_if ((size == strlen (tstKeyName[i].expectedBaseName) + 1), "keyGetBaseNameSize");

		/* keyGetBaseName */
		size = elektraKeyGetBaseNameSize (key) + 1;
		buf = elektraMalloc (size * sizeof (char));
		elektraKeyGetBaseName (key, buf, size);
		succeed_if_same_string (buf, tstKeyName[i].expectedBaseName);
		elektraFree (buf);

		/* keyGetNameSize */
		size = elektraKeyGetNameSize (key);
		succeed_if ((size == strlen (tstKeyName[i].expectedKeyName) + 1), "keyGetKeyNameSize");

		/* keyGetName */
		size = elektraKeyGetNameSize (key);
		buf = elektraMalloc (size * sizeof (char));
		elektraKeyGetName (key, buf, size);
		succeed_if_same_string (buf, tstKeyName[i].expectedKeyName);
		elektraFree (buf);

		elektraKeyDel (key);
		elektraKeyDel (dup);
	}
	elektraKeyDel (copy);
}


static void test_keyValue (void)
{
	ElektraKey * key;
	char ret[1000];
	size_t i;
	char testString[] = "teststring";
	char testBinary[] = "\0tes\1tbinary";
	testBinary[sizeof (testBinary) - 1] = 'T';

	printf ("Test value of keys\n");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if (elektraKeyGetValueSize (key) == 1, "empty value size");
	succeed_if (elektraKeySetString (key, "perfectvalue") == 13, "could not set string");
	succeed_if (elektraKeyGetValueSize (key) == 13, "value size not correct");
	succeed_if_same_string (elektraKeyValue (key), "perfectvalue");
	succeed_if (elektraKeySetString (key, "perfectvalue") == 13, "could not re-set same string");
	succeed_if_same_string (elektraKeyValue (key), "perfectvalue");
	succeed_if (elektraKeySetString (key, "nearperfectvalue") == 17, "could not re-set other string");
	succeed_if (elektraKeyGetValueSize (key) == 17, "value size not correct");
	succeed_if_same_string (elektraKeyValue (key), "nearperfectvalue");
	succeed_if (elektraKeyGetString (key, ret, elektraKeyGetValueSize (key) >= 999 ? 999 : elektraKeyGetValueSize (key)) == 17, "could not get string");
	succeed_if_same_string (ret, "nearperfectvalue");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if_same_string (elektraKeyValue (key), "");
	succeed_if (elektraKeyGetValueSize (key) == 1, "Empty value size problem");
	succeed_if (elektraKeySetString (key, "") == 1, "could not set empty string");
	succeed_if_same_string (elektraKeyValue (key), "");
	succeed_if (elektraKeyGetValueSize (key) == 1, "Empty value size problem");
	succeed_if (elektraKeyGetString (key, ret, 0) == -1, "Could not get empty value");
	succeed_if (elektraKeyGetString (key, ret, 1) == 1, "Could not get empty value");
	succeed_if (ret[0] == 0, "keyGetValue did not return empty value");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if (elektraKeySetString (key, "a long long string") == 19, "could not set string");
	succeed_if (elektraKeyGetString (key, ret, 6) == -1, "string not truncated");
	succeed_if (elektraKeyGetBinary (key, ret, 999) == -1, "binary not mismatch");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if (elektraKeySetBinary (key, "a", 1) == 1, "could not set binary");
	succeed_if (elektraKeyIsString (key) == 0, "is not a string");
	succeed_if (elektraKeyIsBinary (key) == 1, "is not a string");
	succeed_if (elektraKeyGetBinary (key, ret, 1) == 1, "binary not truncated");
	succeed_if (!strncmp (ret, "a", 1), "binary value wrong");
	succeed_if (elektraKeyGetString (key, ret, 999) == -1, "string not mismatch");
	succeed_if (elektraKeySetString (key, 0) == 1, "wrong error code for SetString");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if (elektraKeySetBinary (key, NULL, 0) == 0, "could not set null binary");
	succeed_if (elektraKeyIsString (key) == 0, "is not a string");
	succeed_if (elektraKeyIsBinary (key) == 1, "is not a string");
	succeed_if (elektraKeyGetValueSize (key) == 0, "Empty value size problem");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if (elektraKeySetString (key, "") == 1, "could not set empty string");
	succeed_if (elektraKeyIsString (key) == 1, "is not a string");
	succeed_if (elektraKeyIsBinary (key) == 0, "is a binary");
	succeed_if (elektraKeyGetValueSize (key) == 1, "Empty value size problem");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if (elektraKeySetBinary (key, "a long long binary", 19) == 19, "could not set string");
	succeed_if (elektraKeyIsString (key) == 0, "is not a string");
	succeed_if (elektraKeyIsBinary (key) == 1, "is not a string");
	succeed_if (elektraKeyGetBinary (key, ret, 6) == -1, "binary not truncated");
	succeed_if (elektraKeyGetBinary (key, ret, 19) == 19, "could not get binary");
	succeed_if (!strncmp (ret, "a long long binary", 19), "binary value wrong");
	succeed_if (elektraKeyGetString (key, ret, 999) == -1, "string not mismatch");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	for (i = 1; i < 255; i++)
	{
		ret[0] = i;
		ret[1] = i;
		ret[2] = 0;
		// output_key (key);
		succeed_if (elektraKeySetString (key, ret) == 3, "could not set string");
		succeed_if_same_string (elektraKeyValue (key), ret);
	}
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");


	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	for (i = 0; i < 255; i++)
	{
		ret[0] = i;
		ret[1] = 255 - i;
		ret[2] = i;
		// output_key (key);
		succeed_if (elektraKeySetBinary (key, ret, 3) == 3, "could not set string");
		succeed_if (memcmp (elektraKeyValue (key), ret, 3) == 0, "String not same as set");
	}
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");


	printf ("Test string of key\n");

	succeed_if (elektraKeyIsString (0) == -1, "no error on NULL key");

	succeed_if (elektraKeyValue (0) == 0, "null pointer");
	succeed_if (elektraKeyGetValueSize (0) == -1, "null pointer");
	succeed_if (elektraKeySetString (0, "") == -1, "null pointer");
	succeed_if_same_string (elektraKeyString (0), "(null)");

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetValueSize (key) == 1, "empty value size");
	succeed_if_same_string (elektraKeyString (key), "");

	elektraKeySetString (key, testString);
	succeed_if (elektraKeyGetString (0, ret, 100) == -1, "null pointer");
	succeed_if (elektraKeyGetString (key, 0, 100) == -1, "string null pointer");
	succeed_if (elektraKeyGetString (key, ret, 0) == -1, "length checking");
	succeed_if_same_string (elektraKeyString (key), testString);

	for (i = 1; i < sizeof (testString); i++)
	{
		succeed_if (elektraKeyGetString (key, ret, i) == -1, "length checking too short");
	}
	for (i = sizeof (testString); i < sizeof (testString) * 2; i++)
	{
		succeed_if (elektraKeyGetString (key, ret, i) == sizeof (testString), "length checking longer");
	}
	succeed_if (elektraKeyGetString (key, ret, (size_t) -1) == -1, "maxSize exceeded");

	succeed_if (elektraKeySetString (key, 0) == 1, "delete string");
	succeed_if (elektraKeyGetString (key, ret, i) == 1, "length checking deleting");
	succeed_if_same_string (ret, "");

	succeed_if (elektraKeySetString (key, testString) == sizeof (testString), "set string");
	succeed_if (elektraKeyGetString (key, ret, i) == sizeof (testString), "length checking working");
	succeed_if_same_string (ret, testString);

	succeed_if (elektraKeySetString (key, "") == 1, "delete string");
	succeed_if (elektraKeyGetString (key, ret, i) == 1, "length checking deleting");
	succeed_if_same_string (ret, "");

	succeed_if (elektraKeySetString (key, testString) == sizeof (testString), "set string");
	succeed_if (elektraKeyGetString (key, ret, i) == sizeof (testString), "length checking working");
	succeed_if_same_string (ret, testString);

	succeed_if (elektraKeyGetValueSize (key) == sizeof (testString), "testString value size");
	succeed_if (strncmp (elektraKeyValue (key), testString, sizeof (testString)) == 0, "testString not same");
	elektraKeyDel (key);


	printf ("Test binary of key\n");

	succeed_if (elektraKeyValue (0) == 0, "null pointer");
	succeed_if (elektraKeyGetValueSize (0) == -1, "null pointer");
	succeed_if (elektraKeySetBinary (0, "", 1) == -1, "null pointer");

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetBinary (key, "", 0) == -1, "null size");
	succeed_if (elektraKeySetBinary (key, "b", 0) == -1, "null size");
	succeed_if (elektraKeySetBinary (key, "", SIZE_MAX) == -1, "max size");
	succeed_if (elektraKeySetBinary (key, "b", SIZE_MAX) == -1, "max size");
	succeed_if (elektraKeyGetValueSize (key) == 1, "empty value size");

	elektraKeySetBinary (key, testBinary, sizeof (testBinary));
	succeed_if (elektraKeyGetBinary (0, ret, 100) == -1, "null pointer");
	succeed_if (elektraKeyGetBinary (key, 0, 100) == -1, "binary null pointer");
	succeed_if (elektraKeyGetBinary (key, ret, 0) == -1, "length checking");

	for (i = 1; i < sizeof (testBinary); i++)
	{
		succeed_if (elektraKeyGetBinary (key, ret, i) == -1, "length checking too short");
	}
	for (i = sizeof (testBinary); i < sizeof (testBinary) * 2; i++)
	{
		succeed_if (elektraKeyGetBinary (key, ret, i) == sizeof (testBinary), "length checking longer");
	}
	succeed_if (elektraKeyGetBinary (key, ret, (size_t) -1) == -1, "maxSize exceeded");

	succeed_if (elektraKeySetBinary (key, 0, 0) == 0, "delete binary");
	succeed_if (elektraKeyGetBinary (key, ret, i) == 0, "length checking deleting");

	succeed_if (elektraKeySetBinary (key, testBinary, sizeof (testBinary)) == sizeof (testBinary), "set binary");
	succeed_if (elektraKeyGetBinary (key, ret, i) == sizeof (testBinary), "length checking working");
	succeed_if_same_string (ret, testBinary);

	succeed_if (elektraKeySetBinary (key, 0, 1) == 0, "delete binary");
	succeed_if (elektraKeyGetBinary (key, ret, i) == 0, "length checking deleting");

	succeed_if (elektraKeySetBinary (key, testBinary, sizeof (testBinary)) == sizeof (testBinary), "set binary");
	succeed_if (elektraKeyGetBinary (key, ret, i) == sizeof (testBinary), "length checking working");
	succeed_if_same_string (ret, testBinary);

	succeed_if (elektraKeySetBinary (key, "", 1) == 1, "delete binary the string way");
	succeed_if (elektraKeyGetBinary (key, ret, i) == 1, "length checking deleting string way");
	succeed_if_same_string (ret, "");

	succeed_if (elektraKeySetBinary (key, testBinary, sizeof (testBinary)) == sizeof (testBinary), "set binary");
	succeed_if (elektraKeyGetBinary (key, ret, i) == sizeof (testBinary), "length checking working");
	succeed_if_same_string (ret, testBinary);

	succeed_if (elektraKeyGetValueSize (key) == sizeof (testBinary), "testBinary value size");
	succeed_if (strncmp (elektraKeyValue (key), testBinary, sizeof (testBinary)) == 0, "testBinary not same");
	elektraKeyDel (key);
}

static void test_keyBinary (void)
{
	ElektraKey * key = 0;
	char ret[1000];
	int i;
	char binaryData[] = "\0binary \1\34data";
	binaryData[sizeof (binaryData) - 1] = 'T';

	printf ("Test binary special cases\n");

	succeed_if (elektraKeyIsBinary (0) == -1, "no error on checking NULL Key");

	key = elektraKeyNew ("user:/binary", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_END);

	succeed_if (elektraKeyIsBinary (key) == 1, "should be binary");
	succeed_if (elektraKeyIsString (key) == 0, "should not be string");
	succeed_if (elektraKeyGetValueSize (key) == 0, "size not correct");
	succeed_if (elektraKeyValue (key) == 0, "should be null key");
	succeed_if (elektraKeyGetString (key, ret, 1000) == -1, "should be type mismatch");

	elektraKeyDel (key);

	key = elektraKeyNew ("user:/binary", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (binaryData), ELEKTRA_KEY_VALUE, binaryData, ELEKTRA_KEY_END);

	succeed_if (elektraKeyIsBinary (key) == 1, "should be binary");
	succeed_if (elektraKeyIsString (key) == 0, "should not be string");
	succeed_if (elektraKeyGetValueSize (key) == sizeof (binaryData), "size not correct");
	succeed_if (memcmp (binaryData, elektraKeyValue (key), sizeof (binaryData)) == 0, "memcmp");
	succeed_if (elektraKeyGetBinary (key, ret, 1000) == sizeof (binaryData), "could not get binary data");
	succeed_if (memcmp (binaryData, ret, sizeof (binaryData)) == 0, "memcmp");
	succeed_if (elektraKeyGetString (key, ret, 1000) == -1, "should be type mismatch");
	succeed_if_same_string (elektraKeyString (key), "(binary)");

	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetBinary (key, binaryData, sizeof (binaryData));

	succeed_if (elektraKeyIsBinary (key) == 1, "should be binary");
	succeed_if (elektraKeyIsString (key) == 0, "should not be string");
	succeed_if (elektraKeyGetValueSize (key) == sizeof (binaryData), "size not correct");
	succeed_if (memcmp (binaryData, elektraKeyValue (key), sizeof (binaryData)) == 0, "memcmp");
	succeed_if (elektraKeyGetBinary (key, ret, 1000) == sizeof (binaryData), "could not get binary data");
	succeed_if (memcmp (binaryData, ret, sizeof (binaryData)) == 0, "memcmp");
	succeed_if (elektraKeyGetString (key, ret, 1000) == -1, "should be type mismatch");
	succeed_if_same_string (elektraKeyString (key), "(binary)");

	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetBinary (key, 0, 0);

	succeed_if (elektraKeyIsBinary (key) == 1, "should be binary");
	succeed_if (elektraKeyIsString (key) == 0, "should not be string");
	succeed_if (elektraKeyGetValueSize (key) == 0, "size not correct");
	succeed_if (elektraKeyValue (key) == 0, "should be null pointer");
	succeed_if (elektraKeyGetBinary (key, ret, 1000) == 0, "should write nothing because of no data");
	succeed_if (elektraKeyGetString (key, ret, 1000) == -1, "should be type mismatch");

	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetBinary (key, 0, 1);

	succeed_if (elektraKeyIsBinary (key) == 1, "should be binary");
	succeed_if (elektraKeyIsString (key) == 0, "should not be string");
	succeed_if (elektraKeyGetValueSize (key) == 0, "size not correct");
	succeed_if (elektraKeyValue (key) == 0, "should be null pointer");
	succeed_if (elektraKeyGetBinary (key, ret, 1000) == 0, "should write nothing because of no data");
	succeed_if (elektraKeyGetString (key, ret, 1000) == -1, "should be type mismatch");

	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetBinary (key, "", 1);
	succeed_if (elektraKeySetBinary (key, 0, SIZE_MAX) == -1, "should do nothing and fail");
	succeed_if (elektraKeySetBinary (key, 0, SSIZE_MAX) == 0, "should free data");

	succeed_if (elektraKeyIsBinary (key) == 1, "should be binary");
	succeed_if (elektraKeyIsString (key) == 0, "should not be string");
	succeed_if (elektraKeyGetValueSize (key) == 0, "size not correct");
	succeed_if (elektraKeyValue (key) == 0, "should be null pointer");
	succeed_if (elektraKeyGetBinary (key, ret, 1000) == 0, "should write nothing because of no data");
	succeed_if (elektraKeyGetString (key, ret, 1000) == -1, "should be type mismatch");

	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetBinary (key, "", 1);

	succeed_if (elektraKeyIsBinary (key) == 1, "should be binary");
	succeed_if (elektraKeyIsString (key) == 0, "should not be string");
	succeed_if (elektraKeyGetValueSize (key) == 1, "size not correct");
	succeed_if (memcmp (binaryData, elektraKeyValue (key), 1) == 0, "memcmp");
	succeed_if (elektraKeyGetBinary (key, ret, 1000) == 1, "could not get binary data");
	succeed_if (memcmp (binaryData, ret, 1) == 0, "memcmp");
	succeed_if (elektraKeyGetString (key, ret, 1000) == -1, "should be type mismatch");
	succeed_if_same_string (elektraKeyString (key), "(binary)");

	elektraKeyDel (key);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	i = 23;
	elektraKeySetBinary (key, (void *) &i, sizeof (i));

	succeed_if (elektraKeyIsBinary (key) == 1, "should be binary");
	succeed_if (elektraKeyIsString (key) == 0, "should not be string");
	succeed_if (elektraKeyGetValueSize (key) == sizeof (i), "size not correct");
	succeed_if (memcmp ((void *) &i, elektraKeyValue (key), sizeof (i)) == 0, "memcmp");
	succeed_if (elektraKeyGetBinary (key, ret, 1000) == sizeof (i), "could not get binary data");
	succeed_if (memcmp ((void *) &i, ret, sizeof (i)) == 0, "memcmp");
	succeed_if (elektraKeyGetString (key, ret, 1000) == -1, "should be type mismatch");
	succeed_if_same_string (elektraKeyString (key), "(binary)");

	i = *(int *) elektraKeyValue (key);
	succeed_if (i == 23, "incorrect int");

	elektraKeyDel (key);
}

static void test_keyBelow (void)
{
	ElektraKey * key1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * key2 = elektraKeyNew ("/", ELEKTRA_KEY_END);

	printf ("Test of relative positions of keys\n");

	succeed_if (elektraKeyIsBelow (key1, 0) == -1, "NULL pointer");
	succeed_if (elektraKeyIsBelow (0, 0) == -1, "NULL pointer");
	succeed_if (elektraKeyIsBelow (0, key1) == -1, "NULL pointer");

	succeed_if (elektraKeyIsDirectlyBelow (key1, 0) == -1, "NULL pointer");
	succeed_if (elektraKeyIsDirectlyBelow (0, 0) == -1, "NULL pointer");
	succeed_if (elektraKeyIsDirectlyBelow (0, key1) == -1, "NULL pointer");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/below");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/b");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/b");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/b/e");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/valid");
	elektraKeySetName (key2, "/valid/b/e");
	succeed_if (elektraKeyIsBelow (key1, key2), "cascading Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "cascading Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "/valid/b/e");
	succeed_if (elektraKeyIsBelow (key1, key2), "cascading Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "cascading Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "/v");
	succeed_if (elektraKeyIsBelow (key1, key2), "cascading Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "cascading Key should not be below");

	elektraKeySetName (key1, "user:/");
	elektraKeySetName (key2, "user:/");
	succeed_if (!elektraKeyIsBelow (key1, key2), "root Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "root Key should not be below");

	elektraKeySetName (key1, "dir:/");
	elektraKeySetName (key2, "dir:/");
	succeed_if (!elektraKeyIsBelow (key1, key2), "root Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "root Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "dir:/");
	succeed_if (!elektraKeyIsBelow (key1, key2), "root Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "root Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "system:/");
	succeed_if (!elektraKeyIsBelow (key1, key2), "root Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "root Key should not be below");

	elektraKeySetName (key1, "user:/a");
	elektraKeySetName (key2, "user:/a");
	succeed_if (!elektraKeyIsBelow (key1, key2), "cascading Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "cascading Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "/");
	succeed_if (!elektraKeyIsBelow (key1, key2), "cascading Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "cascading Key should not be below");

	elektraKeySetName (key1, "user:/valide");
	elektraKeySetName (key2, "user:/valid/e");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid/b");
	elektraKeySetName (key2, "user:/valid/e");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valide");
	elektraKeySetName (key2, "user:/valid/valide");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "system:/infos");
	elektraKeySetName (key2, "/infos/constants");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "/infos/constants");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "system:/");
	elektraKeySetName (key2, "/infos/constants");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/infos");
	elektraKeySetName (key2, "system:/infos/constants");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "system:/infos/constants");
	elektraKeySetName (key2, "/infos/constants/syste");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "system:/infos/constants");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/infos");
	elektraKeySetName (key2, "/infos/constants");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "system:/infos/constants/version/version");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid\\/e");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid\\/");
	elektraKeySetName (key2, "user:/valid/e");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/valid\\/");
	elektraKeySetName (key2, "user:/valid\\//valid");
	succeed_if (elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/a/b/c");
	elektraKeySetName (key2, "/a/b/d");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/a");
	elektraKeySetName (key2, "/b");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/valid\\/");
	elektraKeySetName (key2, "user:/valid\\/valid");
	succeed_if (!elektraKeyIsBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelow (key2, key1), "Key should not be below");


	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/valide");
	succeed_if (elektraKeyIsDirectlyBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/non/valid");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid/a");
	elektraKeySetName (key2, "user:/valid/b");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid\\/");
	elektraKeySetName (key2, "user:/valid\\a");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid\\/");
	elektraKeySetName (key2, "user:/valid\\/a");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid\\/");
	elektraKeySetName (key2, "user:/valid\\/");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid\\/");
	elektraKeySetName (key2, "user:/valid\\/a");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key2, "user:/valid\\/a");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid\\/");
	elektraKeySetName (key2, "user:/valid\\/\\/");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid\\/");
	elektraKeySetName (key2, "user:/valid\\/ab");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid/a/b");
	elektraKeySetName (key2, "user:/valid");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid/a");
	elektraKeySetName (key2, "user:/valid");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (elektraKeyIsDirectlyBelow (key2, key1), "Key should be below");

	elektraKeySetName (key1, "user:/tests/ini-section-write");
	elektraKeySetName (key2, "user:/tests/ini-section-write/akey\\/looking\\/like\\/sections");
	succeed_if (elektraKeyIsDirectlyBelow (key1, key2), "looking like sections not recognised");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "/valid/valide");
	succeed_if (elektraKeyIsDirectlyBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "/valid/non/valid");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/valid");
	elektraKeySetName (key2, "user:/valid/valide");
	succeed_if (elektraKeyIsDirectlyBelow (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/valid");
	elektraKeySetName (key2, "user:/valid/non/valid");
	succeed_if (!elektraKeyIsDirectlyBelow (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsDirectlyBelow (key2, key1), "Key should not be below");


	elektraKeyDel (key1);
	elektraKeyDel (key2);
}

static void test_keyDup (void)
{
	ElektraKey *orig, *copy;

	printf ("Test key duplication\n");

	// Create test key
	orig = elektraKeyNew ("user:/foo/bar", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, 6, ELEKTRA_KEY_VALUE, "foobar", ELEKTRA_KEY_COMMENT, "mycomment", ELEKTRA_KEY_END);


	// Dup the key
	succeed_if ((copy = elektraKeyDup (orig, ELEKTRA_KEY_CP_ALL)) != 0, "keyDup failed");
	compare_key (orig, copy);
	elektraKeyDel (orig); // everything independent from original!

	// Check the duplication
	succeed_if_same_string (elektraKeyName (copy), "user:/foo/bar");
	succeed_if (strncmp (elektraKeyValue (copy), "foobar", 6) == 0, "keyDup: key value copy error");
	succeed_if (elektraKeyIsBinary (copy), "keyDup: key type copy error");

	// Dup the key again
	ElektraKey * ccopy;
	succeed_if ((ccopy = elektraKeyDup (copy, ELEKTRA_KEY_CP_ALL)) != 0, "keyDup failed");
	compare_key (copy, ccopy);
	elektraKeyDel (copy); // everything independent from original!

	succeed_if_same_string (elektraKeyName (ccopy), "user:/foo/bar");
	succeed_if (strncmp (elektraKeyValue (ccopy), "foobar", 6) == 0, "keyDup: key value ccopy error");
	succeed_if (elektraKeyIsBinary (ccopy), "keyDup: key type ccopy error");

	elektraKeyDel (ccopy);

	orig = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetName (orig, "invalid");

	succeed_if ((copy = elektraKeyDup (orig, ELEKTRA_KEY_CP_ALL)) != 0, "keyDup failed");
	succeed_if_same_string (elektraKeyName (orig), "/");
	succeed_if_same_string (elektraKeyName (copy), "/");
	succeed_if (elektraKeyGetNameSize (orig) == 2, "orig name size");
	succeed_if (elektraKeyGetNameSize (copy) == 2, "orig name size");
	succeed_if (elektraKeyGetRef (orig) == 0, "orig ref counter should be 0");
	succeed_if (elektraKeyGetRef (copy) == 0, "copy ref counter should be 0");

	elektraKeyDel (orig);
	elektraKeyDel (copy);
}

static void test_keyCopy (void)
{
	ElektraKey *orig, *copy;
	char origBuffer[5], copyBuffer[5];

	printf ("Test key copy\n");

	// Create test key
	orig = elektraKeyNew ("user:/foo/bar", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, 6, ELEKTRA_KEY_VALUE, "foobar", ELEKTRA_KEY_COMMENT, "mycomment", ELEKTRA_KEY_END);


	// Copy the key
	copy = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyCopy (copy, orig, ELEKTRA_KEY_CP_ALL) == copy, "keyCopy failed");
	succeed_if (elektraKeyGetRef (orig) == 0, "orig ref counter should be 0");
	succeed_if (elektraKeyGetRef (copy) == 0, "copy ref counter should be 0");
	compare_key (orig, copy);

	succeed_if (elektraKeyCopy (copy, orig, ELEKTRA_KEY_CP_ALL) == copy, "keyCopy failed");
	succeed_if (elektraKeyGetRef (orig) == 0, "orig ref counter should be 0");
	succeed_if (elektraKeyGetRef (copy) == 0, "copy ref counter should be 0");
	compare_key (orig, copy);
	elektraKeyDel (orig); // everything independent from original!

	// Check the duplication
	succeed_if_same_string (elektraKeyName (copy), "user:/foo/bar");
	succeed_if (strncmp (elektraKeyValue (copy), "foobar", 6) == 0, "keyCopy: key value copy error");

	orig = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyCopy (copy, orig, ELEKTRA_KEY_CP_ALL) == copy, "make a key copy of an unmodified key");
	compare_key (orig, copy);

	succeed_if (elektraKeyCopy (copy, 0, ELEKTRA_KEY_CP_ALL) == copy, "make the key copy fresh");
	compare_key (orig, copy);
	elektraKeyDel (orig);

	elektraKeyDel (copy);

	orig = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetName (orig, "invalid");

	copy = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyCopy (copy, orig, ELEKTRA_KEY_CP_ALL) == copy, "keyCopy failed");
	succeed_if_same_string (elektraKeyName (orig), "/");
	succeed_if_same_string (elektraKeyName (copy), "/");
	succeed_if (elektraKeyGetNameSize (orig) == 2, "orig name size");
	succeed_if (elektraKeyGetNameSize (copy) == 2, "orig name size");
	succeed_if (elektraKeyGetRef (orig) == 0, "orig ref counter should be 0");
	succeed_if (elektraKeyGetRef (copy) == 0, "copy ref counter should be 0");

	elektraKeyDel (orig);
	elektraKeyDel (copy);


	// check KEY_CP_NAME
	orig = elektraKeyNew ("user:/orig", ELEKTRA_KEY_END);
	elektraKeySetString (orig, "orig");
	elektraKeySetMeta (orig, "orig", "orig");

	copy = elektraKeyNew ("user:/copy", ELEKTRA_KEY_END);
	elektraKeySetString (copy, "copy");
	elektraKeySetMeta (copy, "copy", "copy");

	succeed_if (elektraKeyCopy (copy, orig, ELEKTRA_KEY_CP_NAME) == copy, "keyCopy failed");
	succeed_if_same_string (elektraKeyName (orig), "user:/orig");
	succeed_if_same_string (elektraKeyName (copy), "user:/orig");
	elektraKeyGetString (orig, origBuffer, sizeof (origBuffer));
	elektraKeyGetString (copy, copyBuffer, sizeof (copyBuffer));
	succeed_if_same_string (origBuffer, "orig");
	succeed_if_same_string (copyBuffer, "copy");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (orig, "orig")), "orig");
	succeed_if (elektraKeyGetMeta (orig, "copy") == 0, "Metadata not cleared on keyCopy with KEY_CP_NAME");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (copy, "copy")), "copy");
	succeed_if (elektraKeyGetMeta (copy, "orig") == 0, "Metadata not cleared on keyCopy with KEY_CP_NAME");

	elektraKeyDel (orig);
	elektraKeyDel (copy);

	// check KEY_CP_VALUE
	orig = elektraKeyNew ("user:/orig", ELEKTRA_KEY_END);
	elektraKeySetString (orig, "orig");
	elektraKeySetMeta (orig, "orig", "orig");

	copy = elektraKeyNew ("user:/copy", ELEKTRA_KEY_END);
	elektraKeySetString (copy, "copy");
	elektraKeySetMeta (copy, "copy", "copy");

	succeed_if (elektraKeyCopy (copy, orig, ELEKTRA_KEY_CP_VALUE) == copy, "keyCopy failed");
	succeed_if_same_string (elektraKeyName (orig), "user:/orig");
	succeed_if_same_string (elektraKeyName (copy), "user:/copy");
	elektraKeyGetString (orig, origBuffer, sizeof (origBuffer));
	elektraKeyGetString (copy, copyBuffer, sizeof (copyBuffer));
	succeed_if_same_string (origBuffer, "orig");
	succeed_if_same_string (copyBuffer, "orig");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (orig, "orig")), "orig");
	succeed_if (elektraKeyGetMeta (orig, "copy") == 0, "Metadata not cleared on keyCopy with KEY_CP_VALUE");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (copy, "copy")), "copy");
	succeed_if (elektraKeyGetMeta (copy, "orig") == 0, "Metadata not cleared on keyCopy with KEY_CP_VALUE");

	elektraKeyDel (orig);
	elektraKeyDel (copy);

	// check KEY_CP_META
	orig = elektraKeyNew ("user:/orig", ELEKTRA_KEY_END);
	elektraKeySetString (orig, "orig");
	elektraKeySetMeta (orig, "orig", "orig");

	copy = elektraKeyNew ("user:/copy", ELEKTRA_KEY_END);
	elektraKeySetString (copy, "copy");
	elektraKeySetMeta (copy, "copy", "copy");

	succeed_if (elektraKeyCopy (copy, orig, ELEKTRA_KEY_CP_META) == copy, "keyCopy failed");
	succeed_if_same_string (elektraKeyName (orig), "user:/orig");
	succeed_if_same_string (elektraKeyName (copy), "user:/copy");
	elektraKeyGetString (orig, origBuffer, sizeof (origBuffer));
	elektraKeyGetString (copy, copyBuffer, sizeof (copyBuffer));
	succeed_if_same_string (origBuffer, "orig");
	succeed_if_same_string (copyBuffer, "copy");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (orig, "orig")), "orig");
	succeed_if (elektraKeyGetMeta (orig, "copy") == 0, "Metadata not cleared on keyCopy with KEY_CP_META");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (copy, "orig")), "orig");
	succeed_if (elektraKeyGetMeta (copy, "copy") == 0, "Metadata not cleared on keyCopy with KEY_CP_META");

	elektraKeyDel (orig);
	elektraKeyDel (copy);


	orig = elektraKeyNew ("user:/orig", ELEKTRA_KEY_END);
	succeed_if (elektraKeyNeedSync (orig), "fresh key does not need sync?");
	ElektraKeyset * ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, orig);
	copy = elektraKeyNew ("user:/othername", ELEKTRA_KEY_END);
	succeed_if (elektraKeyNeedSync (copy), "fresh key does not need sync?");
	succeed_if (elektraKeyGetRef (orig) == 1, "orig ref counter should be 1");
	succeed_if (elektraKeyGetRef (copy) == 0, "copy ref counter should be 0");
	succeed_if (elektraKeyCopy (orig, copy, ELEKTRA_KEY_CP_ALL) == NULL, "copy should not be allowed when key is already referred to");
	succeed_if (elektraKeyNeedSync (orig), "copied key does not need sync?");
	succeed_if (elektraKeyNeedSync (copy), "copied key does not need sync?");

	succeed_if (elektraKeyGetRef (orig) == 1, "orig ref counter should be 1");
	succeed_if (elektraKeyGetRef (copy) == 0, "copy ref counter should be 1");

	succeed_if_same_string (elektraKeyName (orig), "user:/orig");
	succeed_if_same_string (elektraKeyName (copy), "user:/othername");

	elektraKeyDel (copy);
	elektraKeysetRewind (ks);
	succeed_if_same_string (elektraKeyName (elektraKeysetNext (ks)), "user:/orig");
	elektraKeysetDel (ks);
}


typedef void (*fun_t) (void);
static void fun (void)
{
}

static void test_binary (void)
{
	printf ("Test binary values\n");

	ElektraKey * k = 0;

	int i = 20;
	int * p = &i;

	k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetBinary (k, &p, sizeof (p)) == sizeof (p), "could not set binary");

	int * q;
	succeed_if (elektraKeyGetBinary (k, &q, sizeof (q)) == sizeof (q), "could not get binary");
	succeed_if (p == q, "pointers to int are not equal");
	succeed_if (*p == *q, "values are not equal");
	succeed_if (*q == 20, "values are not equal");
	succeed_if (*p == 20, "values are not equal");

	elektraKeyDel (k);


	union
	{
		void (*f) (void);
		void * v;
	} conversation;

	k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	conversation.f = fun;
	succeed_if (elektraKeySetBinary (k, &conversation.v, sizeof (conversation)) == sizeof (conversation), "could not set binary");

	conversation.v = 0;
	conversation.f = 0;
	void (*g) (void) = 0;
	succeed_if (elektraKeyGetBinary (k, &conversation.v, sizeof (conversation)) == sizeof (conversation), "could not get binary");
	g = conversation.f;
	succeed_if (g == fun, "pointers to functions are not equal");

	elektraKeyDel (k);


	conversation.f = fun;
	k = elektraKeyNew ("system:/symbols/fun", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (conversation), ELEKTRA_KEY_VALUE, &conversation.v, ELEKTRA_KEY_END);

	conversation.v = 0;
	conversation.f = 0;
	succeed_if (elektraKeyGetBinary (k, &conversation.v, sizeof (conversation)) == sizeof (conversation), "could not get binary");
	g = conversation.f;
	succeed_if (g == fun, "pointers to functions are not equal");

	elektraKeyDel (k);


	fun_t tmp = fun;

	k = elektraKeyNew ("system:/symbol/fun", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (fun_t), ELEKTRA_KEY_VALUE, &tmp, ELEKTRA_KEY_END);

	fun_t myfun = 0;
	succeed_if (elektraKeyGetBinary (k, &myfun, sizeof (fun_t)) == sizeof (fun_t), "could not get binary");

	succeed_if (fun == myfun, "pointers not equal");

	elektraKeyDel (k);


	k = elektraKeyNew ("system:/symbol/cool", ELEKTRA_KEY_FUNC, fun, ELEKTRA_KEY_END);

	succeed_if (elektraKeyGetBinary (k, &myfun, sizeof (fun_t)) == sizeof (fun_t), "could not get binary");

	succeed_if (fun == myfun, "pointers not equal");

	elektraKeyDel (k);

	char data[10];
	k = elektraKeyNew ("system:/empty_binary", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetBinary (k, 0, 0) == 0, "could not set binary will null pointer");
	succeed_if (elektraKeyIsBinary (k), "key is not binary");
	succeed_if (elektraKeyGetBinary (k, data, 1) == 0, "could not get empty binary");
	succeed_if (elektraKeyValue (k) == 0, "did not get back null pointer");

	succeed_if (elektraKeySetBinary (k, 0, 1) == 0, "could not set binary will null pointer");
	succeed_if (elektraKeyIsBinary (k), "key is not binary");
	succeed_if (elektraKeyGetBinary (k, data, 1) == 0, "could not get empty binary");
	succeed_if (elektraKeyValue (k) == 0, "did not get back null pointer");

	succeed_if (elektraKeySetBinary (k, 0, 5) == 0, "could not set binary will null pointer");
	succeed_if (elektraKeyIsBinary (k), "key is not binary");
	succeed_if (elektraKeyGetBinary (k, data, 1) == 0, "could not get empty binary");
	succeed_if (elektraKeyValue (k) == 0, "did not get back null pointer");

	succeed_if (elektraKeySetBinary (k, 0, -1) == -1, "misusage: this will fail");
	succeed_if (elektraKeyIsBinary (k), "key is not binary (should be from previous calls)");
	succeed_if (elektraKeyGetBinary (k, data, 1) == 0, "could not get empty binary");
	succeed_if (elektraKeyValue (k) == 0, "did not get back null pointer");

	elektraKeyDel (k);
}

static void test_keyBelowOrSame (void)
{
	ElektraKey * key1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * key2 = elektraKeyNew ("/", ELEKTRA_KEY_END);

	printf ("Test of keyBelowOrSame\n");

	succeed_if (elektraKeyIsBelowOrSame (key1, 0) == -1, "NULL pointer");
	succeed_if (elektraKeyIsBelowOrSame (0, 0) == -1, "NULL pointer");
	succeed_if (elektraKeyIsBelowOrSame (0, key1) == -1, "NULL pointer");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (elektraKeyIsBelowOrSame (key2, key1), "Key should be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (elektraKeyIsBelowOrSame (key2, key1), "Key should be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/below");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/b");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/b");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid");
	elektraKeySetName (key2, "user:/valid/b/e");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/valid");
	elektraKeySetName (key2, "/valid/b/e");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "/valid/b/e");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "/v");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "/");
	elektraKeySetName (key2, "/");
	succeed_if (elektraKeyIsBelowOrSame (key1, key2), "Key should be below");
	succeed_if (elektraKeyIsBelowOrSame (key2, key1), "Key should be below");

	elektraKeySetName (key1, "user:/valide");
	elektraKeySetName (key2, "user:/valid/e");
	succeed_if (!elektraKeyIsBelowOrSame (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/valid/b");
	elektraKeySetName (key2, "user:/valid/e");
	succeed_if (!elektraKeyIsBelowOrSame (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/export/a");
	elektraKeySetName (key2, "user:/export-backup/b");
	succeed_if (!elektraKeyIsBelowOrSame (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeySetName (key1, "user:/export");
	elektraKeySetName (key2, "user:/export-backup-2/x");
	succeed_if (!elektraKeyIsBelowOrSame (key1, key2), "Key should not be below");
	succeed_if (!elektraKeyIsBelowOrSame (key2, key1), "Key should not be below");

	elektraKeyDel (key1);
	elektraKeyDel (key2);
}

static void test_keyNameSpecial (void)
{
	printf ("Test special keynames\n");
	ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (k), "/");

	succeed_if (elektraKeySetName (k, "system:/"), "could not set key name with system");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, 0) < 0, "could set key name with 0");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/"), "could not set key name with system");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "") < 0, "could set key name with empty string");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/"), "could not set key name with system");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "invalid") < 0, "could set key name invalid");
	succeed_if_same_string (elektraKeyName (k), "system:/");


	succeed_if (elektraKeySetName (k, "system:/something/.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/something/.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/something/../.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/something/../../.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/something/../../../.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/something/../../../../.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");


	succeed_if (elektraKeySetName (k, "system:/../something"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/something");

	succeed_if (elektraKeySetName (k, "system:/../../something"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/something");

	succeed_if (elektraKeySetName (k, "system:/../../../something"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/something");

	succeed_if (elektraKeySetName (k, "system:/../../../../something"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/something");

	succeed_if (elektraKeySetName (k, "system:/../../../../../something"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/something");


	succeed_if (elektraKeySetName (k, "system:/a/b/c/.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/a/b");

	succeed_if (elektraKeySetName (k, "system:/a/b/c/../.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/a");

	succeed_if (elektraKeySetName (k, "system:/a/b/c/../../.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/a/b/c/../../../.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");

	succeed_if (elektraKeySetName (k, "system:/a/b/c/../../../../.."), "could not set key name with ..");
	succeed_if_same_string (elektraKeyName (k), "system:/");


	succeed_if (elektraKeySetName (k, "system:/../a/b/c"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/a/b/c");

	succeed_if (elektraKeySetName (k, "system:/../../a/b/c"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/a/b/c");

	succeed_if (elektraKeySetName (k, "system:/../../../a/b/c"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/a/b/c");

	succeed_if (elektraKeySetName (k, "system:/../../../../a/b/c"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/a/b/c");

	succeed_if (elektraKeySetName (k, "system:/../../../../../a/b/c"), "could not set key name with too many ..");
	succeed_if_same_string (elektraKeyName (k), "system:/a/b/c");


	elektraKeyDel (k);


	printf ("Test invalid names\n");
	k = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeySetName (k, 0) == -1, "no error code setting invalid name");
	succeed_if_same_string (elektraKeyName (k), "/");

	succeed_if (elektraKeySetName (k, "") == -1, "no error code setting invalid name");
	succeed_if_same_string (elektraKeyName (k), "/");

	succeed_if (elektraKeyGetNameSize (k) == 2, "empty name size");

	succeed_if (elektraKeyGetNameSize (0) == -1, "no error on getting name size of NULL pointer");

	elektraKeyDel (k);
}

static void test_keyClear (void)
{
	printf ("Test clear of key\n");

	ElektraKey * k1 = elektraKeyNew ("system:/abc", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (k1), "system:/abc");

	succeed_if (elektraKeyGetRef (k1) == 0, "New key reference");
	elektraKeyIncRef (k1);
	succeed_if (elektraKeyGetRef (k1) == 1, "Incremented key reference");
	ElektraKey * k2 = k1; // create an alias for k1
	succeed_if_same_string (elektraKeyName (k2), "system:/abc");
	succeed_if (elektraKeyGetRef (k1) == 1, "Incremented key reference");
	succeed_if (elektraKeyGetRef (k2) == 1, "Incremented key reference");

	elektraKeyIncRef (k1);
	ElektraKey * k3 = k1; // create an alias for k1
	succeed_if_same_string (elektraKeyName (k3), "system:/abc");
	succeed_if (elektraKeyGetRef (k1) == 2, "Incremented key reference");
	succeed_if (elektraKeyGetRef (k2) == 2, "Incremented key reference");
	succeed_if (elektraKeyGetRef (k3) == 2, "Incremented key reference");

	elektraKeyClear (k1);
	succeed_if_same_string (elektraKeyName (k1), "/");
	succeed_if_same_string (elektraKeyName (k2), "/");
	succeed_if_same_string (elektraKeyName (k3), "/");

	elektraKeySetMeta (k1, "test_meta", "test_value");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (k1, "test_meta")), "test_value");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (k2, "test_meta")), "test_value");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (k3, "test_meta")), "test_value");

	elektraKeyClear (k2);
	succeed_if (elektraKeyGetMeta (k1, "test_meta") == 0, "there should be no meta after keyClear");
	succeed_if (elektraKeyGetMeta (k2, "test_meta") == 0, "there should be no meta after keyClear");
	succeed_if (elektraKeyGetMeta (k3, "test_meta") == 0, "there should be no meta after keyClear");

	elektraKeySetString (k1, "mystring");
	succeed_if_same_string (elektraKeyValue (k1), "mystring");
	succeed_if_same_string (elektraKeyValue (k2), "mystring");
	succeed_if_same_string (elektraKeyValue (k3), "mystring");

	elektraKeyClear (k3);
	succeed_if_same_string (elektraKeyValue (k1), "");
	succeed_if_same_string (elektraKeyValue (k2), "");
	succeed_if_same_string (elektraKeyValue (k3), "");

	succeed_if (elektraKeyGetRef (k1) == 2, "Incremented key reference");
	succeed_if (elektraKeyGetRef (k2) == 2, "Incremented key reference");
	succeed_if (elektraKeyGetRef (k3) == 2, "Incremented key reference");

	elektraKeyDel (k3); // does nothing
	elektraKeyDecRef (k3);
	k3 = 0; // remove alias
	succeed_if (elektraKeyGetRef (k1) == 1, "Incremented key reference");
	succeed_if (elektraKeyGetRef (k2) == 1, "Incremented key reference");

	elektraKeyDel (k2); // does nothing
	elektraKeyDecRef (k2);
	k2 = 0; // remove alias
	succeed_if (elektraKeyGetRef (k1) == 0, "Incremented key reference");

	elektraKeyDel (k1);
}

static void test_keyBaseName (void)
{
	printf ("Test basename\n");
	ElektraKey * k = elektraKeyNew ("user:///foo\\///bar\\/foo_bar\\/", ELEKTRA_KEY_END);
	succeed_if_same_string (elektraKeyName (k), "user:/foo\\//bar\\/foo_bar\\/");
	succeed_if_same_string (elektraKeyBaseName (k), "bar/foo_bar/");
	succeed_if (elektraKeyGetBaseNameSize (k) == 13, "wrong base name size");

	elektraKeySetName (k, "system:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");
	succeed_if (elektraKeyGetBaseNameSize (k) == 1, "wrong base name size");

	elektraKeySetName (k, "system:/valid//////");
	succeed_if_same_string (elektraKeyName (k), "system:/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");
	succeed_if (elektraKeyGetBaseNameSize (k) == 6, "wrong base name size");

	elektraKeySetName (k, "system://////valid//////");
	succeed_if_same_string (elektraKeyName (k), "system:/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");
	succeed_if (elektraKeyGetBaseNameSize (k) == 6, "wrong base name size");

	elektraKeySetName (k, "system:///.///valid//.////");
	succeed_if_same_string (elektraKeyName (k), "system:/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");
	succeed_if (elektraKeyGetBaseNameSize (k) == 6, "wrong base name size");

	elektraKeySetName (k, "user:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");
	succeed_if (elektraKeyGetBaseNameSize (k) == 1, "wrong base name size");

	elektraKeySetName (k, "user:/valid//////");
	succeed_if_same_string (elektraKeyName (k), "user:/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");
	succeed_if (elektraKeyGetBaseNameSize (k) == 6, "wrong base name size");

	elektraKeySetName (k, "user://////valid//////");
	succeed_if_same_string (elektraKeyName (k), "user:/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");
	succeed_if (elektraKeyGetBaseNameSize (k) == 6, "wrong base name size");

	elektraKeySetName (k, "user:///.///valid//.////");
	succeed_if_same_string (elektraKeyName (k), "user:/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");
	succeed_if (elektraKeyGetBaseNameSize (k) == 6, "wrong base name size");

	elektraKeySetName (k, "user:///foo\\///bar\\/foo_bar\\/");
	succeed_if_same_string (elektraKeyName (k), "user:/foo\\//bar\\/foo_bar\\/");
	succeed_if_same_string (elektraKeyBaseName (k), "bar/foo_bar/");
	succeed_if (elektraKeyGetBaseNameSize (k) == 13, "wrong base name size");

	elektraKeySetName (k, "user://////foo_bar\\/");
	succeed_if_same_string (elektraKeyName (k), "user:/foo_bar\\/");
	succeed_if_same_string (elektraKeyBaseName (k), "foo_bar/");
	succeed_if (elektraKeyGetBaseNameSize (k) == 9, "wrong base name size");

	elektraKeySetName (k, "user://////%");
	succeed_if_same_string (elektraKeyName (k), "user:/%");
	succeed_if_same_string (elektraKeyBaseName (k), "");
	succeed_if (elektraKeyGetBaseNameSize (k) == 1, "wrong base name size");

	elektraKeySetName (k, "user://////\\%");
	succeed_if_same_string (elektraKeyName (k), "user:/\\%");
	succeed_if_same_string (elektraKeyBaseName (k), "%");
	succeed_if (elektraKeyGetBaseNameSize (k) == 2, "wrong base name size");

	elektraKeySetName (k, "user://////\\\\%");
	succeed_if_same_string (elektraKeyName (k), "user:/\\\\%");
	succeed_if_same_string (elektraKeyBaseName (k), "\\%");
	succeed_if (elektraKeyGetBaseNameSize (k) == 3, "wrong base name size");

	elektraKeySetName (k, "system:/\\\\valid/\\\\base");
	succeed_if_same_string (elektraKeyName (k), "system:/\\\\valid/\\\\base");
	succeed_if_same_string (elektraKeyBaseName (k), "\\base");
	succeed_if (elektraKeyGetBaseNameSize (k) == 6, "wrong base name size");

	elektraKeySetName (k, "system:/\\/valid/\\/base");
	succeed_if_same_string (elektraKeyName (k), "system:/\\/valid/\\/base");
	succeed_if_same_string (elektraKeyBaseName (k), "/base"); // wanted?
	succeed_if (elektraKeyGetBaseNameSize (k) == 6, "wrong base name size");

	elektraKeySetName (k, "system:/valid\\\\/base");
	succeed_if_same_string (elektraKeyName (k), "system:/valid\\\\/base");
	succeed_if_same_string (elektraKeyBaseName (k), "base");
	succeed_if (elektraKeyGetBaseNameSize (k) == 5, "wrong base name size");

	elektraKeySetName (k, "user://////\\\\%");
	succeed_if_same_string (elektraKeyName (k), "user:/\\\\%");
	succeed_if_same_string (elektraKeyBaseName (k), "\\%");
	elektraKeyDel (k);
}

static void test_keySetBaseName (void)
{
	printf ("Test set basename\n");

	ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeySetBaseName (0, "abc") == -1, "NULL key");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	succeed_if (elektraKeySetBaseName (k, "abc") == -1, "invalid key");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "spec:/");
	succeed_if (elektraKeySetBaseName (k, 0) == -1, "could remove root name");
	succeed_if_same_string (elektraKeyName (k), "spec:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "proc:/");
	succeed_if (elektraKeySetBaseName (k, 0) == -1, "could remove root name");
	succeed_if_same_string (elektraKeyName (k), "proc:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "dir:/");
	succeed_if (elektraKeySetBaseName (k, 0) == -1, "could remove root name");
	succeed_if_same_string (elektraKeyName (k), "dir:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "user:/");
	succeed_if (elektraKeySetBaseName (k, 0) == -1, "could remove root name");
	succeed_if_same_string (elektraKeyName (k), "user:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "system:/");
	succeed_if (elektraKeySetBaseName (k, 0) == -1, "could remove root name");
	succeed_if_same_string (elektraKeyName (k), "system:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeySetBaseName (k, 0) == -1, "could remove root name");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/x");
	succeed_if (elektraKeySetBaseName (k, 0) == 2, "removing single character basename of cascading key with depth 1 failed");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/cascading");
	succeed_if (elektraKeySetBaseName (k, 0) == 2, "removing basename of cascading key with depth 1 failed");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "system:/notCascading");
	succeed_if (elektraKeySetBaseName (k, 0) >= 0, "removing basename of non cascading key with depth 1 failed");
	succeed_if_same_string (elektraKeyName (k), "system:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "system:/foo/bar");
	succeed_if (elektraKeySetBaseName (k, 0) >= 0, "removing basename of non cascading key with depth 2 failed");
	succeed_if_same_string (elektraKeyName (k), "system:/foo");
	succeed_if_same_string (elektraKeyBaseName (k), "foo");
	succeed_if (elektraKeySetBaseName (k, 0) >= 0, "second removing basename of non cascading key with depth 2 failed");
	succeed_if_same_string (elektraKeyName (k), "system:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/foo/bar");
	succeed_if (elektraKeySetBaseName (k, 0) >= 0, "removing basename of cascading key with depth 2 failed");
	succeed_if_same_string (elektraKeyName (k), "/foo");
	succeed_if_same_string (elektraKeyBaseName (k), "foo");
	succeed_if (elektraKeySetBaseName (k, 0) == 2, "second removing basename of cascading key with depth 2 failed");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");
	succeed_if (elektraKeySetBaseName (k, 0) == -1, "third removing basename of cascading key with depth 2 was possible");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/\\/");
	succeed_if (elektraKeySetBaseName (k, 0) == 2, "removing basename of single character escaped cascading key with depth 1 failed");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/\\.");
	succeed_if (elektraKeySetBaseName (k, 0) == 2, "removing basename of single character escaped dot cascading key with depth 1 failed");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/\\..");
	succeed_if (elektraKeySetBaseName (k, 0) == 2, "removing basename of escaped dot dot cascading key with depth 1 failed");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/\\%");
	succeed_if (elektraKeySetBaseName (k, 0) == 2, "removing basename of single character escaped % cascading key with depth 1 failed");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/#1");
	succeed_if (elektraKeySetBaseName (k, 0) == 2, "removing basename of array cascading key with depth 1 failed");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "system:/");
	succeed_if (elektraKeySetBaseName (k, "valid") == -1, "add root name, but set was used");
	succeed_if_same_string (elektraKeyName (k), "system:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "proc:/");
	succeed_if (elektraKeySetBaseName (k, "a") == -1, "add root name, but set was used");
	succeed_if_same_string (elektraKeyName (k), "proc:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, 0) == sizeof ("system:/"), "could not remove base name");
	succeed_if_same_string (elektraKeyName (k), "system:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "") > 0, "could not set empty name");
	succeed_if_same_string (elektraKeyName (k), "system:/%"); // is an empty name
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "a") >= 0, "escaped slash ok");
	succeed_if_same_string (elektraKeyName (k), "system:/a");
	succeed_if_same_string (elektraKeyBaseName (k), "a");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "/") >= 0, "escaped slash ok");
	succeed_if_same_string (elektraKeyName (k), "system:/\\/");
	succeed_if_same_string (elektraKeyBaseName (k), "/");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "\\/") >= 0, "escaped slash ok");
	succeed_if_same_string (elektraKeyName (k), "system:/\\\\\\/");
	succeed_if_same_string (elektraKeyBaseName (k), "\\/");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "\\\\/") >= 0, "backslash escaped, but slash unescaped");
	succeed_if_same_string (elektraKeyName (k), "system:/\\\\\\\\\\/");
	succeed_if_same_string (elektraKeyBaseName (k), "\\\\/");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "\\\\\\/") >= 0, "backslash escaped, slash escaped");
	succeed_if_same_string (elektraKeyName (k), "system:/\\\\\\\\\\\\\\/");
	succeed_if_same_string (elektraKeyBaseName (k), "\\\\\\/");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "%") == sizeof ("system:/\\%"), "could not set basename");
	succeed_if_same_string (elektraKeyBaseName (k), "%");
	succeed_if_same_string (elektraKeyName (k), "system:/\\%");
	succeed_if_same_string (elektraKeyBaseName (k), "%");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, ".") == sizeof ("system:/\\%"), "could not set basename");
	succeed_if_same_string (elektraKeyBaseName (k), ".");
	succeed_if_same_string (elektraKeyName (k), "system:/\\.");
	succeed_if_same_string (elektraKeyBaseName (k), ".");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "..") == sizeof ("system:/\\.."), "could not set basename");
	succeed_if_same_string (elektraKeyBaseName (k), "..");
	succeed_if_same_string (elektraKeyName (k), "system:/\\..");
	succeed_if_same_string (elektraKeyBaseName (k), "..");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "\\\\\\\\") >= 0, "backslash escaped, backslash escaped");
	succeed_if_same_string (elektraKeyName (k), "system:/\\\\\\\\\\\\\\\\");
	succeed_if_same_string (elektraKeyBaseName (k), "\\\\\\\\");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "\\\\") >= 0, "escaped backslash ok");
	succeed_if_same_string (elektraKeyName (k), "system:/\\\\\\\\");
	succeed_if_same_string (elektraKeyBaseName (k), "\\\\");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "\\.") >= 0, "escaped dot");
	succeed_if_same_string (elektraKeyName (k), "system:/\\\\.");
	succeed_if_same_string (elektraKeyBaseName (k), "\\.");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "\\..") >= 0, "escaped dot-dot");
	succeed_if_same_string (elektraKeyName (k), "system:/\\\\..");
	succeed_if_same_string (elektraKeyBaseName (k), "\\..");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "%") == sizeof ("system:/\\%"), "add some char");
	succeed_if_same_string (elektraKeyName (k), "system:/\\%");
	succeed_if_same_string (elektraKeyBaseName (k), "%");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "#1") == sizeof ("system:/#1"), "valid array entry");
	succeed_if_same_string (elektraKeyName (k), "system:/#1");
	succeed_if_same_string (elektraKeyBaseName (k), "#1");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeySetBaseName (k, "#_10") >= 0, "valid array entry");
	succeed_if_same_string (elektraKeyName (k), "system:/#_10");
	succeed_if_same_string (elektraKeyBaseName (k), "#_10");

	elektraKeySetName (k, "user:/tests/yajl/___empty_map");
	succeed_if_same_string (elektraKeyBaseName (k), "___empty_map");
	elektraKeySetBaseName (k, "#0");
	succeed_if_same_string (elektraKeyBaseName (k), "#0");

	elektraKeySetBaseName (k, "nullkey");
	succeed_if_same_string (elektraKeyBaseName (k), "nullkey");
	succeed_if_same_string (elektraKeyName (k), "user:/tests/yajl/nullkey");

	//! [base1]
	elektraKeySetName (k, "system:/valid");
	elektraKeySetBaseName (k, ".hiddenkey");
	succeed_if_same_string (elektraKeyName (k), "system:/.hiddenkey");
	succeed_if_same_string (elektraKeyBaseName (k), ".hiddenkey");
	//! [base1]

	//! [base2]
	elektraKeySetName (k, "system:/valid");
	elektraKeySetBaseName (k, "");
	succeed_if_same_string (elektraKeyName (k), "system:/%");
	succeed_if_same_string (elektraKeyBaseName (k), "");
	//! [base2]

	//! [base3]
	elektraKeySetName (k, "system:/valid");
	elektraKeySetBaseName (k, "%");
	succeed_if_same_string (elektraKeyName (k), "system:/\\%");
	succeed_if_same_string (elektraKeyBaseName (k), "%");
	//! [base3]

	elektraKeyDel (k);
}

static void test_keyAddBaseName (void)
{
	printf ("Test add basename\n");

	ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_END);

	//![base0 empty]
	elektraKeySetName (k, "");
	succeed_if_same_string (elektraKeyBaseName (k), "");
	elektraKeySetName (k, "user:/");
	succeed_if_same_string (elektraKeyBaseName (k), "");
	//![base0 empty]

	//![base1 empty]
	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/%");
	succeed_if_same_string (elektraKeyBaseName (k), "");
	//![base1 empty]

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "%") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/\\%");
	succeed_if_same_string (elektraKeyBaseName (k), "%");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "#") == sizeof ("system:/valid/#"), "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/#");
	succeed_if_same_string (elektraKeyBaseName (k), "#");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "#2") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/#2");
	succeed_if_same_string (elektraKeyBaseName (k), "#2");

	//![base1 add]
	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, ".") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/\\.");
	succeed_if_same_string (elektraKeyBaseName (k), ".");
	//![base1 add]

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "..") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/\\..");
	succeed_if_same_string (elektraKeyBaseName (k), "..");


	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "hello%#") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/hello%#");
	succeed_if_same_string (elektraKeyBaseName (k), "hello%#");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "hello..") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/hello..");
	succeed_if_same_string (elektraKeyBaseName (k), "hello..");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "..hello..") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/..hello..");
	succeed_if_same_string (elektraKeyBaseName (k), "..hello..");

	elektraKeySetName (k, "system:/valid");
	succeed_if (elektraKeyAddBaseName (k, "has/slash") >= 0, "could not add a base name");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/has\\/slash");
	succeed_if_same_string (elektraKeyBaseName (k), "has/slash");

	elektraKeySetName (k, "system:/valid");
	elektraKeyAddBaseName (k, "#0");
	succeed_if_same_string (elektraKeyName (k), "system:/valid/#0");
	succeed_if_same_string (elektraKeyBaseName (k), "#0");

	elektraKeyDel (k);
}

static void test_keyDirectBelow (void)
{
	printf ("Test direct below check\n");

	ElektraKey * k1 = elektraKeyNew ("/dir", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("/dir/directbelow", ELEKTRA_KEY_END);
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "not direct below");

	elektraKeySetName (k1, "user:/dir");
	elektraKeySetName (k2, "user:/dir/directbelow");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "not direct below");

	elektraKeySetName (k1, "user:/dir");
	elektraKeySetName (k2, "user:/dir/direct\\/below");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "not direct below");

	elektraKeySetName (k1, "user:/dir");
	elektraKeySetName (k2, "user:/dir/direct\\/");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "not direct below");

	elektraKeySetName (k1, "user:/dir");
	succeed_if (elektraKeySetName (k2, "user:/dir/direct\\\\") > -1, "could not set correct name");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "not direct below");

	elektraKeySetName (k1, "user:/dir");
	elektraKeySetName (k2, "user:/dir/direct\\\\\\/below");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "not direct below");

	elektraKeySetName (k1, "user:/dir");
	elektraKeySetName (k2, "user:/dir/direct\\\\below");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "below");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 1, "not direct below");

	elektraKeySetName (k1, "user:/dir");
	elektraKeySetName (k2, "user:/dir/direct\\\\/b");
	succeed_if_same_string (elektraKeyName (k2), "user:/dir/direct\\\\/b");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "below");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 0, "direct below, but shouldnt be");

	elektraKeySetName (k1, "user:/dir");
	elektraKeySetName (k2, "user:/dir/direct\\\\/below");
	succeed_if (elektraKeyIsBelow (k1, k2) == 1, "below");
	succeed_if (elektraKeyIsDirectlyBelow (k1, k2) == 0, "direct below, but shouldnt be");

	elektraKeyDel (k1);
	elektraKeyDel (k2);
}

static void test_keyEscape (void)
{
	printf ("test escape in basename\n");

	ElektraKey * k = elektraKeyNew ("/valid", ELEKTRA_KEY_END);
	char buffer[500];

#define TEST_ESCAPE_PART(A, S)                                                                                                             \
	do                                                                                                                                 \
	{                                                                                                                                  \
		succeed_if (keySetBaseName (k, A) != -1, "keySetBaseName returned an error");                                              \
		succeed_if_same_string (keyBaseName (k), A);                                                                               \
		succeed_if (keyGetBaseName (k, buffer, 499) != -1, "keyGetBaseName returned an error");                                    \
		succeed_if_same_string (buffer, A);                                                                                        \
	} while (0)

#include <data_escape.c>

	elektraKeySetName (k, "spec:/valid");

#include <data_escape.c>

	elektraKeySetName (k, "proc:/valid");

#include <data_escape.c>

	elektraKeySetName (k, "dir:/valid");

#include <data_escape.c>

	elektraKeySetName (k, "user:/valid");

#include <data_escape.c>

	elektraKeySetName (k, "system:/valid");

#include <data_escape.c>

#undef TEST_ESCAPE_PART
#define TEST_ESCAPE_PART(A, S)                                                                                                             \
	do                                                                                                                                 \
	{                                                                                                                                  \
		succeed_if (keySetBaseName (k, A) == -1, "keySetBaseName should have returned an error");                                  \
		succeed_if_same_string (keyBaseName (k), "");                                                                              \
		succeed_if (keyGetBaseName (k, buffer, 499) != -1, "keyGetBaseName returned an error");                                    \
		succeed_if_same_string (buffer, "");                                                                                       \
	} while (0)

	for (int i = 0; i < NUMBER_OF_NAMESPACES; ++i)
	{
		elektraKeySetName (k, namespaces[i]);

#include <data_escape.c>
	}

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyAddBaseName (k, "valid") != -1, "keyAddBaseName returned an error");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");
	succeed_if_same_string (elektraKeyName (k), "/valid");


// generates huge key (but fits within 500)
#undef TEST_ESCAPE_PART
#define TEST_ESCAPE_PART(A, S)                                                                                                             \
	do                                                                                                                                 \
	{                                                                                                                                  \
		succeed_if (keyAddBaseName (k, A) != -1, "keyAddBaseName returned an error");                                              \
		succeed_if_same_string (keyBaseName (k), A);                                                                               \
		succeed_if (keyGetBaseName (k, buffer, 499) != -1, "keyGetBaseName (for keyAddBaseName) returned an error");               \
		succeed_if_same_string (buffer, A);                                                                                        \
	} while (0)

	for (int i = 0; i < NUMBER_OF_NAMESPACES; ++i)
	{
		elektraKeySetName (k, namespaces[i]);

#include <data_escape.c>
	}

	elektraKeyDel (k);
}

static void test_keyAdd_test (ElektraKey * k, const char * escaped, const char * unescaped)
{
	char buffer[500];
	succeed_if_fmt (elektraKeyAddName (k, escaped) != -1, "keyAddName returned an error for '%s'", escaped);
	succeed_if_same_string (elektraKeyBaseName (k), unescaped);
	succeed_if (elektraKeyGetBaseName (k, buffer, 499) != -1, "keyGetBaseName returned an error");
	succeed_if_same_string (buffer, unescaped);
}

static void test_keyAdd (void)
{
	printf ("test keyAdd\n");

	ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (0, "valid") == -1, "cannot add to null name");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyAddName (k, 0) < 0, "could add null pointer");
	succeed_if (elektraKeyAddName (k, "") == sizeof ("/"), "cannot add empty name");
	succeed_if (elektraKeyAddName (k, "/") == sizeof ("/"), "cannot add slashes");
	succeed_if (elektraKeyAddName (k, "//") == sizeof ("/"), "cannot add slashes");
	succeed_if (elektraKeyAddName (k, "////") == sizeof ("/"), "cannot add slashes");
	succeed_if (elektraKeyAddName (k, "invalid\\") < 0, "added invalid name");
	succeed_if (elektraKeyAddName (k, "valid") == sizeof ("/valid"), "added valid name");

	elektraKeySetName (k, "user:/");
	succeed_if (elektraKeyAddName (k, 0) < 0, "could add null pointer");
	succeed_if (elektraKeyAddName (k, "") == sizeof ("user:/"), "cannot add empty name");
	succeed_if (elektraKeyAddName (k, "/") == sizeof ("user:/"), "cannot add slashes");
	succeed_if (elektraKeyAddName (k, "//") == sizeof ("user:/"), "cannot add slashes");
	succeed_if (elektraKeyAddName (k, "////") == sizeof ("user:/"), "cannot add slashes");
	succeed_if (elektraKeyAddName (k, "invalid\\") < 0, "added invalid name");
	succeed_if (elektraKeyAddName (k, "valid") == sizeof ("user:/valid"), "added valid name");

	for (int i = 0; i < NUMBER_OF_NAMESPACES; ++i)
	{
		elektraKeySetName (k, namespaces[i]);

		test_keyAdd_test (k, "a", "a");
		test_keyAdd_test (k, "$", "$");
		test_keyAdd_test (k, "€", "€");
		test_keyAdd_test (k, "\x01", "\x01");
		test_keyAdd_test (k, "\xFF", "\xFF");
		test_keyAdd_test (k, "\xFF\xFF\xFF\xFF", "\xFF\xFF\xFF\xFF");
		test_keyAdd_test (k, "\xFF\xFF/\xFF\xFF", "\xFF\xFF");
		test_keyAdd_test (k, "test", "test");
		test_keyAdd_test (k, "test/name", "name");
		test_keyAdd_test (k, "a/b/c/d/e/f/g/h/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z", "z");
		test_keyAdd_test (k, "a\\/b\\/c\\/d\\/e\\/f\\/g\\/h\\/j\\/k\\/l\\/m\\/n\\/o\\/p\\/q\\/r\\/s\\/t\\/u\\/v\\/w\\/x\\/y\\/z",
				  "a/b/c/d/e/f/g/h/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z");
		test_keyAdd_test (k, "\\\\%", "\\%");
		test_keyAdd_test (k, "a/test", "test");
		test_keyAdd_test (k, "a\\/test", "a/test");
	}

	elektraKeyDel (k);
}

void test_keyCascading (void)
{
	printf ("test cascading\n");

	ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	succeed_if (elektraKeyAddName (k, "valid") > 0, "could not add valid");
	succeed_if (elektraKeyGetNameSize (k) == 7, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if (elektraKeyAddName (k, "/valid") == 7, "could not add valid with starting slash");
	succeed_if (elektraKeyGetNameSize (k) == 7, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");

	elektraKeySetName (k, "////");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if (elektraKeyAddName (k, "/////valid") == 7, "could not add valid with starting slash");
	succeed_if (elektraKeyGetNameSize (k) == 7, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if (elektraKeyAddName (k, "/////more/valid") > 0, "could not add valid with starting slash");
	succeed_if_same_string (elektraKeyName (k), "/more/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if (elektraKeyAddName (k, "/////./valid") == 7, "could not add valid with starting slash");
	succeed_if (elektraKeyGetNameSize (k) == 7, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if (elektraKeyAddName (k, "/////more/../valid") == 7, "could not add valid with ..");
	succeed_if (elektraKeyGetNameSize (k) == 7, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/valid");
	succeed_if_same_string (elektraKeyBaseName (k), "valid");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if (elektraKeyAddName (k, "/////..") == 2, "could not add nothing with ..");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if (elektraKeyAddName (k, "/////../more") == 6, "could not add more with ..");
	succeed_if (elektraKeyGetNameSize (k) == 6, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/more");
	succeed_if_same_string (elektraKeyBaseName (k), "more");


	elektraKeySetName (k, "/");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if (elektraKeyAddName (k, "/////more/..") == 2, "could not add nothing with ..");
	succeed_if (elektraKeyGetNameSize (k) == 2, "size not correct");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");


	elektraKeySetName (k, "/");
	succeed_if (elektraKeyAddName (k, "/is//../a//../complex/..///.") == 2, "could not add complex stuff");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyBaseName (k), "");

	// printf ("%s\n", keyName(k));

	elektraKeyDel (k);
}

int main (int argc, char ** argv)
{
	printf ("KEY ABI  TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_keyBelow ();
	test_keyBelowOrSame ();
	test_keyDirectBelow ();

	test_keyNewSpecial ();
	test_keyNewSystem ();
	test_keyNewUser ();
	test_keyReference ();
	test_keyName ();
	test_keyNameSlashes ();
	test_keyValue ();
	test_keyBinary ();
	test_keyDup ();
	test_keyCopy ();
	test_binary ();
	test_keyNameSpecial ();
	test_keyClear ();
	test_keyBaseName ();
	test_keySetBaseName ();
	test_keyAddBaseName ();
	test_keyEscape ();
	test_keyAdd ();
	test_keyCascading ();

	printf ("\ntestabi_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
