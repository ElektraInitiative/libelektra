/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/key.h>
#include <stdio.h>
#include <string.h>


void printNameAndContent (Key * k, char * varName)
{
	char buffer[256];
	strncpy (buffer, keyString (k), sizeof (buffer) - 1);
	printf ("%s: %s\t\tvalue: %s\n", varName, keyName (k), buffer);
}

int main (void)
{
	Key * dup;
	Key * key1 = keyNew ("user:/foo/bar", KEY_VALUE, "content of key1", KEY_END);
	Key * key2 = keyNew ("user:/hello", KEY_VALUE, "a unique string", KEY_END);

	printf ("Original name and content of 'key1' to 'key2'\n");
	printNameAndContent (key1, "key1");
	printNameAndContent (key2, "key2");
	printf ("\n");

	printf ("Duplicate 'key2' to 'dup'\n");
	dup = keyCopy (keyNew ("/", KEY_END), key2, KEY_CP_ALL);
	printNameAndContent (dup, "dup");
	printNameAndContent (key2, "key2");
	printf ("\n");

	printf ("Copy only the content of 'key1' to 'key2'\n");
	keySetString (key1, "a new content");
	keyCopy (key2, key1, KEY_CP_STRING);
	printNameAndContent (key1, "key1");
	printNameAndContent (key2, "key2");
	printf ("\n");

	printf ("Copy only the name of 'key1' to 'key2'\n");
	keySetName (key1, "user:/this/is/new");
	keySetString (key1, "the content has changed too!");
	keyCopy (key2, key1, KEY_CP_NAME);
	printNameAndContent (key1, "key1");
	printNameAndContent (key2, "key2");
	printf ("\n");

	printf ("Copy everything from 'key1' to 'key2'\n");
	keySetName (key1, "user:/all/changed");
	keySetString (key1, "an even newer content");
	keyCopy (key2, key1, KEY_CP_ALL);
	printNameAndContent (key1, "key1");
	printNameAndContent (key2, "key2");
	printf ("\n");

	printf ("Copy everything from 'NULL' to 'key2'. \n");
	// This erases the name and value and metadata but not the flags, reference count, etc
	keyCopy (key2, NULL, KEY_CP_ALL);
	printNameAndContent (key2, "key2");
	printf ("\n");

	keyDel (key2);
	keyDel (key1);
	keyDel (dup);
}
