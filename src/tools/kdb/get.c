/**
 * @file
 *
 * @brief KDB get subcommand
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <get.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define COMMAND_NAME "get"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addGetSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Get the value of an individual key.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/all", KEY_META, "description", "Consider all of the keys", KEY_META,
				   "opt", "a", KEY_META, "opt/long", "all", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execGet (KeySet * options, Key * errorKey)
{
	int ret = 0;
	bool verbose = false;
	Key * tmp = GET_OPTION_KEY (options, "verbose");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "verbose"), &verbose);
	}

	bool all = false;
	tmp = GET_OPTION_KEY (options, "all");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "all"), &all);
	}

	const char * name = getKeyNameFromOptions (options, GET_OPTION (options, "name"), errorKey, verbose);
	if (name == NULL) return 1;

	KeySet * searchIn = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);
	Key * toLookUp = keyNew (name, KEY_END);

	if (toLookUp == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "'%s' is not a valid key name", name);
		kdbClose (handle, errorKey);
		ksDel (searchIn);
		keyDel (toLookUp);
		return -1;
	}

	Key * parentKey = toLookUp;
	if (all)
	{
		parentKey = keyNew ("/", KEY_END);
	}
	if (kdbGet (handle, searchIn, parentKey) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", name, GET_ERR (toLookUp));
		kdbClose (handle, errorKey);
		ksDel (searchIn);
		keyDel (toLookUp);
		keyDel (parentKey);
		return 1;
	}
	kdbClose (handle, errorKey);
	keyDel (parentKey);

	setCallback (toLookUp, warnOnMeta);
	if (verbose)
	{
		printf ("got %ld keys\n", ksGetSize (searchIn));
		setCallback (toLookUp, printTrace);
	}

	Key * found = ksLookup (searchIn, toLookUp, KDB_O_CALLBACK);

	if (keyCopyAllMeta (errorKey, toLookUp) == -1)
	{
		ELEKTRA_SET_INTERNAL_ERROR (errorKey, "could not copy key meta to errorKey");
		ksDel (searchIn);
		keyDel (toLookUp);
		return 1;
	}

	if (found == NULL)
	{
		printf ("Did not find key '%s'", name);
		ret = -1;
		goto cleanup;
	}

	if (verbose)
	{
		if (keyGetNamespace (found) == KEY_NS_DEFAULT)
		{
			printf ("The key was not found in any other namespace, taking the default\n");
		}
		printf ("The resulting keyname is %s\n", keyName (found));
		printf ("The resulting value size is %ld\n", keyGetValueSize (found));
	}
	if (keyIsBinary (found))
	{
		ssize_t binSize = keyGetValueSize (found);
		if (verbose)
		{
			printf ("The key is %s.\n", binSize == 0 ? "null" : "binary");
		}
		const char * buffer = keyValue (found);
		for (ssize_t pos = 0; pos < binSize; pos++)
		{
			printf ("%x", buffer[pos]);
		}
	}
	else
	{
		printf ("%s", keyString (found));
	}

cleanup:
	printf ("\n");
	ksDel (searchIn);
	keyDel (toLookUp);
	return ret;
}

void printOptions (elektraLookupFlags options)
{
	// :'<,'>s/\(.*\)/^Iif(options \& \1) printf("\1 ");
	if (options & KDB_O_SPEC) printf ("KDB_O_SPEC ");
	if (options & KDB_O_CREATE) printf ("KDB_O_CREATE ");
	if (options & KDB_O_NOCASCADING) printf ("KDB_O_NOCASCADING ");
	if (options & KDB_O_NOSPEC) printf ("KDB_O_NOSPEC ");
	if (options & KDB_O_NODEFAULT) printf ("KDB_O_NODEFAULT ");
	if (options & KDB_O_CALLBACK) printf ("KDB_O_CALLBACK");
}

const char * getCascadingName (const char * str)
{
	if (str == NULL) return "/";
	const char * r = strchr (str, '/');
	return r == NULL ? "/" : r;
}

Key * warnOnMeta (ELEKTRA_UNUSED KeySet * ks, ELEKTRA_UNUSED Key * key, Key * found, elektraLookupFlags options)
{
	if (found != NULL && strncmp (keyName (found), "spec:/", 6) == 0 && options == KDB_O_CALLBACK)
	{
		const Key * meta = keyGetMeta (found, "context");
		if (meta != NULL)
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (
				key, "%s is context dependent, shown result might be wrong, -v shows you the trace to the key\n",
				keyName (found));
		}
	}
	return found;
}


Key * printTrace (ELEKTRA_UNUSED KeySet * ks, Key * key, Key * found, elektraLookupFlags options)
{
	warnOnMeta (ks, key, found, options);
	const char * lastKeyName = keyValue (keyGetMeta (key, "callback/print_trace/last_key_name"));
	const char * name = keyName (key);
	const char * rawDepth = keyValue (keyGetMeta (key, "callback/print_trace/depth"));

	int depth = rawDepth == NULL ? 0 : atoi (rawDepth);
	for (int i = 0; i < depth; ++i)
	{
		printf (" ");
	}

	printf ("searching %s%s", (name[0] == '/' ? "default of spec" : ""), name);
	printf (", found: %s", (found != NULL ? keyName (found) : "<nothing>"));
	if (options)
	{
		printf (", options: ");
		printOptions (options);
	}
	printf ("\n");

	int newDepth = depth;
	if (elektraStrNCmp (name, "spec:/", 6) == 0 && (options & KDB_O_CALLBACK))
	{
		newDepth += 4;
	}
	else if (elektraStrCmp (getCascadingName (lastKeyName), getCascadingName (name)) != 0)
	{
		newDepth = depth != 0 ? depth - 2 : depth;
	}
	if (newDepth != depth)
	{
		char buff[11];
		snprintf (buff, 11, "%d", newDepth);
		keySetMeta (key, "callback/print_trace/depth", buff);
	}
	keySetMeta (key, "callback/print_trace/last_key_name", name);
	return found;
}

void setCallback (Key * key, Key * (*f) (KeySet * ks, Key * key, Key * found, elektraLookupFlags flags))
{
	union
	{
		Key * (*f) (KeySet * ks, Key * key, Key * found, elektraLookupFlags flags);
		void * v;
	} conversation;

	conversation.f = f;
	keySetBinary (key, &conversation.v, sizeof (conversation));
	keySetMeta (key, "callback", "");
}
