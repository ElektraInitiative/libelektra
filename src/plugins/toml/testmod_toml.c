/**
 * @file
 *
 * @brief Tests for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbprivate.h>
#include <tests.h>
#include <tests_plugin.h>

#include "toml.h"

#define PREFIX "user/tests/toml-read"


static void printKs (KeySet * ks, const char * name)
{
	printf ("######KEYSET: %s\n", name);
	ksRewind (ks);
	Key * key = ksNext (ks);
	while (key != NULL)
	{
		printf ("Key: '%s'\t->\t'%s'", keyName (key), keyString (key));

		keyRewindMeta (key);
		const Key * meta = keyNextMeta (key);
		while (meta != NULL)
		{
			printf ("\n\tMeta: '%s'\t->\t'%s'", keyName (meta), keyString (meta));
			meta = keyNextMeta (key);
		}
		key = ksNext (ks);
		printf ("\n");
	}
}

static void showDiff (KeySet * expected, KeySet * is, const char * name, bool stopOnFirstDiff)
{
	bool headerPrinted = false;
	ksRewind (expected);
	ksRewind (is);
	Key * kExp = ksNext (expected);
	Key * kIs = ksNext (is);
	while (kExp != NULL && kIs != NULL)
	{
		if (keyCmp (kExp, kIs) != 0 || strcmp (keyString (kExp), keyString (kIs)) != 0)
		{
			if (!headerPrinted)
			{
				printf ("###### Diffs in '%s'\n", name);
				headerPrinted = true;
			}
			printf ("Key diff:\n\texpected\t= '%s'\n\tcontent:\t'%s'\n\tfound\t\t= '%s'\n\tcontent:\t: '%s'\n", keyName (kExp), keyString (kExp), keyName (kIs), keyString (kIs));
			if (stopOnFirstDiff)
			{
				return;
			}
		}
		keyRewindMeta (kExp);
		keyRewindMeta (kIs);
		const Key * metaExp = keyNextMeta (kExp);
		const Key * metaIs = keyNextMeta (kIs);
		while (metaExp != NULL && metaIs != NULL)
		{
			if (keyCmp (metaExp, metaIs) != 0 || strcmp (keyString (metaExp), keyString (metaIs)) != 0)
			{
				if (!headerPrinted)
				{
					printf ("###### Diffs in '%s'\n", name);
					headerPrinted = true;
				}
				printf ("MetaKey diff:\n\texpected\t= '%s'\n\tcontent:\t'%s' = '%s'\n\tfound\t\t= '%s'\n\tcontent:\t'%s' = "
					"'%s'\n",
					keyName (kExp), keyName (metaExp), keyString (metaExp), keyName (kIs), keyName (metaIs),
					keyString (metaIs));
				if (stopOnFirstDiff)
				{
					return;
				}
			}
			metaExp = keyNextMeta (kExp);
			metaIs = keyNextMeta (kIs);
		}
		if (metaExp != NULL || metaIs != NULL)
		{
			if (!headerPrinted)
			{
				printf ("###### Diffs in '%s'\n", name);
				headerPrinted = true;
			}
			printf ("Mismatching metakeys count, there are %s metakeys generated than expected\n",
				metaIs == NULL ? "less" : "too much");
			printf ("Affected keys:\n\texpected\t= '%s'\n\tfound\t\t= '%s'\n", keyName (kExp), keyName (kIs));
			Key * overhead = metaExp != NULL ? kExp : kIs;
			const Key * meta = metaExp != NULL ? metaExp : metaIs;
			do
			{
				printf ("%s Metakeys: '%s': '%s'\n", metaIs == NULL ? "Missing" : "Overhead", keyName (meta),
					keyString (meta));
				meta = keyNextMeta (overhead);
			} while (meta != NULL);
			if (stopOnFirstDiff)
			{
				return;
			}
		}
		kExp = ksNext (expected);
		kIs = ksNext (is);
	}
	if (kExp != NULL || kIs != NULL)
	{
		if (!headerPrinted)
		{
			printf ("###### Diffs in '%s'\n", name);
			headerPrinted = true;
		}
		printf ("Mismatching keyset size, there are %s keys generated than expected\n", kIs == NULL ? "less" : "too much");
		return;
	}
}

static void testRead (const char * filename, KeySet * expected)
{
	// printf ("################################################\n");
	// printf ("############ testRead (%s)\n", filename);
	// printf ("################################################\n");
	printf ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	Key * root = ksLookupByName (expected, PREFIX, KDB_O_POP);
	if (root != NULL)
	{
		if (strcmp (keyString (root), "@CONFIG_FILEPATH@") == 0)
		{
			keySetString (root, srcdir_file (filename));
			ksAppendKey (expected, root);
		}
	}
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	compare_keyset (expected, ks);

	showDiff (expected, ks, filename, true);
	// printKs (expected, "expected");
	// printKs (ks, "parsed");

	PLUGIN_CLOSE ();
	ksDel (expected);
}


int main (int argc, char ** argv)
{
	init (argc, argv);

	testRead ("toml/basic.toml",
#include "toml/basic.h"
	);
	testRead ("toml/utf8.toml",
#include "toml/utf8.h"
	);

    testRead ("toml/multiline_strings.toml",
#include "toml/multiline_strings.h"
    );

	testRead ("toml/date.toml",
#include "toml/date.h"
	);

	testRead ("toml/array.toml",
#include "toml/array.h"
	);

	testRead ("toml/simple_table.toml",
#include "toml/simple_table.h"
	);

	testRead ("toml/table_array.toml",
#include "toml/table_array.h"
	);

	testRead ("toml/table_array_nested.toml",
#include "toml/table_array_nested.h"
	);

	testRead ("toml/table_array_table_mixed.toml",
#include "toml/table_array_table_mixed.h"
	);

	testRead ("toml/inline_table.toml",
#include "toml/inline_table.h"
	);

	testRead ("toml/inline_table_empty.toml",
#include "toml/inline_table_empty.h"
	);

	testRead ("toml/inline_table_multiline_values.toml",
#include "toml/inline_table_multiline_values.h"
	);

	testRead ("toml/comment.toml",
#include "toml/comment.h"
	);

	print_result ("testmod_toml");
	return nbError;
}
