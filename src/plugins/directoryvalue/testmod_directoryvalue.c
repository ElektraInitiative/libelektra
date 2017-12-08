/**
 * @file
 *
 * @brief Tests for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define INIT_PLUGIN(parent)                                                                                                                \
	Key * parentKey = keyNew (parent, KEY_END);                                                                                        \
	KeySet * conf = ksNew (0, KS_END);                                                                                                 \
	PLUGIN_OPEN ("directoryvalue")

#define CLOSE_PLUGIN()                                                                                                                     \
	keyDel (parentKey);                                                                                                                \
	ksDel (keySet);                                                                                                                    \
	PLUGIN_CLOSE ()

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

static KeySet * arrayWithDirValues (void)
{
	// clang-format off
	return ksNew (10,
      		      keyNew ("user/movies", KEY_VALUE, "🎬", KEY_META, "comment", "🍿", KEY_META, "array", "#2", KEY_END),
      		      keyNew ("user/movies/#0", KEY_VALUE, "A Silent Voice", KEY_END),
      		      keyNew ("user/movies/#1", KEY_VALUE, "Me and Earl and the Dying Girl", KEY_END),
      		      keyNew ("user/movies/#2", KEY_VALUE, "12 Angry Men", KEY_END),
		      KS_END);
	// clang-format on
}

static KeySet * arrayWithoutDirValues (void)
{
	// clang-format off
	return ksNew (10,
      		      keyNew ("user/movies", KEY_END),
      		      keyNew ("user/movies/#0", KEY_VALUE, "___dirdata: 🎬", KEY_META, "comment", "🍿", KEY_END),
      		      keyNew ("user/movies/#1", KEY_VALUE, "A Silent Voice", KEY_END),
      		      keyNew ("user/movies/#2", KEY_VALUE, "Me and Earl and the Dying Girl", KEY_END),
      		      keyNew ("user/movies/#3", KEY_VALUE, "12 Angry Men", KEY_END),
		      KS_END);
	// clang-format on
}

static KeySet * keySetWithDirValues (void)
{
	// clang-format off
	return ksNew (20,
		      keyNew ("user/grandparent", KEY_VALUE, "Grandparent", KEY_END),
		      keyNew ("user/grandparent/leaf", KEY_VALUE, "Leaf", KEY_END),
		      keyNew ("user/grandparent/parent", KEY_VALUE, "Parent", KEY_END),
		      keyNew ("user/grandparent/parent/child", KEY_VALUE, "Child", KEY_END),
		      keyNew ("user/mother", KEY_VALUE, "Mother", KEY_END),
		      keyNew ("user/mother/daughter", KEY_VALUE, "Daughter", KEY_END),
		      keyNew ("user/mother/son", KEY_VALUE, "Son", KEY_END),
		      keyNew ("user/characters", KEY_META, "array", "#1", KEY_END),
		      keyNew ("user/characters/#0", KEY_VALUE, "Rick Sanchez", KEY_END),
		      keyNew ("user/characters/#1", KEY_VALUE, "Morty Smith", KEY_END),
		      KS_END);
	// clang-format on
}

static KeySet * keySetWithoutDirValues (void)
{
	// clang-format off
	return ksNew (20,
		      keyNew ("user/grandparent", KEY_END),
		      keyNew ("user/grandparent/___dirdata", KEY_VALUE, "Grandparent", KEY_END),
		      keyNew ("user/grandparent/leaf", KEY_VALUE, "Leaf", KEY_END),
		      keyNew ("user/grandparent/parent", KEY_END),
		      keyNew ("user/grandparent/parent/___dirdata", KEY_VALUE, "Parent", KEY_END),
		      keyNew ("user/grandparent/parent/child", KEY_VALUE, "Child", KEY_END),
		      keyNew ("user/mother", KEY_END),
		      keyNew ("user/mother/___dirdata", KEY_VALUE, "Mother", KEY_END),
		      keyNew ("user/mother/daughter", KEY_VALUE, "Daughter", KEY_END),
		      keyNew ("user/mother/son", KEY_VALUE, "Son", KEY_END),
		      keyNew ("user/characters", KEY_META, "array", "#1", KEY_END),
		      keyNew ("user/characters/#0", KEY_VALUE, "Rick Sanchez", KEY_END),
		      keyNew ("user/characters/#1", KEY_VALUE, "Morty Smith", KEY_END),
		      KS_END);
	// clang-format on
}

// =========
// = Tests =
// =========

static void test_contract (void)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]")))
#endif
{
	printf ("• Retrieve plugin contract\n");

	INIT_PLUGIN ("system/elektra/modules/directoryvalue");
	KeySet * keySet = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not retrieve plugin contract");
	CLOSE_PLUGIN ();
}

static void test_set (char const * const message, KeySet * keySet, KeySet * expected)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[deep nested block]"), annotate ("oclint:suppress[high ncss method]"),
			annotate ("oclint:suppress[high npath complexity]"), annotate ("oclint:suppress[high cyclomatic complexity]")))
#endif
{
	printf ("%s\n", message);

	INIT_PLUGIN ("user");

	succeed_if (plugin->kdbSet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to open plugin in set direction");
	compare_keyset (keySet, expected); //! OCLint
	ksDel (expected);

	CLOSE_PLUGIN ();
}

static void test_get (void)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[deep nested block]"), annotate ("oclint:suppress[high ncss method]"),
			annotate ("oclint:suppress[high npath complexity]"), annotate ("oclint:suppress[high cyclomatic complexity]")))
#endif
{
	printf ("• Test get method\n");
	INIT_PLUGIN ("user");

	KeySet * keySet = keySetWithoutDirValues ();
	KeySet * expected = keySetWithDirValues ();
	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to open plugin in get direction");
	compare_keyset (keySet, expected); //! OCLint
	ksDel (expected);

	CLOSE_PLUGIN ();
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char ** argv)
{
	printf ("Directory Value Tests\n");
	printf ("=====================\n\n");

	init (argc, argv);

	test_contract ();
	test_set ("• Test set method", keySetWithDirValues (), keySetWithoutDirValues ());
	test_set ("• Test set method with array values", arrayWithDirValues (), arrayWithoutDirValues ());
	test_get ();

	print_result ("testmod_directoryvalue");

	return nbError;
}
