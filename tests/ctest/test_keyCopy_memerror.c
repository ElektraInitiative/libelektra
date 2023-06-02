/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#define ksDup ksDupFailing

#include "../../src/libs/core/cow.c"
#include "../../src/libs/core/key.c"
#include "../../src/libs/core/keyhelpers.c"
#include "../../src/libs/core/keyname.c"
#include "../../src/libs/core/keyvalue.c"
#include "../../src/libs/core/opmphm.c"
#include "../../src/libs/core/opmphmpredictor.c"

#include <tests_internal.h>

KeySet * ksDupFailing (const KeySet * source ELEKTRA_UNUSED)
{
	// We override ksDup here to return NULL
	// This should cause keyCopy to fail
	return NULL;
}

static void keyCopy_with_memerror_should_not_modify_dest (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * source = keyNew ("system:/original", KEY_VALUE, "Hello, World!", KEY_END);
	Key * dest = keyNew ("user:/target", KEY_VALUE, "Target Value", KEY_END);

	keySetMeta (source, "meta:/m1", "Hello");
	keySetMeta (dest, "meta:/m2", "Bye");

	// Act
	keyCopy (dest, source, KEY_CP_ALL);

	// Assert
	succeed_if (dest->keyName != source->keyName, "should not have copied keyName");
	succeed_if (dest->keyData != source->keyData, "should not have copied keyData");
	succeed_if_same_string (dest->keyName->key, "user:/target");
	succeed_if_same_string (dest->keyData->data.c, "Target Value");
	succeed_if (keyGetMeta (dest, "meta:/m2") != NULL, "should still have meta");

	keyDel (source);
	keyDel (dest);
}


int main (int argc, char ** argv)
{
	printf ("KEYCOPY MEMERROR TESTS\n");
	printf ("======================\n\n");

	init (argc, argv);

	keyCopy_with_memerror_should_not_modify_dest ();

	print_result ("test_keyCopy_memerror");
	return nbError;
}
