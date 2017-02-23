/**
 * @file
 *
 * @brief Tests for typedispatcher plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

void testSimple()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/ace", "",
		KEY_META, "define/type/ace/check/range", "0-4,6-9",
		KEY_META, "define/type/foo", "",
		KEY_META, "define/type/foo/check/range", "4-20",
		KEY_META, "define/type/bar", "",
		KEY_META, "define/type/bar/check/range", "3-5,8-20", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/all", KEY_VALUE, "10", 
		KEY_META, "type", "#2",
		KEY_META, "type/#0", "ace", 
		KEY_META, "type/#1", "foo", 
		KEY_META, "type/#2", "bar", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/fookey", KEY_VALUE, "12",
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "foo", 
		KEY_END),
	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "bla failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();

}

void testSub()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/ab", "", 
		KEY_META, "define/type/ab/check/enum", "#1", 
		KEY_META, "define/type/ab/check/enum/#0", "a", 
		KEY_META, "define/type/ab/check/enum/#1","b", 
		KEY_META, "define/type/ab/type", "#0", 
		KEY_META, "define/type/ab/type/#0", "character", 
		KEY_META, "define/type/character", "", 
		KEY_META, "define/type/character/check/type", "char", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/ab", KEY_VALUE, "c", 
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "ab", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/char", KEY_VALUE, "c",
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "character", 
		KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "bla failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();

}

void testSub2()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/character", "", 
		KEY_META, "define/type/character/check/type", "char", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/character", KEY_VALUE, "c", 
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "ab", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/sub", KEY_VALUE, "", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/ab", "",
		KEY_META, "define/type/ab/check/enum", "#1",
		KEY_META, "define/type/ab/check/enum/#0", "a",
		KEY_META, "define/type/ab/check/enum/#1","b", 
		KEY_META, "define/type/ab/type", "#0",
		KEY_META, "define/type/ab/type/#0", "character", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/sub/ab", "c", 
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "ab", 
		KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "bla failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();

}

void testSimpleWithOthers()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "ameta", "",
		KEY_META, "ometa", "",
		KEY_META, "zmeta", "",
		KEY_META, "define/type", "", 
		KEY_META, "define/type/ace", "",
		KEY_META, "define/type/ace/check/range", "0-4,6-9",
		KEY_META, "define/type/foo", "",
		KEY_META, "define/type/foo/check/range", "4-20",
		KEY_META, "define/type/bar", "",
		KEY_META, "define/type/bar/check/range", "3-5,8-20", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/all", KEY_VALUE, "10", 
		KEY_META, "ameta", "",
		KEY_META, "ometa", "",
		KEY_META, "zmeta", "",
		KEY_META, "type", "#2",
		KEY_META, "type/#0", "ace", 
		KEY_META, "type/#1", "foo", 
		KEY_META, "type/#2", "bar", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/fookey", "12",
		    KEY_META, "ameta", "",
		    KEY_META, "ometa", "",
		    KEY_META, "zmeta", "",
		    KEY_META, "type", "#0", 
		    KEY_META, "type/#0", "foo", 
		    KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "bla failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();

}

int main (int argc, char ** argv)
{
    printf ("TYPEDISPATCHER     TESTS\n");
    printf ("==================\n\n");

    init (argc, argv);

    fprintf(stderr, "\n============================================\n");
    testSimple();
    fprintf(stderr, "\n============================================\n");
    testSub();
    fprintf(stderr, "\n============================================\n");
    testSub2();
    fprintf(stderr, "\n============================================\n");
    testSimpleWithOthers();
    fprintf(stderr, "\n============================================\n");

    printf ("\ntestmod_typedispatcher RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

    return nbError;
}
