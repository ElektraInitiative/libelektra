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
	    keyNew("user/tests/typedispatcher/all", KEY_VALUE, "4", 
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
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "Simple failed");
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
		KEY_META, "define/type/ab/check/enum/#1", "b", 
		KEY_META, "define/type/ab/type", "#0", 
		KEY_META, "define/type/ab/type/#0", "character", 
		KEY_META, "define/type/character", "", 
		KEY_META, "define/type/character/check/type", "char", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/ab", KEY_VALUE, "a", 
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
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "Sub failed");
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
		KEY_META, "type/#0", "character", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/sub", KEY_VALUE, "", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/ab", "",
		KEY_META, "define/type/ab/check/enum", "#1",
		KEY_META, "define/type/ab/check/enum/#0", "a",
		KEY_META, "define/type/ab/check/enum/#1", "b", 
		KEY_META, "define/type/ab/type", "#0",
		KEY_META, "define/type/ab/type/#0", "character", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/sub/ab", KEY_VALUE, "c", 
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "ab", 
		KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "Sub2 failed");
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
	    keyNew("user/tests/typedispatcher/all", KEY_VALUE, "4", 
		KEY_META, "ameta", "",
		KEY_META, "ometa", "",
		KEY_META, "zmeta", "",
		KEY_META, "type", "#2",
		KEY_META, "type/#0", "ace", 
		KEY_META, "type/#1", "foo", 
		KEY_META, "type/#2", "bar", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/fookey", KEY_VALUE, "12",
		    KEY_META, "ameta", "",
		    KEY_META, "ometa", "",
		    KEY_META, "zmeta", "",
		    KEY_META, "type", "#0", 
		    KEY_META, "type/#0", "foo", 
		    KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "SimpleWithOthers failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();

}

void testSimpleOutOfScope()
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
		KEY_META, "type/#0", "character", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/asub", KEY_VALUE, "",
		KEY_META, "define/type", "",
		KEY_META, "define/type/notReachable", "",
		KEY_META, "define/type/notReachable/check/type", "long",
		KEY_END),
	    keyNew("user/tests/typedispatcher/sub", KEY_VALUE, "", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/ab", "",
		KEY_META, "define/type/ab/check/enum", "#1",
		KEY_META, "define/type/ab/check/enum/#0", "a",
		KEY_META, "define/type/ab/check/enum/#1", "b", 
		KEY_META, "define/type/ab/type", "#1",
		KEY_META, "define/type/ab/type/#0", "character", 
		KEY_META, "define/type/ab/type/#1", "notReachable", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/sub/ab", KEY_VALUE, "a", 
		    KEY_META, "type", "#0", 
		    KEY_META, "type/#0", "ab", 
		    KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "OutOfScope test failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testSimpleClash()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/character", "", 
		KEY_META, "define/type/character/check/type", "char", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/character", KEY_VALUE, "a", 
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "character", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/sub", KEY_VALUE, "", 
		KEY_META, "define/type", "",
		KEY_META, "define/type/character", "", 
		KEY_META, "define/type/character/check/type", "char", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/sub/ab", KEY_VALUE, "b", 
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "ab", 
		KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "clash test failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testMultiSub()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/character", "", 
		KEY_META, "define/type/character/check/type", "char", 		
		KEY_META, "define/type/upperChar", "",
		KEY_META, "define/type/upperChar/check/range", "A-Z",
		KEY_META, "define/type/upperChar/check/range/type", "CHAR",
		KEY_META, "define/type/upperChar/type", "character",
		KEY_END), 
	    keyNew("user/tests/typedispatcher/character", KEY_VALUE, "c", 
		KEY_META, "type", "#0", 
		KEY_META, "type/#0", "character", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/asub", KEY_VALUE, "",
		KEY_META, "define/type", "",
		KEY_META, "define/type/notReachable", "",
		KEY_META, "define/type/notReachable/check/type", "long",
		KEY_END),
	    keyNew("user/tests/typedispatcher/sub", KEY_VALUE, "",
		KEY_META, "define/type", "",
		KEY_META, "define/type/AB", "",
		KEY_META, "define/type/AB/check/enum", "#1",
		KEY_META, "define/type/AB/check/enum/#0", "A",
		KEY_META, "define/type/AB/check/enum/#1", "B", 
		KEY_END),
	    keyNew("user/tests/typedispatcher/sub/sub", KEY_VALUE, "", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/multi", "",
		KEY_META, "define/type/multi/type", "#1",
		KEY_META, "define/type/multi/type/#0", "upperChar", 
		KEY_META, "define/type/multi/type/#1", "AB",
		KEY_END),
	    keyNew("user/tests/typedispatcher/sub/sub/Akey", KEY_VALUE, "A",
		KEY_META, "type", "multi", 
		KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "Multi test failed");
    fprintf(stderr, "\t=================\n");
    ksAppendKey(ks, keyNew("user/tests/typedispatcher/sub/sub/Ckey", KEY_VALUE, "C",
		KEY_META, "type", "multi", 
		KEY_END));
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "Multi test 2 failed");

    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testSimpleSum()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/sab", "",
		KEY_META, "define/type/sab/check/enum", "#1",
		KEY_META, "define/type/sab/check/enum/#0", "a",
		KEY_META, "define/type/sab/check/enum/#1", "b",
		KEY_META, "define/type/s", "sum",
		KEY_META, "define/type/s/check/range", "1-10",
		KEY_META, "define/type/s/type", "sab",
		KEY_END), 
	    keyNew("user/tests/typedispatcher/long", KEY_VALUE, "7", 
		KEY_META, "type", "s",
		KEY_END), 
	    keyNew("user/tests/typedispatcher/a", KEY_VALUE, "a",
		KEY_META, "type", "s",
		KEY_END),
	    keyNew("user/tests/typedispatcher/fail", KEY_VALUE, "c",
		KEY_META, "type", "s",
		KEY_END),
	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "SimpleSum failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testArraySum()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/s", "sum",
		KEY_META, "define/type/s/#0/check/enum", "#1",
		KEY_META, "define/type/s/#0/check/enum/#0", "a",
		KEY_META, "define/type/s/#0/check/enum/#1", "b",
		KEY_META, "define/type/s/#1/check/range", "1-10",
		KEY_END), 
	    keyNew("user/tests/typedispatcher/long", KEY_VALUE, "5", 
		KEY_META, "type", "s",
		KEY_END), 
	    keyNew("user/tests/typedispatcher/a", KEY_VALUE, "a",
		KEY_META, "type", "s",
		KEY_END),
	    keyNew("user/tests/typedispatcher/fail", KEY_VALUE, "c",
		KEY_META, "type", "s",
		KEY_END),
	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "SimpleSum failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testSimpleParameter()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/ab", "", 
		KEY_META, "define/type/ab/parameter", "c",
		KEY_META, "define/type/ab/check/enum", "#2", 
		KEY_META, "define/type/ab/check/enum/#0", "a", 
		KEY_META, "define/type/ab/check/enum/#1", "b", 
		KEY_META, "define/type/ab/check/enum/#2", "%c%", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/ab", KEY_VALUE, "a", 
		KEY_META, "type", "ab (c)", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/c", KEY_VALUE, "c",
		KEY_META, "type", "ab (c)", 
		KEY_END), 
	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "SimpleParameter failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testSimpleParameterFail()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/ab", "", 
		KEY_META, "define/type/ab/parameter", "c",
		KEY_META, "define/type/ab/check/enum", "#2", 
		KEY_META, "define/type/ab/check/enum/#0", "a", 
		KEY_META, "define/type/ab/check/enum/#1", "b", 
		KEY_META, "define/type/ab/check/enum/#2", "%c%", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/ab", KEY_VALUE, "a", 
		KEY_META, "type", "ab (c)", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/c", KEY_VALUE, "c",
		KEY_META, "type", "ab (c)", 
		KEY_END),
	    keyNew("user/tests/typedispatcher/d", KEY_VALUE, "d", 
		KEY_META, "type", "ab (c)", 
		KEY_END), 
	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "SimpleParameterFail failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testMultipleParameter()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/paramEnum", "", 
		KEY_META, "define/type/paramEnum/parameter", "#2",
		KEY_META, "define/type/paramEnum/parameter/#0", "x",
		KEY_META, "define/type/paramEnum/parameter/#1", "y",
		KEY_META, "define/type/paramEnum/parameter/#2", "z",
		KEY_META, "define/type/paramEnum/check/enum", "#2", 
		KEY_META, "define/type/paramEnum/check/enum/#0", "%x%", 
		KEY_META, "define/type/paramEnum/check/enum/#1", "%y%", 
		KEY_META, "define/type/paramEnum/check/enum/#2", "%z%", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/a", KEY_VALUE, "a", 
		KEY_META, "type", "paramEnum (a,b,c)", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/c", KEY_VALUE, "c",
		KEY_META, "type", "paramEnum (a,b,c)",
		KEY_END), 
	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "SimpleParameter failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testMultipleParameterFail()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/paramEnum", "", 
		KEY_META, "define/type/paramEnum/parameter", "#2",
		KEY_META, "define/type/paramEnum/parameter/#0", "x",
		KEY_META, "define/type/paramEnum/parameter/#1", "y",
		KEY_META, "define/type/paramEnum/parameter/#2", "z",
		KEY_META, "define/type/paramEnum/check/enum", "#2", 
		KEY_META, "define/type/paramEnum/check/enum/#0", "%x%", 
		KEY_META, "define/type/paramEnum/check/enum/#1", "%y%", 
		KEY_META, "define/type/paramEnum/check/enum/#2", "%z%", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/a", KEY_VALUE, "a", 
		KEY_META, "type", "paramEnum (d,e,f)", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/c", KEY_VALUE, "i",
		KEY_META, "type", "paramEnum (j,k,l)",
		KEY_END), 
	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "SimpleParameter failed");
    ksDel (ks);
    keyDel (parentKey);
    PLUGIN_CLOSE ();
}

void testMultiParametersExtended()
{
    Key * parentKey = keyNew ("user/tests/typedispatcher", KEY_VALUE, "", KEY_END);
    KeySet * ks = ksNew (5, 
	    keyNew ("user/tests/typedispatcher", KEY_VALUE, "typedefinitions", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/character", "", 
		KEY_META, "define/type/character/parameter", "a", 
		KEY_META, "define/type/character/check/type", "%a%", 		
		KEY_META, "define/type/upperChar", "",
		KEY_META, "define/type/upperChar/parameter", "b",
		KEY_META, "define/type/upperChar/check/range", "%b%",
		KEY_META, "define/type/upperChar/check/range/type", "CHAR",
		KEY_META, "define/type/upperChar/type", "character (char)",
		KEY_END), 
	    keyNew("user/tests/typedispatcher/character", KEY_VALUE, "c", 
		KEY_META, "type", "character (char)", 
		KEY_END), 
	    keyNew("user/tests/typedispatcher/sub", KEY_VALUE, "",
		KEY_META, "define/type", "",
		KEY_META, "define/type/AB", "",
		KEY_META, "define/type/AB/parameter", "#1",
		KEY_META, "define/type/AB/parameter/#0", "c",
		KEY_META, "define/type/AB/parameter/#1", "d",
		KEY_META, "define/type/AB/check/enum", "#1",
		KEY_META, "define/type/AB/check/enum/#0", "%c%",
		KEY_META, "define/type/AB/check/enum/#1", "%d%", 
		KEY_END),
	    keyNew("user/tests/typedispatcher/sub/sub", KEY_VALUE, "", 
		KEY_META, "define/type", "", 
		KEY_META, "define/type/multi", "",
		KEY_META, "define/type/multi/parameter", "#1",
		KEY_META, "define/type/multi/parameter/#0", "e",
		KEY_META, "define/type/multi/parameter/#1", "f",
		KEY_META, "define/type/multi/type", "#1",
		KEY_META, "define/type/multi/type/#0", "upperChar (A-Z)", 
		KEY_META, "define/type/multi/type/#1", "AB (A,B)",
		KEY_END),
	    keyNew("user/tests/typedispatcher/sub/sub/Akey", KEY_VALUE, "A",
		KEY_META, "type", "multi", 
		KEY_END),

	    KS_END);
    KeySet * conf = ksNew (0, KS_END);
    PLUGIN_OPEN ("typedispatcher");
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "MultiParameters Ext 1 failed");
    fprintf(stderr, "\t=================\n");
    ksAppendKey(ks, keyNew("user/tests/typedispatcher/sub/sub/Ckey", KEY_VALUE, "C",
		KEY_META, "type", "multi", 
		KEY_END));
    succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "MultiParameters Ext 2 failed");
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
    testSimpleOutOfScope();
    fprintf(stderr, "\n============================================\n");
    testSimpleClash();
    fprintf(stderr, "\n============================================\n");
    testMultiSub();
    fprintf(stderr, "\n============================================\n");
    testSimpleSum();
    fprintf(stderr, "\n============================================\n");
    testArraySum();
    fprintf(stderr, "\n============================================\n");
    testSimpleParameter();
    fprintf(stderr, "\n============================================\n");
    testSimpleParameterFail();
    fprintf(stderr, "\n============================================\n");
    testMultipleParameter();
    fprintf(stderr, "\n============================================\n");
    testMultipleParameterFail();
    fprintf(stderr, "\n============================================\n");
    testMultiParametersExtended();
    fprintf(stderr, "\n============================================\n");
    printf ("\ntestmod_typedispatcher RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

    return nbError;
}
