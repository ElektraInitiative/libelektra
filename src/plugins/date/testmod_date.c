/**
 * @file
 *
 * @brief Tests for date plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>


static void testFmt (const char * date, const char * fmt, const short res)
{
	Key * parentKey = keyNew ("user/tests/date", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user/tests/date/test", KEY_VALUE, date, KEY_META, "check/date", "POSIX", KEY_META,
					"check/date/format", fmt, KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("date");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == res, "validation failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testIso (const char * date, const char * isoString, const short res)
{
	Key * parentKey = keyNew ("user/tests/date", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user/tests/date/test", KEY_VALUE, date, KEY_META, "check/date", "ISO8601", KEY_META,
					"check/date/format", isoString, KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("date");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == res, "validation failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testRfc2822 (const char * date, const short res)
{
	Key * parentKey = keyNew ("user/tests/date", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user/tests/date/test", KEY_VALUE, date, KEY_META, "check/date", "RFC2822", KEY_META,
					"check/date/format", "", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("date");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == res, "validation failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("DATE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testFmt ("20:15:00", "%H:%M:%S", 1);
	testFmt ("20:15:00", "%I:%M:%S", -1);
	testFmt ("Sat 17 Dec 2016 08:07:43 PM CET", "%a %d %b %Y %r %Z", 1);

	testIso ("2016-12-12T23:59:01Z", "<datetimecomplete>", 1);
	testIso ("2016-12-12T23:59:01Z", "<datetimeother>", -1);
	testIso ("2016-12-12T23:59:01Z", "<datetimeother>", -1);
	testIso ("2016-W23", "<weekdate>", 1);
	testIso ("22:30+04", "<utc>", 1);
	testIso ("22:30-04", "<utc>", 1);

	testRfc2822 ("Sat, 01 Mar 2016 23:59:01 +0400", 1);
	testRfc2822 ("01 Mar 2016 23:59:01 -0400", 1);
	testRfc2822 ("Sat, Mar 01 2016 23:59:01 +0400", -1);
	testRfc2822 ("01 Mar 2016 23:59 +0400", 1);
	testRfc2822 ("01 Mar 2016 01:00:59", -1);

	printf ("\ntestmod_date RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
