/**
 * @file
 *
 * @brief Tests for date plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifdef __GNU_LIBRARY__
#include <features.h>
#endif

#include <locale.h>
#include <stdlib.h>
#include <string.h>

#include <internal/config.h>

#include <tests_plugin.h>


static void testFmt (const char * date, const char * fmt, const short res)
{
	Key * parentKey = keyNew ("user:/tests/date", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/date/test", KEY_VALUE, date, KEY_META, "check/date", "POSIX", KEY_META,
				     "check/date/format", fmt, KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("date");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == res, "validation failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testIso (const char * date, const char * isoString, const short expectedResult)
{
	Key * parentKey = keyNew ("user:/tests/date", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/date/test", KEY_VALUE, date, KEY_META, "check/date", "ISO8601", KEY_META,
				     "check/date/format", isoString, KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("date");
	short actualResult = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if_fmt (expectedResult == actualResult, "validation for %s, %s failed. Expected result: %hi, actual result %hi", date,
			isoString, expectedResult, actualResult);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

#ifdef __GNU_LIBRARY__
static void testRfc2822 (const char * date, const short expectedResult)
{
	Key * parentKey = keyNew ("user:/tests/date", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/date/test", KEY_VALUE, date, KEY_META, "check/date", "RFC2822", KEY_META,
				     "check/date/format", "", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("date");
	short actualResult = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if_fmt (expectedResult == actualResult, "validation for %s failed. Expected result: %hi, actual result %hi", date,
			expectedResult, actualResult);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void testRfc822 (const char * date, const short expectedResult)
{
	Key * parentKey = keyNew ("user:/tests/date", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/date/test", KEY_VALUE, date, KEY_META, "check/date", "RFC822", KEY_META,
				     "check/date/format", "", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("date");
	short actualResult = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if_fmt (expectedResult == actualResult, "validation for %s failed. Expected result: %hi, actual result %hi", date,
			expectedResult, actualResult);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
#endif

int main (int argc, char ** argv)
{
	printf ("DATE     TESTS\n");
	printf ("==================\n\n");

#ifdef __GNU_LIBRARY__
	const char * old_locale = setlocale (LC_ALL, NULL);
#endif

	init (argc, argv);

	testFmt ("20:15:00", "%H:%M:%S", 1);
	testFmt ("20:15:00", "%I:%M:%S", -1);
#ifdef __GNU_LIBRARY__
	setlocale (LC_ALL, "C");
	testFmt ("Sat 17 Dec 2016 08:07:43 PM CET", "%a %d %b %Y %r %Z", 1);
	setlocale (LC_ALL, old_locale);
#else
	testFmt ("Sat 17 Dec 2016 08:07:43 PM", "%a %d %b %Y %r", 1);
#endif

#ifdef __GNU_LIBRARY__
	testIso ("2016-12-12T23:59:01Z", "datetime complete", 1);
	testIso ("2016-12-12 23:59:01Z", "datetime complete noT", 1);
	testIso ("2016-12-12T23:59:01Z", "datetime truncated", -1);
	testIso ("2016-12-12T23:59:01", "datetime complete", 1);
	testIso ("-12-12T23:59:01Z", "datetime truncated", 1);
	testIso ("22:30+04", "utc extended", 1);
	testIso ("22:30-04", "utc extended", 1);
	testIso ("2016-W23", "weekdate", 1);
	testIso ("2022-11-23", "calendardate", 1);
	testIso ("2022-327", "ordinaldate", 1);
	testIso ("2022-11-23", "date", 1);
	testIso ("2022-327", "date", 1);
	testIso ("2022-W47", "date", 1);

	setlocale (LC_ALL, "C");
	testRfc2822 ("Sat, 01 Mar 2016 23:59:01 +0400", 1);
	testRfc2822 ("01 Mar 2016 23:59:01 -0400", 1);
	testRfc2822 ("Sat, Mar 01 2016 23:59:01 +0400", -1);
	testRfc2822 ("01 Mar 2016 23:59 +0400", 1);
	testRfc2822 ("01 Mar 2016 01:00:59", -1);

	testRfc822 ("Sat, 01 Mar 16 23:59:01 +0400", 1);
	testRfc822 ("Sat, 01 Mar 2016 23:59:01 +0400", -1);
	testRfc822 ("01 Mar 16 23:59:01 -0400", 1);
	testRfc822 ("01 Mar 2016 23:59:01 -0400", -1);
	testRfc822 ("Sat, Mar 01 16 23:59:01 +0400", -1);
	testRfc822 ("01 Mar 16 23:59 +0400", 1);
	testRfc822 ("01 Mar 16 01:00", -1);
	testRfc822 ("Sat, 01 Mar 16 01:00", -1);

	setlocale (LC_ALL, old_locale);
#else
	testIso ("2016-12-12T23:59:01", "datetime truncated", -1);
	testIso ("-12-12T23:59:01", "datetime truncated", 1);
#endif
	testIso ("2230", "timeofday extended", -1);
	testIso ("2230", "timeofday basic", 1);
	testIso ("2230", "timeofday extended", -1);
	testIso ("2230", "time", 1);

	testIso ("2022-11-23", "bogus", -1);


	print_result ("testmod_date");

	return nbError;
}
