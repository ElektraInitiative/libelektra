/**
 * @file
 *
 * @brief Test suite for Libease functions accessing key name data.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <kdbutility.h>

#include "tests.h"

#define MAX_LENGTH 100

static void test_lskip ()
{
	printf ("Test lskip\n");

	succeed_if_same_string (lskip (""), "");
	succeed_if_same_string (lskip ("No Leading Whitespace"), "No Leading Whitespace");
	succeed_if_same_string (lskip ("\tLeading Tab"), "Leading Tab");
	succeed_if_same_string (lskip (" Leading Space"), "Leading Space");
	succeed_if_same_string (lskip (" \tLeading And Trailing Whitespace\t\n "), "Leading And Trailing Whitespace\t\n ");
}

static void test_rstrip ()
{
	printf ("Test rstrip\n");

	char text[MAX_LENGTH];
	char * last = NULL;

	strncpy (text, "", MAX_LENGTH);
	succeed_if_same_string (rstrip (text, NULL), "");
	rstrip (text, &last);
	succeed_if_same_string (last, text);

	strncpy (text, "No Trailing Whitespace", MAX_LENGTH);
	succeed_if_same_string (rstrip (text, NULL), "No Trailing Whitespace");
	last = NULL;
	rstrip (text, &last);
	succeed_if_same_string (last, "e");

	strncpy (text, "\t\nLeading Whitespace", MAX_LENGTH);
	succeed_if_same_string (rstrip (text, NULL), "\t\nLeading Whitespace");
	last = NULL;
	rstrip (text, &last);
	succeed_if_same_string (last, "e");

	strncpy (text, "Trailing Tab\t", MAX_LENGTH);
	succeed_if_same_string (rstrip (text, NULL), "Trailing Tab");
	strncpy (text, "Trailing Tab\t", MAX_LENGTH);
	last = NULL;
	rstrip (text, &last);
	succeed_if_same_string (last, "b");

	strncpy (text, "Trailing Whitespace\n\r\t  ", MAX_LENGTH);
	succeed_if_same_string (rstrip (text, NULL), "Trailing Whitespace");
	strncpy (text, "Trailing Whitespace\n\r\t  ", MAX_LENGTH);
	last = NULL;
	rstrip (text, &last);
	succeed_if_same_string (last, "e");

	strncpy (text, "\r  \t\nLeading And Trailing Whitespace\n  \r\n\t", MAX_LENGTH);
	succeed_if_same_string (rstrip (text, NULL), "\r  \t\nLeading And Trailing Whitespace");
	strncpy (text, "\r  \t\nLeading And Trailing Whitespace\n  \r\n\t", MAX_LENGTH);
	last = NULL;
	rstrip (text, &last);
	succeed_if_same_string (last, "e");

	strncpy (text, "\r\t\nLeading And Trailing Whitespace\n  \r\n\t", MAX_LENGTH);
	last = text + 10;
	succeed_if_same_string (rstrip (text, &last), "\r\t\nLeading");
	succeed_if_same_string (last, "g");
}

static void test_strip ()
{
	printf ("Test strip\n");
	char text[MAX_LENGTH];

	strncpy (text, "", MAX_LENGTH);
	succeed_if_same_string (strip (text), "");

	strncpy (text, "\t \nLeading And Trailing Whitespace\n\tSecond Line\n ", MAX_LENGTH);
	succeed_if_same_string (strip (text), "Leading And Trailing Whitespace\n\tSecond Line");
}

int main (int argc, char ** argv)
{
	printf ("Utility Tests\n");
	printf ("=============\n\n");

	init (argc, argv);

	test_lskip ();
	test_rstrip ();
	test_strip ();

	printf ("\nResults: %d Test%s done — %d Error%s.\n", nbTest, nbTest == 1 ? "" : "s", nbError, nbError == 1 ? "" : "s");

	return nbError;
}
