/**
 * @file
 *
 * @brief Test suite for Libease functions accessing key name data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdbutility.h>

#include "tests.h"

#define MAX_LENGTH 100

static void test_elektraLskip (void)
{
	printf ("Test elektraLskip\n");

	succeed_if_same_string (elektraLskip (""), "");
	succeed_if_same_string (elektraLskip ("No Leading Whitespace"), "No Leading Whitespace");
	succeed_if_same_string (elektraLskip ("\tLeading Tab"), "Leading Tab");
	succeed_if_same_string (elektraLskip (" Leading Space"), "Leading Space");
	succeed_if_same_string (elektraLskip (" \tLeading And Trailing Whitespace\t\n "), "Leading And Trailing Whitespace\t\n ");
}

static void test_elektraRstrip (void)
{
	printf ("Test elektraRstrip\n");

	char text[MAX_LENGTH];
	char * last = NULL;

	strncpy (text, "", MAX_LENGTH);
	succeed_if_same_string (elektraRstrip (text, NULL), "");
	elektraRstrip (text, &last);
	succeed_if_same_string (last, text);

	strncpy (text, "No Trailing Whitespace", MAX_LENGTH);
	succeed_if_same_string (elektraRstrip (text, NULL), "No Trailing Whitespace");
	last = NULL;
	elektraRstrip (text, &last);
	succeed_if_same_string (last, "e");

	strncpy (text, "\t\nLeading Whitespace", MAX_LENGTH);
	succeed_if_same_string (elektraRstrip (text, NULL), "\t\nLeading Whitespace");
	last = NULL;
	elektraRstrip (text, &last);
	succeed_if_same_string (last, "e");

	strncpy (text, "Trailing Tab\t", MAX_LENGTH);
	succeed_if_same_string (elektraRstrip (text, NULL), "Trailing Tab");
	strncpy (text, "Trailing Tab\t", MAX_LENGTH);
	last = NULL;
	elektraRstrip (text, &last);
	succeed_if_same_string (last, "b");

	strncpy (text, "Trailing Whitespace\n\r\t  ", MAX_LENGTH);
	succeed_if_same_string (elektraRstrip (text, NULL), "Trailing Whitespace");
	strncpy (text, "Trailing Whitespace\n\r\t  ", MAX_LENGTH);
	last = NULL;
	elektraRstrip (text, &last);
	succeed_if_same_string (last, "e");

	strncpy (text, "\r  \t\nLeading And Trailing Whitespace\n  \r\n\t", MAX_LENGTH);
	succeed_if_same_string (elektraRstrip (text, NULL), "\r  \t\nLeading And Trailing Whitespace");
	strncpy (text, "\r  \t\nLeading And Trailing Whitespace\n  \r\n\t", MAX_LENGTH);
	last = NULL;
	elektraRstrip (text, &last);
	succeed_if_same_string (last, "e");

	strncpy (text, "\r\t\nLeading And Trailing Whitespace\n  \r\n\t", MAX_LENGTH);
	last = text + 10;
	succeed_if_same_string (elektraRstrip (text, &last), "\r\t\nLeading");
	succeed_if_same_string (last, "g");
}

static void test_elektraStrip (void)
{
	printf ("Test elektraStrip\n");
	char text[MAX_LENGTH];

	strncpy (text, "", MAX_LENGTH);
	succeed_if_same_string (elektraStrip (text), "");

	strncpy (text, "\t \nLeading And Trailing Whitespace\n\tSecond Line\n ", MAX_LENGTH);
	succeed_if_same_string (elektraStrip (text), "Leading And Trailing Whitespace\n\tSecond Line");
}


int main (int argc, char ** argv)
{
	printf ("Utility Tests\n");
	printf ("=============\n\n");

	init (argc, argv);

	test_elektraLskip ();
	test_elektraRstrip ();
	test_elektraStrip ();

	printf ("\nResults: %d Test%s done — %d Error%s.\n", nbTest, nbTest == 1 ? "" : "s", nbError, nbError == 1 ? "" : "s");

	return nbError;
}
