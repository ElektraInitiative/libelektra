/***************************************************************************
 *           testmod_ni.c  - Test suite for nickel ini parser
 ****************************************************************************/

/******************************************************************************
 * Nickel - a library for hierarchical maps and .ini files
 * Part of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: ni.c 349 2008-01-19 18:18:22Z chaz $
 ******************************************************************************/

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <bohr/ni.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#undef NDEBUG
#include <assert.h>
#include <tests_plugin.h>

#define BEGIN_TEST(x)                                                                                                                      \
	void x (void)                                                                                                                      \
	{                                                                                                                                  \
		int test_fail = 0;

#define TEST_COND(cond)                                                                                                                    \
	if (!(cond) && (test_fail = 1)) printf ("%s: %s: '%s' FAILED (%s:%d)\n", argv0, __func__, #cond, __FILE__, __LINE__)

#define END_TEST()                                                                                                                         \
	printf ("%s: %s: %s\n", argv0, __func__, (test_fail ? "FAIL" : "pass"));                                                           \
	if (test_fail) any_fail = 1;                                                                                                       \
	}
#define TEST(x) x ()


char * argv0 = NULL;
int any_fail = 0;


BEGIN_TEST (ver)
uint32_t lib_version = elektraNi_GetVersion ();
TEST_COND (lib_version == elektraNi_VERSION);
END_TEST ()

BEGIN_TEST (new)
elektraNi_node node = elektraNi_New ();
assert (node != NULL);

const char * name = elektraNi_GetName (node, NULL);
TEST_COND (name == NULL);

elektraNi_node root = elektraNi_GetRoot (node);
TEST_COND (root == node);

elektraNi_node parent = elektraNi_GetParent (node);
TEST_COND (parent == NULL);

int children = elektraNi_GetNumChildren (node);
TEST_COND (children == 0);

int modified = elektraNi_GetModified (node);
TEST_COND (modified == 0);

const char * value = elektraNi_GetValue (node, NULL);
TEST_COND (value == NULL);

int error = elektraNi_SetValue (node, "", 0);
TEST_COND (error < 0);

elektraNi_Free (node);
END_TEST ()

BEGIN_TEST (tree)
elektraNi_node node = elektraNi_New ();
assert (node != NULL);

elektraNi_node child = elektraNi_GetChild (node, "a", -1, 1, NULL);
assert (child != NULL);

int children = elektraNi_GetNumChildren (node);
TEST_COND (children == 1);

elektraNi_node child2 = elektraNi_GetChild (child, "b", -1, 1, NULL);
assert (child2 != NULL);

int children2 = elektraNi_GetNumChildren (child);
TEST_COND (children2 == 1);

elektraNi_node child3 = elektraNi_GetChild (node, "a", -1, 1, NULL);
TEST_COND (child3 == child);

elektraNi_node child4 = elektraNi_GetChild (child, "b", -1, 1, NULL);
TEST_COND (child4 == child2);

elektraNi_Free (node);
END_TEST ()

BEGIN_TEST (test_values)
elektraNi_node node = elektraNi_New ();
assert (node != NULL);

elektraNi_node child = elektraNi_GetChild (node, "", 0, 1, NULL);
assert (child != NULL);

int len = elektraNi_SetValue (child, "1", 1);
assert (len >= 0);
TEST_COND (len == 1);

len = 0;
const char * value = elektraNi_GetValue (child, &len);
TEST_COND (!strcmp (value, "1"));
TEST_COND (len == 1);

long ivalue = elektraNi_GetValueInt (child);
TEST_COND (ivalue == 1);

double fvalue = elektraNi_GetValueFloat (child);
TEST_COND (fvalue <= 1.01);
TEST_COND (fvalue >= 0.99);

int bvalue = elektraNi_GetValueBool (child);
TEST_COND (bvalue != 0);

int scanned_ivalue;
len = elektraNi_ValueScan (child, "%i", &scanned_ivalue);
TEST_COND (len == 1);
TEST_COND (scanned_ivalue == 1);

len = elektraNi_ValuePrint (child, "%.3f", 3.333);
assert (len >= 0);
TEST_COND (len == 5);

value = elektraNi_GetValue (child, &len);
TEST_COND (!strcmp (value, "3.333"));
TEST_COND (len == 5);

ivalue = elektraNi_GetValueInt (child);
TEST_COND (ivalue == 3);

fvalue = elektraNi_GetValueFloat (child);
TEST_COND (fvalue >= 3.332);
TEST_COND (fvalue <= 3.334);

bvalue = elektraNi_GetValueBool (child);
TEST_COND (bvalue != 0);

float scanned_fvalue;
len = elektraNi_ValueScan (child, "%f", &scanned_fvalue);
TEST_COND (len == 1);
TEST_COND (scanned_fvalue >= 3.332f);
TEST_COND (scanned_fvalue <= 3.334f);

len = elektraNi_SetValueInt (child, 23);
assert (len >= 0);
TEST_COND (len == 2);

len = 0;
value = elektraNi_GetValue (child, &len);
TEST_COND (!strcmp (value, "23"));
TEST_COND (len == 2);

len = elektraNi_SetValueFloat (child, 4.5);
assert (len >= 0);
TEST_COND (len == 3);

len = 0;
value = elektraNi_GetValue (child, &len);
TEST_COND (!strcmp (value, "4.5"));
TEST_COND (len == 3);

len = elektraNi_SetValueBool (child, 1);
assert (len >= 0);
TEST_COND (len == 4);

len = 0;
value = elektraNi_GetValue (child, &len);
TEST_COND (!strcmp (value, "true"));
TEST_COND (len == 4);

len = elektraNi_ValuePrint (child, "%s", "WHOAH!");
assert (len >= 0);
TEST_COND (len == 6);

char sval[7];
len = elektraNi_ValueScan (child, "%6s", sval);
TEST_COND (len == 1);
TEST_COND (!strcmp (sval, "WHOAH!"));

elektraNi_Free (node);
END_TEST ()

BEGIN_TEST (parse_spaces_quotes)

elektraNi_node node = elektraNi_New ();
assert (node != NULL);

elektraNi_Free (node);
END_TEST ()

BEGIN_TEST (output)
char desired[] = {
	";Ni1\n"
	"; Generated by the ni plugin using Elektra (see libelektra.org).\n\n"
	"1 = 1's value\n\n"
	"[1]\n\n"
	" [[2]]\n"
	"  3 = 3's value\n"
};

elektraNi_node node = elektraNi_New ();
assert (node != NULL);

elektraNi_node child = elektraNi_GetChild (node, "1", -1, 1, NULL);
assert (child != NULL);

int len = elektraNi_SetValue (child, "1's value", -1);
assert (len >= 0);

elektraNi_node child2 = elektraNi_GetChild (child, "2", -1, 1, NULL);
assert (child2 != NULL);

elektraNi_node child3 = elektraNi_GetChild (child2, "3", -1, 1, NULL);
assert (child3 != NULL);

len = elektraNi_SetValue (child3, "3's value", -1);
assert (len >= 0);

elektraNi_node child4 = elektraNi_GetChild (node, "back to 1", -1, 1, NULL);
assert (child4 != NULL);

FILE * temp = tmpfile ();
assert (temp != NULL);

int error = elektraNi_WriteStream (node, temp, 0);
assert (error != 0);

rewind (temp);

char buf[1024];
len = fread (buf, sizeof (char), 1023, temp);
assert (len <= 1023);
buf[len] = '\0';

TEST_COND (len * sizeof (char) == sizeof (desired) - sizeof (char) && !strcmp (buf, desired));

fclose (temp);
elektraNi_Free (node);
END_TEST ()

BEGIN_TEST (output_modified)
char desired[] = {
	";Ni1\n"
	"; Generated by the ni plugin using Elektra (see libelektra.org).\n\n"
	"3 = 3's value\n"
};

elektraNi_node node = elektraNi_New ();
assert (node != NULL);

elektraNi_node child = elektraNi_GetChild (node, "1", -1, 1, NULL);
assert (child != NULL);

int len = elektraNi_SetValue (child, "1's value", -1);
assert (len >= 0);

elektraNi_node child2 = elektraNi_GetChild (node, "2", -1, 1, NULL);
assert (child2 != NULL);

elektraNi_node child3 = elektraNi_GetChild (node, "3", -1, 1, NULL);
assert (child3 != NULL);

len = elektraNi_SetValue (child3, "3's value", -1);
assert (len >= 0);

elektraNi_node child4 = elektraNi_GetChild (node, "4", -1, 1, NULL);
assert (child4 != NULL);

elektraNi_SetModified (node, 0, 1);
elektraNi_SetModified (child3, 1, 0);
elektraNi_SetModified (child4, 1, 0);

FILE * temp = tmpfile ();
assert (temp != NULL);

int error = elektraNi_WriteStream (node, temp, 1);
assert (error != 0);

rewind (temp);

char buf[1024];
len = fread (buf, sizeof (char), 1023, temp);
assert (len <= 1023);
buf[len] = '\0';

TEST_COND (len * sizeof (char) == sizeof (desired) - sizeof (char) && !strcmp (buf, desired));

fclose (temp);
elektraNi_Free (node);
END_TEST ()

BEGIN_TEST (parse_output)
// clang-format off
char * names[] = {
	("loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooon\xc2"
	"\xa9"),
	"\xc2\xa9valid UTF-8",
	"valid \xc2\xa9UTF-8",
	"valid UTF-8\xc2\xa9",
	"\xc3invalid UTF-8",
	"invalid \xc3UTF-8",
	"invalid UTF-8\xc3",
	";asdf",
	"\\asdf",
	"[asdf",
	"]asdf",
	"=asdf",
	"\"asdf",
	"as;df",
	"as\\df",
	"as[df",
	"as]df",
	"as=df",
	"as\"df",
	" ;asdf",
	"\\asdf ",
	" [asdf",
	"]asdf ",
	" =asdf",
	"\"asdf ",
	" as;df",
	"as\\df ",
	" as[df",
	"as]df ",
	" as=df",
	"as\"df ",
	"\a\b\f\n\r\t\v"
};
char * values[] = { "truncated",
		    "\xc2\xa9valid UTF-8",
		    "valid \xc2\xa9UTF-8",
		    "valid UTF-8\xc2\xa9",
		    "\xc3invalid UTF-8",
		    "invalid \xc3UTF-8",
		    "invalid UTF-8\xc3",
		    ";asdf",
		    "\\asdf",
		    "[asdf",
		    "]asdf",
		    "=asdf",
		    "\"asdf",
		    "as;df",
		    "as\\df",
		    "as[df",
		    "as]df",
		    "as=df",
		    "as\"df",
		    " ;asdf",
		    "\\asdf ",
		    " [asdf",
		    "]asdf ",
		    " =asdf",
		    "\"asdf ",
		    " as;df",
		    "as\\df ",
		    " as[df",
		    "as]df ",
		    " as=df",
		    "as\"df ",
		    "\a\b\f\n\r\t\v" };
// clang-format on
#define NUM_parse_output_NODES (sizeof (names) / sizeof (names[0]))

const char * stored_name0 =
	("loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooon"
	 "\xc2");

assert (sizeof (values) / sizeof (values[0]) == NUM_parse_output_NODES);

elektraNi_node node = elektraNi_New ();
assert (node != NULL);

size_t i;
for (i = 0; i < NUM_parse_output_NODES; ++i)
{
	elektraNi_node child = elektraNi_GetChild (node, names[i], -1, 1, NULL);
	assert (child != NULL);

	int len = elektraNi_SetValue (child, values[i], -1);
	assert (len >= 0);
}

// FILE * temp = fopen("temp1.ini", "w+");
FILE * temp = tmpfile ();
assert (temp != NULL);

int error = elektraNi_WriteStream (node, temp, 0);
assert (error != 0);

rewind (temp);
elektraNi_Free (node);

node = elektraNi_New ();
assert (node != NULL);

error = elektraNi_ReadStream (node, temp, 0);
assert (error != 0);

int children = elektraNi_GetNumChildren (node);
TEST_COND (children == NUM_parse_output_NODES);

for (i = 0; i < NUM_parse_output_NODES; ++i)
{
	elektraNi_node child = elektraNi_GetChild (node, names[i], -1, 0, NULL);
	TEST_COND (child != NULL);

	const char * name = elektraNi_GetName (child, NULL);
	TEST_COND (name && !strcmp (name, (i == 0 ? stored_name0 : names[i])));

	const char * value = elektraNi_GetValue (child, NULL);
	TEST_COND (value && !strcmp (value, values[i]));
}

// elektraNi_WriteFile(node, "temp2.ini", 0);
fclose (temp);
elektraNi_Free (node);
#undef NUM_parse_output_NODES
END_TEST ()

int main (int argc, char ** argv)
{
	if (argc >= 1)
	{
		argv0 = argv[0];
	}
	printf ("NICKEL TESTS\n");
	printf ("==================\n\n");

	TEST (ver);
	TEST (new);
	TEST (tree);
	TEST (test_values);
	TEST (parse_spaces_quotes);
	TEST (output);
	TEST (output_modified);
	TEST (parse_output);

	print_result ("testmod_ni");
	return any_fail;
}
