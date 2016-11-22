/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "ccode.h"

#include <tests_internal.h>

CCodeData * get_data ()
{
	CCodeData * d = calloc (1, sizeof (CCodeData));
	d->escape = '\\';

	d->encode[' '] = 'w';
	d->encode['\n'] = 'n';
	d->encode['='] = 'e';
	d->encode[';'] = 's';
	d->encode['#'] = 'r';
	d->encode['\\'] = 'b';

	d->decode['w'] = ' ';
	d->decode['n'] = '\n';
	d->decode['e'] = '=';
	d->decode['s'] = ';';
	d->decode['r'] = '#';
	d->decode['b'] = '\\';
	return d;
}

const char encoded_string[] = "a\\wvalue\\nwith\\e\\s\\r\\wand\\w\\b\\witself";
const char other_encoded_string[] = "a%wvalue%nwith%e%s%r%wand%w%b%witself";
const char decoded_string[] = "a value\nwith=;# and \\ itself";

void test_encode ()
{
	printf ("test encode\n");

	CCodeData * d = get_data ();

	char buf[1000];
	d->buf = buf;
	Key * test = keyNew ("user/test", KEY_VALUE, decoded_string, KEY_END);
	elektraCcodeEncode (test, d);
	succeed_if (!memcmp (keyValue (test), encoded_string, sizeof (encoded_string) - 1), "string not correctly encoded");

	elektraFree (d);
	keyDel (test);
}

void test_decode ()
{
	printf ("test decode\n");

	CCodeData * d = get_data ();

	char buf[1000];
	d->buf = buf;
	Key * test = keyNew ("user/test", KEY_SIZE, sizeof (encoded_string) - 1, KEY_VALUE, encoded_string, KEY_END);
	elektraCcodeDecode (test, d);
	succeed_if (!memcmp (keyValue (test), decoded_string, sizeof (decoded_string) - 1), "string not correctly encoded");

	elektraFree (d);
	keyDel (test);
}

void check_reversibility (const char * msg)
{
	CCodeData * d = get_data ();
	char buf[1000];
	d->buf = buf;
	Key * decode = keyNew ("user/test", KEY_VALUE, msg, KEY_END);

	Key * encode = keyDup (decode);
	elektraCcodeEncode (encode, d);

	elektraCcodeDecode (encode, d);
	compare_key (encode, decode);

	elektraFree (d);
	keyDel (decode);
	keyDel (encode);
}

void test_reversibility ()
{
	printf ("test reversibility\n");

	check_reversibility ("hello world");
	check_reversibility ("hello world!\nnew line");
	check_reversibility ("\0");
	check_reversibility ("\n");
	check_reversibility ("\\");
	check_reversibility (" ");
	check_reversibility ("=");
	check_reversibility (";");
	check_reversibility ("#");
	check_reversibility (" =;#");
	check_reversibility ("\n\\");
}

void test_decodeescape ()
{
	printf ("test decode escape\n");

	CCodeData * d = get_data ();
	d->encode['\\'] = '\\';
	d->decode['\\'] = '\\';

	char buf[1000];
	d->buf = buf;
	Key * test = keyNew ("user/test", KEY_SIZE, 2, KEY_VALUE, "\\\\", KEY_END);
	elektraCcodeDecode (test, d);
	succeed_if (!memcmp (keyValue (test), "\\", 2), "string not correctly encoded");

	elektraFree (d);
	keyDel (test);
}

void test_config ()
{
	printf ("test with config\n");

	KeySet * config = ksNew (20, keyNew ("user/chars", KEY_END), keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
				 keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END),					 // space -> w
				 keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END),					 // # -> r
				 keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
				 keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
				 keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
				 KS_END);

	KeySet * returned = ksNew (20, keyNew ("user/something", KEY_VALUE, decoded_string, KEY_END), KS_END);

	Plugin * p = calloc (1, sizeof (Plugin));
	p->config = config;

	elektraCcodeOpen (p, 0);

	elektraCcodeSet (p, returned, 0);

	Key * test = ksLookupByName (returned, "user/something", 0);
	succeed_if (!memcmp (keyValue (test), encoded_string, sizeof (encoded_string) - 1), "string not correctly encoded");

	elektraCcodeClose (p, 0);

	ksDel (returned);
	ksDel (p->config);
	elektraFree (p);
}

void test_otherescape ()
{
	printf ("test with config with other escape\n");

	KeySet * config = ksNew (20, keyNew ("user/chars", KEY_END), keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
				 keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END),					 // space -> w
				 keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END),					 // # -> r
				 keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
				 keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
				 keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
				 keyNew ("user/escape", KEY_VALUE, "25", KEY_END),   // use % as escape character
				 KS_END);

	KeySet * returned = ksNew (20, keyNew ("user/something", KEY_VALUE, decoded_string, KEY_END), KS_END);

	Plugin * p = calloc (1, sizeof (Plugin));
	p->config = config;

	elektraCcodeOpen (p, 0);

	elektraCcodeSet (p, returned, 0);

	Key * test = ksLookupByName (returned, "user/something", 0);
	succeed_if (!memcmp (keyValue (test), other_encoded_string, sizeof (other_encoded_string) - 1), "string not correctly encoded");

	elektraCcodeClose (p, 0);

	ksDel (returned);
	ksDel (p->config);
	elektraFree (p);
}


int main (int argc, char ** argv)
{
	printf ("   ICONV   TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_encode ();
	test_decode ();
	test_reversibility ();
	test_decodeescape ();
	test_config ();
	test_otherescape ();

	printf ("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
