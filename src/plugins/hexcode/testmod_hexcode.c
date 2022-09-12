/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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

#include "hexcode.h"

#include <tests_internal.h>

const char encoded_string[] = "a\\20value\\20with\\3D\\3B\\23\\20and\\20\\5C\\20itself";
const char decoded_string[] = "a value with=;# and \\ itself";

void test_encode (void)
{
	printf ("test encode\n");

	CHexData * hd = calloc (1, sizeof (CHexData));
	hd->hd['\0'] = 1;
	hd->hd['\n'] = 1;
	hd->hd['\\'] = 1;
	hd->hd[' '] = 1;
	hd->hd['='] = 1;
	hd->hd[';'] = 1;
	hd->hd['#'] = 1;
	hd->escape = '\\';

	char buf[1000];
	hd->buf = buf;

	ElektraKey * test = keyNew ("user:/test", ELEKTRA_KEY_VALUE, decoded_string, ELEKTRA_KEY_END);
	elektraHexcodeEncode (test, hd);
	succeed_if (!memcmp (keyValue (test), encoded_string, sizeof (encoded_string) - 1), "string not correctly encoded");

	elektraFree (hd);
	keyDel (test);
}

void test_decode (void)
{
	printf ("test decode\n");

	CHexData * hd = calloc (1, sizeof (CHexData));
	hd->escape = '\\';

	char buf[1000];
	hd->buf = buf;

	ElektraKey * test = keyNew ("user:/test", ELEKTRA_KEY_SIZE, sizeof (encoded_string) - 1, ELEKTRA_KEY_VALUE, encoded_string, ELEKTRA_KEY_END);
	elektraHexcodeDecode (test, hd);
	succeed_if (!strcmp (keyString (test), decoded_string), "string not correctly encoded");

	elektraFree (hd);
	keyDel (test);
}

void check_reversibility (const char * msg)
{
	ElektraKey * decode = keyNew ("user:/test", ELEKTRA_KEY_VALUE, msg, ELEKTRA_KEY_END);

	CHexData * hd = calloc (1, sizeof (CHexData));
	hd->hd['\0'] = 1;
	hd->hd['\n'] = 1;
	hd->hd['\\'] = 1;
	hd->hd[' '] = 1;
	hd->hd['='] = 1;
	hd->hd[';'] = 1;
	hd->hd['#'] = 1;
	hd->escape = '\\';

	char buf[1000];
	hd->buf = buf;

	ElektraKey * encode = keyDup (decode, ELEKTRA_KEY_CP_ALL);
	elektraHexcodeEncode (encode, hd);

	elektraHexcodeDecode (encode, hd);
	compare_key (encode, decode);

	elektraFree (hd);
	keyDel (decode);
	keyDel (encode);
}

void test_reversibility (void)
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

void test_config (void)
{
	ElektraKeyset * config =
		ksNew (20, keyNew ("user:/chars", ELEKTRA_KEY_END), keyNew ("user:/chars/20", ELEKTRA_KEY_END), keyNew ("user:/chars/23", ELEKTRA_KEY_END),
		       keyNew ("user:/chars/5C", ELEKTRA_KEY_END), keyNew ("user:/chars/3D", ELEKTRA_KEY_END), keyNew ("user:/chars/3B", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * returned = ksNew (20, keyNew ("user:/something", ELEKTRA_KEY_VALUE, decoded_string, ELEKTRA_KEY_END), ELEKTRA_KS_END);

	Plugin * p = calloc (1, sizeof (Plugin));
	p->config = config;

	elektraHexcodeOpen (p, 0);

	elektraHexcodeSet (p, returned, 0);

	ElektraKey * test = ksLookupByName (returned, "user:/something", 0);
	succeed_if (!memcmp (keyValue (test), encoded_string, sizeof (encoded_string) - 1), "string not correctly encoded");

	elektraHexcodeClose (p, 0);

	ksDel (returned);
	ksDel (p->config);
	elektraFree (p);
}


int main (int argc, char ** argv)
{
	printf ("HEXCODE   TESTS\n");
	printf ("===============\n\n");

	init (argc, argv);

	test_encode ();
	test_decode ();
	test_reversibility ();
	test_config ();

	print_result ("testmod_hexcode");

	return nbError;
}
