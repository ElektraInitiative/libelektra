#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "hexcode.h"

#include <tests.h>

const char encoded_string[] = "a\\20value\\20with\\3D\\3B\\23\\20and\\20\\5C\\20itself";
const char decoded_string[] = "a value with=;# and \\ itself";

void test_encode()
{
	printf ("test encode\n");

	char buf[1000];
	char hd[256] = {0};
	hd['\0'] = 1;
	hd['\n'] = 1;
	hd['\\'] = 1;
	hd[' '] = 1;
	hd['='] = 1;
	hd[';'] = 1;
	hd['#'] = 1;
	Key *test = keyNew ("user/test",
			KEY_VALUE, decoded_string,
			KEY_END);
	elektraHexcodeEncode (test, buf, hd);
	succeed_if (!memcmp(keyValue(test), encoded_string, sizeof(encoded_string)-1), "string not correctly encoded");

	keyDel (test);
}

void test_decode()
{
	printf ("test decode\n");

	char buf[1000];
	Key *test = keyNew ("user/test",
			KEY_SIZE, sizeof(encoded_string)-1,
			KEY_VALUE, encoded_string,
			KEY_END);
	elektraHexcodeDecode (test, buf);
	succeed_if (!strcmp(keyString(test), decoded_string), "string not correctly encoded");

	keyDel (test);
}

void check_reversibility(const char* msg)
{
	char buf[1000];
	Key *decode = keyNew ("user/test",
			KEY_VALUE, msg,
			KEY_END);

	char hd[256] = {0};
	hd['\0'] = 1;
	hd['\n'] = 1;
	hd['\\'] = 1;
	hd[' '] = 1;
	hd['='] = 1;
	hd[';'] = 1;
	hd['#'] = 1;
	Key *encode = keyDup (decode);
	elektraHexcodeEncode (encode, buf, hd);

	elektraHexcodeDecode (encode, buf);
	succeed_if (compare_key(encode, decode) == 0, "was not reversible");

	keyDel (decode);
	keyDel (encode);
}

void test_reversibility()
{
	printf ("test reversibility\n");

	check_reversibility("hello world");
	check_reversibility("hello world!\nnew line");
	check_reversibility("\0");
	check_reversibility("\n");
	check_reversibility("\\");
	check_reversibility(" ");
	check_reversibility("=");
	check_reversibility(";");
	check_reversibility("#");
	check_reversibility(" =;#");
	check_reversibility("\n\\");
}

void test_config()
{
	KeySet *config = ksNew (20,
		keyNew ("user/chars", KEY_END),
		keyNew ("user/chars/20", KEY_END),
		keyNew ("user/chars/23", KEY_END),
		keyNew ("user/chars/5C", KEY_END),
		keyNew ("user/chars/3D", KEY_END),
		keyNew ("user/chars/3B", KEY_END),
		KS_END);

	KeySet *returned = ksNew (20,
		keyNew ("user/something", KEY_VALUE, decoded_string, KEY_END),
		KS_END);

	Plugin *p = calloc(1, sizeof(Plugin));
	p->config = config;

	elektraHexcodeSet(p, returned, 0);

	Key * test = ksLookupByName (returned, "user/something", 0);
	succeed_if (!memcmp(keyValue(test), encoded_string, sizeof(encoded_string)-1), "string not correctly encoded");

	elektraHexcodeClose(p, 0);

	ksDel (returned);
	ksDel (p->config);
	free (p);
}


int main(int argc, char** argv)
{
	printf("   ICONV   TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_encode();
	test_decode();
	test_reversibility();
	test_config();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

