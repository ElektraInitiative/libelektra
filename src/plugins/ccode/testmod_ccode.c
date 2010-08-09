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

#include "ccode.h"

#include <tests.h>

const char *encoded_string = "a\\wvalue\\nwith\\e\\s\\r\\wand\\w\\b\\witself\\0";
const char *decoded_string = "a value\nwith=;# and \\ itself";

void test_encode()
{
	printf ("test encode\n");

	char buf[1000];
	Key *test = keyNew ("user/test",
			KEY_VALUE, decoded_string,
			KEY_END);
	elektraCcodeEncode (test, buf);
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
	elektraCcodeDecode (test, buf);
	succeed_if (!strcmp(keyString(test), decoded_string), "string not correctly encoded");

	keyDel (test);
}

void check_reversibility(const char* msg)
{
	char buf[1000];
	Key *decode = keyNew ("user/test",
			KEY_VALUE, msg,
			KEY_END);

	Key *encode = keyDup (decode);
	elektraCcodeEncode (encode, buf);

	elektraCcodeDecode (encode, buf);
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


int main(int argc, char** argv)
{
	printf("   ICONV   TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_encode();
	test_decode();
	test_reversibility();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

