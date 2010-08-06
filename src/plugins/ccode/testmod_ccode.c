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

void test_encode()
{
	char buf[1000];
	Key *test = keyNew ("user/test",
			KEY_VALUE, "a value\nwith=;# and \\ itself",
			KEY_END);
	elektraCcodeEncode (test, buf);
	printf ("%s", keyString(test));
}


int main(int argc, char** argv)
{
	printf("   ICONV   TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_encode();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

