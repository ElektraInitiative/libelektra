#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>

#include "glob.h"

#include <tests.h>

#define NR_KEYS 1

void test_match()
{
	int ret = fnmatch ("user/*/to/key", "user/path/to/key",
			FNM_PATHNAME);
	printf ("%d", ret);
}


int main(int argc, char** argv)
{
	printf("   ICONV   TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_match();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

