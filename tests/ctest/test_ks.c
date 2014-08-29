#include <tests_internal.h>

ssize_t ksCopyInternal(KeySet *ks, size_t to, size_t from);

static KeySet * set_a ()
{
	return ksNew(16,
		keyNew ("user/0", KEY_END),
		keyNew ("user/a", KEY_END),
		keyNew ("user/a/a", KEY_END),
		keyNew ("user/a/a/a", KEY_END),
		keyNew ("user/a/a/b", KEY_END),
		keyNew ("user/a/b", KEY_END),
		keyNew ("user/a/b/a", KEY_END),
		keyNew ("user/a/b/b", KEY_END),
		keyNew ("user/a/c", KEY_END),
		keyNew ("user/a/d", KEY_END),
		keyNew ("user/a/x/a", KEY_END),
		keyNew ("user/a/x/b", KEY_END),
		keyNew ("user/a/x/c", KEY_END),
		keyNew ("user/a/x/c/a", KEY_END),
		keyNew ("user/a/x/c/b", KEY_END),
		keyNew ("user/x", KEY_END),
		KS_END);
}

static void test_copy()
{
	printf ("Testing operation copy (internal)\n");

	KeySet *copy[17][17];
#include "data_copy.c"

	KeySet *current;

	for (int i=0; i<17; ++i)
	{
		for (int j=0; j<17; ++j)
		{
			/* There are some cases which contain duplicates, we have to jump these...*/
			if (i>j) goto cleanup;
			if (i==0 && j==16) goto cleanup;

			current = set_a();
			/* Some blocks are lost in the next operation */
			succeed_if (ksCopyInternal (current, i, j) != -1, "ksCopyInternal failed");
			compare_keyset(current, copy[i][j]);
			ksDel (current);

cleanup:
			ksDel (copy[i][j]);
		}
	}
}

int main()
{
	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

