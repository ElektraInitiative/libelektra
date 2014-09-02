static void test_kdbopen()
{
	printf ("Test mounting modules\n");

	Key *errorKey = keyNew("", KEY_END);
	KDB *kdb = kdbOpen (errorKey);

	output_trie (kdb->trie);
	output_split (kdb->split);

	kdbClose (kdb, errorKey);

	succeed_if(output_warnings (errorKey), "warnings found");
	succeed_if(output_error (errorKey), "error found");

	keyDel (errorKey);
}

int main(int argc, char** argv)
{
	printf("KDB        TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_kdbopen();

	printf("\ntest_kdb RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

