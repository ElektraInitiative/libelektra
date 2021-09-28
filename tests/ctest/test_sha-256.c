/**
 * @file
 *
 * @brief Tests for src/libs/ease/sha-256.c.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "../../src/libs/ease/hash.c"
#include "../../src/libs/ease/sha-256.c"
#include <tests_internal.h>

struct string_vector
{
	const char * input;
	const char * output;
};

static const struct string_vector STRING_VECTORS[] = { { "", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" },
						       { "abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" },
						       { "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
							 "a8ae6e6ee929abea3afcfc5258c8ccd6f85273e0d4626d26c7279f3250f77c8e" },
						       { "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde",
							 "057ee79ece0b9a849552ab8d3c335fe9a5f1c46ef5f1d9b190c295728628299c" },
						       { "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0",
							 "2a6ad82f3620d3ebe9d678c812ae12312699d673240d5be8fac0910a70000d93" },
						       { "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
							 "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1" },
						       { "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
							 "ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu",
							 "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1" } };

static void string_test (const char input[], const char output[])
{
	uint8_t hash[32];
	char hash_string[65];
	calc_sha_256 (hash, input, strlen (input));
	hash_to_string (hash_string, hash);
	succeed_if_fmt (strcmp (hash_string, output) == 0, "calculated sha-256 hash %s did not match expected result %s", hash_string,
			output);
}

/**
 * Test basic test cases using test vectors from https://github.com/amosnier/sha-2#testing
 * (adapted from https://github.com/amosnier/sha-2/blob/f0d7baf076207b943649e68946049059f018c10b/test.c)
 */
static void test_testVectors (void)
{
	for (size_t i = 0; i < (sizeof STRING_VECTORS / sizeof (struct string_vector)); i++)
	{
		const struct string_vector * vector = &STRING_VECTORS[i];
		string_test (vector->input, vector->output);
	}
}

/**
 * Test whether streaming API is aware of character order.
 * Expected results calculated via GNU coreutils' sha256sum (GNU coreutils) 8.30.
 */
static void test_streaming_aware_of_order (void)
{
	// Check if streaming API is aware of character order.
	// Input holds the same characters, but in different order.
	// Output must be different.
	const struct string_vector vector1 = { "key = value", "f7c496f5894d24805efd2e40dc78bd31f57c07c900320c6426b4ddaa5da31d25" };
	const struct string_vector vector2 = { "keyvalue = ", "610ed9b80c58e2585d0ad426b4662c4d187c36dd64f093f17e99fc0b089630c2" };
	string_test (vector1.input, vector1.output);
	string_test (vector2.input, vector2.output);
}

int main (int argc, char ** argv)
{
	printf ("SHA-256    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_testVectors ();
	test_streaming_aware_of_order ();

	printf ("\ntest_sha-256 RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
