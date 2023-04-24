/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "tests.h"
#include <elektra/type/conversion.h>

#define TEST_TO_STRING(Type, value, expect)                                                                                                \
	do                                                                                                                                 \
	{                                                                                                                                  \
		const char * expected = (expect);                                                                                          \
		char * actual = elektra##Type##ToString ((value));                                                                         \
		succeed_if_same_string (actual, expected);                                                                                 \
		elektraFree (actual);                                                                                                      \
	} while (0)

#define TEST_FROM_KEY(Type, ctype, val, expect)                                                                                            \
	do                                                                                                                                 \
	{                                                                                                                                  \
		const char * value = (val);                                                                                                \
		Key * k = keyNew ("user:/test", KEY_VALUE, value, KEY_END);                                                                \
		ctype expected = (expect);                                                                                                 \
		ctype var;                                                                                                                 \
		succeed_if (elektraKeyTo##Type (k, &var) == 1, val " couldn't be read");                                                   \
		succeed_if (var == expected, val " not read correctly");                                                                   \
		keyDel (k);                                                                                                                \
	} while (0)

#define TEST_ROUNDTRIP(Type, ctype, strval, val)                                                                                           \
	do                                                                                                                                 \
	{                                                                                                                                  \
		const char * string = (strval);                                                                                            \
		ctype value = (val);                                                                                                       \
		{                                                                                                                          \
			char * s = elektra##Type##ToString (value);                                                                        \
			Key * k = keyNew ("user:/test", KEY_VALUE, s, KEY_END);                                                            \
			ctype var;                                                                                                         \
			succeed_if (elektraKeyTo##Type (k, &var) == 1, strval " couldn't be read back (1)");                               \
			succeed_if (var == value, strval " not read back correctly");                                                      \
			elektraFree (s);                                                                                                   \
			keyDel (k);                                                                                                        \
		}                                                                                                                          \
                                                                                                                                           \
		{                                                                                                                          \
			Key * k = keyNew ("user:/test", KEY_VALUE, string, KEY_END);                                                       \
			ctype var;                                                                                                         \
			succeed_if (elektraKeyTo##Type (k, &var) == 1, strval " couldn't be read (1)");                                    \
			char * s = elektra##Type##ToString (var);                                                                          \
			succeed_if (strcmp (s, string) == 0, strval " not read correctly");                                                \
			elektraFree (s);                                                                                                   \
			keyDel (k);                                                                                                        \
		}                                                                                                                          \
	} while (0)

#define TEST_DOUBLE_ROUNDTRIP(Type, ctype, strval, val)                                                                                    \
	do                                                                                                                                 \
	{                                                                                                                                  \
		const char * string = (strval);                                                                                            \
		ctype value = (val);                                                                                                       \
		{                                                                                                                          \
			char * s = elektra##Type##ToString (value);                                                                        \
			Key * k = keyNew ("user:/test", KEY_VALUE, s, KEY_END);                                                            \
			ctype var;                                                                                                         \
			succeed_if (elektraKeyTo##Type (k, &var) == 1, strval " couldn't be read back (1)");                               \
			char * s1 = elektra##Type##ToString (var);                                                                         \
			Key * k1 = keyNew ("user:/test", KEY_VALUE, s1, KEY_END);                                                          \
			ctype var1;                                                                                                        \
			succeed_if (elektraKeyTo##Type (k1, &var1) == 1, strval " couldn't be read back (2)");                             \
			succeed_if (var1 == var, strval " not read back correctly");                                                       \
			elektraFree (s);                                                                                                   \
			elektraFree (s1);                                                                                                  \
			keyDel (k);                                                                                                        \
			keyDel (k1);                                                                                                       \
		}                                                                                                                          \
                                                                                                                                           \
		{                                                                                                                          \
			Key * k = keyNew ("user:/test", KEY_VALUE, string, KEY_END);                                                       \
			ctype var;                                                                                                         \
			succeed_if (elektraKeyTo##Type (k, &var) == 1, strval " couldn't be read (1)");                                    \
			char * s = elektra##Type##ToString (var);                                                                          \
			Key * k1 = keyNew ("user:/test", KEY_VALUE, s, KEY_END);                                                           \
			ctype var1;                                                                                                        \
			succeed_if (elektraKeyTo##Type (k, &var1) == 1, strval " couldn't be read (2)");                                   \
			char * s1 = elektra##Type##ToString (var1);                                                                        \
			succeed_if (strcmp (s1, s) == 0, strval " not read correctly");                                                    \
			elektraFree (s);                                                                                                   \
			elektraFree (s1);                                                                                                  \
			keyDel (k);                                                                                                        \
			keyDel (k1);                                                                                                       \
		}                                                                                                                          \
	} while (0)

static void test_to_string (void)
{
	TEST_TO_STRING (Boolean, true, "1");
	TEST_TO_STRING (Boolean, false, "0");
	TEST_TO_STRING (Boolean, 1, "1");
	TEST_TO_STRING (Boolean, 0, "0");

	for (int c = -128; c < 128; c++)
	{
		const char expect[2] = { (char) c, '\0' };
		TEST_TO_STRING (Char, (kdb_char_t) c, expect);
	}

	TEST_TO_STRING (Octet, 15, "15");
	TEST_TO_STRING (Octet, 0, "0");
	TEST_TO_STRING (Octet, 255, "255");

	TEST_TO_STRING (Short, 15, "15");
	TEST_TO_STRING (Short, 0, "0");
	TEST_TO_STRING (Short, 127, "127");
	TEST_TO_STRING (Short, -128, "-128");
	TEST_TO_STRING (Short, 32767, "32767");
	TEST_TO_STRING (Short, -32768, "-32768");

	TEST_TO_STRING (UnsignedShort, 15, "15");
	TEST_TO_STRING (UnsignedShort, 0, "0");
	TEST_TO_STRING (UnsignedShort, 255, "255");
	TEST_TO_STRING (UnsignedShort, 65535, "65535");

	TEST_TO_STRING (Long, 15, "15");
	TEST_TO_STRING (Long, 0, "0");
	TEST_TO_STRING (Long, 127, "127");
	TEST_TO_STRING (Long, -128, "-128");
	TEST_TO_STRING (Long, 32767, "32767");
	TEST_TO_STRING (Long, -32768, "-32768");
	TEST_TO_STRING (Long, 2147483647, "2147483647");
	TEST_TO_STRING (Long, -2147483648, "-2147483648");

	TEST_TO_STRING (UnsignedLong, 15, "15");
	TEST_TO_STRING (UnsignedLong, 0, "0");
	TEST_TO_STRING (UnsignedLong, 255, "255");
	TEST_TO_STRING (UnsignedLong, 65535, "65535");
	TEST_TO_STRING (UnsignedLong, 4294967295, "4294967295");

	TEST_TO_STRING (LongLong, 15, "15");
	TEST_TO_STRING (LongLong, 0, "0");
	TEST_TO_STRING (LongLong, 127, "127");
	TEST_TO_STRING (LongLong, -128, "-128");
	TEST_TO_STRING (LongLong, 32767, "32767");
	TEST_TO_STRING (LongLong, -32768, "-32768");
	TEST_TO_STRING (LongLong, 2147483647, "2147483647");
	TEST_TO_STRING (LongLong, -2147483648, "-2147483648");
	TEST_TO_STRING (LongLong, 9223372036854775807LL, "9223372036854775807");
	TEST_TO_STRING (LongLong, -9223372036854775807LL - 1LL, "-9223372036854775808");

	TEST_TO_STRING (UnsignedLongLong, 15, "15");
	TEST_TO_STRING (UnsignedLongLong, 0, "0");
	TEST_TO_STRING (UnsignedLongLong, 255, "255");
	TEST_TO_STRING (UnsignedLongLong, 65535, "65535");
	TEST_TO_STRING (UnsignedLongLong, 4294967295, "4294967295");
	TEST_TO_STRING (UnsignedLongLong, 18446744073709551615ULL, "18446744073709551615");

	TEST_TO_STRING (Float, 0, "0");
	TEST_TO_STRING (Float, 1, "1");
	TEST_TO_STRING (Float, 2.5, "2.5");
	TEST_TO_STRING (Float, -2.5, "-2.5");
	TEST_TO_STRING (Float, 1.10000002, "1.10000002");
	TEST_TO_STRING (Float, -1.10000002, "-1.10000002");

	TEST_TO_STRING (Double, 0, "0");
	TEST_TO_STRING (Double, 1, "1");
	TEST_TO_STRING (Double, 2.5, "2.5");
	TEST_TO_STRING (Double, -2.5, "-2.5");
	TEST_TO_STRING (Double, 1.1000000000000001, "1.1000000000000001");
	TEST_TO_STRING (Double, -1.1000000000000001, "-1.1000000000000001");

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	TEST_TO_STRING (LongDouble, 0, "0");
	TEST_TO_STRING (LongDouble, 1, "1");
	TEST_TO_STRING (LongDouble, 2.5, "2.5");
	TEST_TO_STRING (LongDouble, -2.5, "-2.5");
	TEST_TO_STRING (LongDouble, 1.10000000000000008882, "1.10000000000000008882");
	TEST_TO_STRING (LongDouble, -1.10000000000000008882, "-1.10000000000000008882");

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE
}

static void test_from_key (void)
{
	TEST_FROM_KEY (Boolean, kdb_boolean_t, "1", true);
	TEST_FROM_KEY (Boolean, kdb_boolean_t, "0", false);

	TEST_FROM_KEY (Char, kdb_char_t, "a", 'a');
	TEST_FROM_KEY (Char, kdb_char_t, "+", '+');
	TEST_FROM_KEY (Char, kdb_char_t, "\n", '\n');

	TEST_FROM_KEY (Octet, kdb_octet_t, "15", 15);
	TEST_FROM_KEY (Octet, kdb_octet_t, "0", 0);
	TEST_FROM_KEY (Octet, kdb_octet_t, "255", 255);

	TEST_FROM_KEY (Short, kdb_short_t, "15", 15);
	TEST_FROM_KEY (Short, kdb_short_t, "0", 0);
	TEST_FROM_KEY (Short, kdb_short_t, "127", 127);
	TEST_FROM_KEY (Short, kdb_short_t, "-128", -128);
	TEST_FROM_KEY (Short, kdb_short_t, "32767", 32767);
	TEST_FROM_KEY (Short, kdb_short_t, "-32768", -32768);

	TEST_FROM_KEY (UnsignedShort, kdb_unsigned_short_t, "15", 15);
	TEST_FROM_KEY (UnsignedShort, kdb_unsigned_short_t, "0", 0);
	TEST_FROM_KEY (UnsignedShort, kdb_unsigned_short_t, "255", 255);
	TEST_FROM_KEY (UnsignedShort, kdb_unsigned_short_t, "65535", 65535);

	TEST_FROM_KEY (Long, kdb_long_t, "15", 15);
	TEST_FROM_KEY (Long, kdb_long_t, "0", 0);
	TEST_FROM_KEY (Long, kdb_long_t, "127", 127);
	TEST_FROM_KEY (Long, kdb_long_t, "-128", -128);
	TEST_FROM_KEY (Long, kdb_long_t, "32767", 32767);
	TEST_FROM_KEY (Long, kdb_long_t, "-32768", -32768);
	TEST_FROM_KEY (Long, kdb_long_t, "2147483647", 2147483647);
	TEST_FROM_KEY (Long, kdb_long_t, "-2147483648", -2147483648);

	TEST_FROM_KEY (UnsignedLong, kdb_unsigned_long_t, "15", 15);
	TEST_FROM_KEY (UnsignedLong, kdb_unsigned_long_t, "0", 0);
	TEST_FROM_KEY (UnsignedLong, kdb_unsigned_long_t, "255", 255);
	TEST_FROM_KEY (UnsignedLong, kdb_unsigned_long_t, "65535", 65535);
	TEST_FROM_KEY (UnsignedLong, kdb_unsigned_long_t, "4294967295", 4294967295);

	TEST_FROM_KEY (LongLong, kdb_long_long_t, "15", 15);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "0", 0);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "127", 127);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "-128", -128);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "32767", 32767);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "-32768", -32768);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "2147483647", 2147483647);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "-2147483648", -2147483648);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "9223372036854775807", 9223372036854775807LL);
	TEST_FROM_KEY (LongLong, kdb_long_long_t, "-9223372036854775808", -9223372036854775807LL - 1LL);

	TEST_FROM_KEY (UnsignedLongLong, kdb_unsigned_long_long_t, "15", 15);
	TEST_FROM_KEY (UnsignedLongLong, kdb_unsigned_long_long_t, "0", 0);
	TEST_FROM_KEY (UnsignedLongLong, kdb_unsigned_long_long_t, "255", 255);
	TEST_FROM_KEY (UnsignedLongLong, kdb_unsigned_long_long_t, "65535", 65535);
	TEST_FROM_KEY (UnsignedLongLong, kdb_unsigned_long_long_t, "4294967295", 4294967295);
	TEST_FROM_KEY (UnsignedLongLong, kdb_unsigned_long_long_t, "18446744073709551615", 18446744073709551615ULL);

	// disable float-equal warnings
	// we intentionally compare floats exactly
	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	TEST_FROM_KEY (Float, kdb_float_t, "0", 0);
	TEST_FROM_KEY (Float, kdb_float_t, "1", 1);
	TEST_FROM_KEY (Float, kdb_float_t, "2.5", 2.5);
	TEST_FROM_KEY (Float, kdb_float_t, "2.5", 2.5);
	TEST_FROM_KEY (Float, kdb_float_t, "1.10000002", 1.10000002);
	TEST_FROM_KEY (Float, kdb_float_t, "-1.10000002", -1.10000002);

	TEST_FROM_KEY (Double, kdb_double_t, "0", 0);
	TEST_FROM_KEY (Double, kdb_double_t, "1", 1);
	TEST_FROM_KEY (Double, kdb_double_t, "2.5", 2.5);
	TEST_FROM_KEY (Double, kdb_double_t, "2.5", 2.5);
	TEST_FROM_KEY (Double, kdb_double_t, "1.1000000000000001", 1.1000000000000001);
	TEST_FROM_KEY (Double, kdb_double_t, "-1.1000000000000001", -1.1000000000000001);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	TEST_FROM_KEY (LongDouble, kdb_long_double_t, "0", 0);
	TEST_FROM_KEY (LongDouble, kdb_long_double_t, "1", 1);
	TEST_FROM_KEY (LongDouble, kdb_long_double_t, "2.5", 2.5);
	TEST_FROM_KEY (LongDouble, kdb_long_double_t, "2.5", 2.5);
	TEST_FROM_KEY (LongDouble, kdb_long_double_t, "1.10000000000000008882", 1.10000000000000008882);
	TEST_FROM_KEY (LongDouble, kdb_long_double_t, "-1.10000000000000008882", -1.10000000000000008882);

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

	ELEKTRA_DIAG_RESTORE
}

static void test_roundtrip (void)
{
	TEST_ROUNDTRIP (Boolean, kdb_boolean_t, "1", true);
	TEST_ROUNDTRIP (Boolean, kdb_boolean_t, "0", false);

	TEST_ROUNDTRIP (Char, kdb_char_t, "a", 'a');
	TEST_ROUNDTRIP (Char, kdb_char_t, "+", '+');
	TEST_ROUNDTRIP (Char, kdb_char_t, "\n", '\n');

	TEST_ROUNDTRIP (Octet, kdb_octet_t, "15", 15);
	TEST_ROUNDTRIP (Octet, kdb_octet_t, "0", 0);
	TEST_ROUNDTRIP (Octet, kdb_octet_t, "255", 255);

	TEST_ROUNDTRIP (Short, kdb_short_t, "15", 15);
	TEST_ROUNDTRIP (Short, kdb_short_t, "0", 0);
	TEST_ROUNDTRIP (Short, kdb_short_t, "127", 127);
	TEST_ROUNDTRIP (Short, kdb_short_t, "-128", -128);
	TEST_ROUNDTRIP (Short, kdb_short_t, "32767", 32767);
	TEST_ROUNDTRIP (Short, kdb_short_t, "-32768", -32768);

	TEST_ROUNDTRIP (UnsignedShort, kdb_unsigned_short_t, "15", 15);
	TEST_ROUNDTRIP (UnsignedShort, kdb_unsigned_short_t, "0", 0);
	TEST_ROUNDTRIP (UnsignedShort, kdb_unsigned_short_t, "255", 255);
	TEST_ROUNDTRIP (UnsignedShort, kdb_unsigned_short_t, "65535", 65535);

	TEST_ROUNDTRIP (Long, kdb_long_t, "15", 15);
	TEST_ROUNDTRIP (Long, kdb_long_t, "0", 0);
	TEST_ROUNDTRIP (Long, kdb_long_t, "127", 127);
	TEST_ROUNDTRIP (Long, kdb_long_t, "-128", -128);
	TEST_ROUNDTRIP (Long, kdb_long_t, "32767", 32767);
	TEST_ROUNDTRIP (Long, kdb_long_t, "-32768", -32768);
	TEST_ROUNDTRIP (Long, kdb_long_t, "2147483647", 2147483647);
	TEST_ROUNDTRIP (Long, kdb_long_t, "-2147483648", -2147483648);

	TEST_ROUNDTRIP (UnsignedLong, kdb_unsigned_long_t, "15", 15);
	TEST_ROUNDTRIP (UnsignedLong, kdb_unsigned_long_t, "0", 0);
	TEST_ROUNDTRIP (UnsignedLong, kdb_unsigned_long_t, "255", 255);
	TEST_ROUNDTRIP (UnsignedLong, kdb_unsigned_long_t, "65535", 65535);
	TEST_ROUNDTRIP (UnsignedLong, kdb_unsigned_long_t, "4294967295", 4294967295);

	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "15", 15);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "0", 0);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "127", 127);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "-128", -128);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "32767", 32767);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "-32768", -32768);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "2147483647", 2147483647);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "-2147483648", -2147483648);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "9223372036854775807", 9223372036854775807LL);
	TEST_ROUNDTRIP (LongLong, kdb_long_long_t, "-9223372036854775808", -9223372036854775807LL - 1LL);

	TEST_ROUNDTRIP (UnsignedLongLong, kdb_unsigned_long_long_t, "15", 15);
	TEST_ROUNDTRIP (UnsignedLongLong, kdb_unsigned_long_long_t, "0", 0);
	TEST_ROUNDTRIP (UnsignedLongLong, kdb_unsigned_long_long_t, "255", 255);
	TEST_ROUNDTRIP (UnsignedLongLong, kdb_unsigned_long_long_t, "65535", 65535);
	TEST_ROUNDTRIP (UnsignedLongLong, kdb_unsigned_long_long_t, "4294967295", 4294967295);
	TEST_ROUNDTRIP (UnsignedLongLong, kdb_unsigned_long_long_t, "18446744073709551615", 18446744073709551615ULL);

	// do double roundtrip for floats, because the initial value
	// may not be exactly representable, after one roundtrip
	// the value must stay the same for further roundtrips

	// disable float-equal warnings
	// we intentionally compare floats exactly
	ELEKTRA_DIAG_STORE
	ELEKTRA_DIAG_OFF_STR ("-Wfloat-equal")

	TEST_DOUBLE_ROUNDTRIP (Float, kdb_float_t, "0", 0);
	TEST_DOUBLE_ROUNDTRIP (Float, kdb_float_t, "1", 1);
	TEST_DOUBLE_ROUNDTRIP (Float, kdb_float_t, "2.5", 2.5);
	TEST_DOUBLE_ROUNDTRIP (Float, kdb_float_t, "2.5", 2.5);
	TEST_DOUBLE_ROUNDTRIP (Float, kdb_float_t, "1.1", 1.1);
	TEST_DOUBLE_ROUNDTRIP (Float, kdb_float_t, "-1.1", -1.1);

	TEST_DOUBLE_ROUNDTRIP (Double, kdb_double_t, "0", 0);
	TEST_DOUBLE_ROUNDTRIP (Double, kdb_double_t, "1", 1);
	TEST_DOUBLE_ROUNDTRIP (Double, kdb_double_t, "2.5", 2.5);
	TEST_DOUBLE_ROUNDTRIP (Double, kdb_double_t, "2.5", 2.5);
	TEST_DOUBLE_ROUNDTRIP (Double, kdb_double_t, "1.1", 1.1);
	TEST_DOUBLE_ROUNDTRIP (Double, kdb_double_t, "-1.1", -1.1);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	TEST_DOUBLE_ROUNDTRIP (LongDouble, kdb_long_double_t, "0", 0);
	TEST_DOUBLE_ROUNDTRIP (LongDouble, kdb_long_double_t, "1", 1);
	TEST_DOUBLE_ROUNDTRIP (LongDouble, kdb_long_double_t, "2.5", 2.5);
	TEST_DOUBLE_ROUNDTRIP (LongDouble, kdb_long_double_t, "2.5", 2.5);
	TEST_DOUBLE_ROUNDTRIP (LongDouble, kdb_long_double_t, "1.1", 1.1);
	TEST_DOUBLE_ROUNDTRIP (LongDouble, kdb_long_double_t, "-1.1", -1.1);

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

	ELEKTRA_DIAG_RESTORE
}

int main (int argc, char ** argv)
{
	printf (" CONVERSION   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_to_string ();
	test_from_key ();
	test_roundtrip ();

	print_result ("test_conversion");

	return nbError;
}
