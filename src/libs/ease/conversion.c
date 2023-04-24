/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <ctype.h>
#include <elektra/type/conversion.h>
#include <errno.h>
#include <internal/utility/old_helper.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

#define CAT(X, Y) CAT_ (X, Y)
#define CAT_(X, Y) X##Y

/**
 * Converts a Key to string.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToString (const Key * key ELEKTRA_UNUSED, const char ** variable ELEKTRA_UNUSED)
{
#define TYPE_NAME String
#define TYPE const char *
#define KDB_TYPE KDB_TYPE_STRING
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_FAIL_BLOCK
#define TO_VALUE (string)
#define PRE_CHECK_CONVERSION (keyIsString (key) == 1)
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable
#define CODE_ONLY 1

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}

/**
 * Converts a Key to boolean.
 *
 * The value "1" is regarded as true, anything is as false.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToBoolean (const Key * key ELEKTRA_UNUSED, kdb_boolean_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME Boolean
#define TYPE kdb_boolean_t
#define KDB_TYPE KDB_TYPE_BOOLEAN
#define PRE_CHECK_CONVERSION ((string[0] == '0' || string[0] == '1') && string[1] == '\0')
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_FAIL_BLOCK
#define TO_VALUE (string[0] == '1')
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to char.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToChar (const Key * key ELEKTRA_UNUSED, kdb_char_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME Char
#define TYPE kdb_char_t
#define KDB_TYPE KDB_TYPE_CHAR
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_FAIL_BLOCK
#define TO_VALUE (string[0])
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to octet.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToOctet (const Key * key ELEKTRA_UNUSED, kdb_octet_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME Octet
#define TYPE kdb_octet_t
#define KDB_TYPE KDB_TYPE_OCTET
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION (ELEKTRA_TYPE_NEGATIVE_PRE_CHECK)
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= UINT8_MAX)
#define CHECK_FAIL_BLOCK
#define VALUE_TYPE unsigned long
#define TO_VALUE (strtoul (string, &end, 10))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to short.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToShort (const Key * key ELEKTRA_UNUSED, kdb_short_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME Short
#define TYPE kdb_short_t
#define KDB_TYPE KDB_TYPE_SHORT
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= INT16_MAX && value >= INT16_MIN)
#define CHECK_FAIL_BLOCK
#define VALUE_TYPE long
#define TO_VALUE (strtol (string, &end, 10))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to unsigned_short.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToUnsignedShort (const Key * key ELEKTRA_UNUSED, kdb_unsigned_short_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME UnsignedShort
#define TYPE kdb_unsigned_short_t
#define KDB_TYPE KDB_TYPE_UNSIGNED_SHORT
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION (ELEKTRA_TYPE_NEGATIVE_PRE_CHECK)
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= UINT16_MAX)
#define CHECK_FAIL_BLOCK
#define VALUE_TYPE unsigned long
#define TO_VALUE (strtoul (string, &end, 10))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to long.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToLong (const Key * key ELEKTRA_UNUSED, kdb_long_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME Long
#define TYPE kdb_long_t
#define KDB_TYPE KDB_TYPE_LONG
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= INT32_MAX && value >= INT32_MIN)
#define CHECK_FAIL_BLOCK
#define VALUE_TYPE long long
#define TO_VALUE (ELEKTRA_LONG_LONG_S (string, &end, 10))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to unsigned_long.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToUnsignedLong (const Key * key ELEKTRA_UNUSED, kdb_unsigned_long_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME UnsignedLong
#define TYPE kdb_unsigned_long_t
#define KDB_TYPE KDB_TYPE_UNSIGNED_LONG
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION (ELEKTRA_TYPE_NEGATIVE_PRE_CHECK)
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= UINT32_MAX)
#define CHECK_FAIL_BLOCK
#define VALUE_TYPE unsigned long long
#define TO_VALUE (ELEKTRA_UNSIGNED_LONG_LONG_S (string, &end, 10))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to long_long.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToLongLong (const Key * key ELEKTRA_UNUSED, kdb_long_long_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME LongLong
#define TYPE kdb_long_long_t
#define KDB_TYPE KDB_TYPE_LONG_LONG
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= INT64_MAX && value >= INT64_MIN)
#define CHECK_FAIL_BLOCK
#define VALUE_TYPE long long
#define TO_VALUE (ELEKTRA_LONG_LONG_S (string, &end, 10))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to unsigned_long_long.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToUnsignedLongLong (const Key * key ELEKTRA_UNUSED, kdb_unsigned_long_long_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME UnsignedLongLong
#define TYPE kdb_unsigned_long_long_t
#define KDB_TYPE KDB_TYPE_UNSIGNED_LONG_LONG
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION (ELEKTRA_TYPE_NEGATIVE_PRE_CHECK)
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= UINT64_MAX)
#define CHECK_FAIL_BLOCK
#define VALUE_TYPE unsigned long long
#define TO_VALUE (ELEKTRA_UNSIGNED_LONG_LONG_S (string, &end, 10))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to float.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToFloat (const Key * key ELEKTRA_UNUSED, kdb_float_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME Float
#define TYPE kdb_float_t
#define KDB_TYPE KDB_TYPE_FLOAT
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#define CHECK_FAIL_BLOCK
#define TO_VALUE (strtof (string, &end))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}


/**
 * Converts a Key to double.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToDouble (const Key * key ELEKTRA_UNUSED, kdb_double_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME Double
#define TYPE kdb_double_t
#define KDB_TYPE KDB_TYPE_DOUBLE
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#define CHECK_FAIL_BLOCK
#define TO_VALUE (strtod (string, &end))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}

/**
 * Converts a Key to long_double.
 *
 * The variable pointed to by @p variable is unchanged,
 * if an error occurs.
 *
 * @param key      the key to convert
 * @param variable pointer to the output variable
 * @retval 1 on success
 * @retval 0 otherwise
 */
int elektraKeyToLongDouble (const Key * key ELEKTRA_UNUSED, kdb_long_double_t * variable ELEKTRA_UNUSED)
{
#define TYPE_NAME LongDouble
#define TYPE kdb_long_double_t
#define KDB_TYPE KDB_TYPE_LONG_DOUBLE
#define PRE_CHECK_FAIL_BLOCK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#define CHECK_FAIL_BLOCK
#define TO_VALUE (strtold (string, &end))
#define NAME_MACRO(TYPE_NAME) CAT (elektraKeyTo, TYPE_NAME)
#define CODE_ONLY 1
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable

#include <internal/macros/type_create_to_value.h>

#undef KDB_TYPE
}

/**
 * Converts a boolean to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the boolean to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraBooleanToString (kdb_boolean_t value)
{
	return elektraFormat ("%s", value ? "1" : "0");
}

/**
 * Converts a char to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraCharToString (kdb_char_t value)
{
	return elektraFormat ("%c", value);
}

/**
 * Converts an octet to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraOctetToString (kdb_octet_t value)
{
	return elektraFormat ("%d", value);
}

/**
 * Converts a short to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraShortToString (kdb_short_t value)
{
	return elektraFormat ("%d", value);
}

/**
 * Converts an unsigned short to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraUnsignedShortToString (kdb_unsigned_short_t value)
{
	return elektraFormat ("%d", value);
}

/**
 * Converts a long to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraLongToString (kdb_long_t value)
{
	return elektraFormat (ELEKTRA_LONG_F, value);
}

/**
 * Converts an unsigned long to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraUnsignedLongToString (kdb_unsigned_long_t value)
{
	return elektraFormat (ELEKTRA_UNSIGNED_LONG_F, value);
}

/**
 * Converts a long long to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraLongLongToString (kdb_long_long_t value)
{
	return elektraFormat (ELEKTRA_LONG_LONG_F, value);
}

/**
 * Converts an unsigned long long to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraUnsignedLongLongToString (kdb_unsigned_long_long_t value)
{
	return elektraFormat (ELEKTRA_UNSIGNED_LONG_LONG_F, value);
}

/**
 * Converts a float to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraFloatToString (kdb_float_t value)
{
	return elektraFormat ("%.9g", value);
}

/**
 * Converts a double to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraDoubleToString (kdb_double_t value)
{
	return elektraFormat ("%.17g", value);
}

/**
 * Converts a long double to string
 *
 * The string is allocated with elektraMalloc() and must
 * be disposed of with elektraFree().
 *
 * @param value the value to convert
 * @return a new string allocated with elektraMalloc, or 0 on error
 */
char * elektraLongDoubleToString (kdb_long_double_t value)
{
	return elektraFormat ("%.21Lg", value);
}

#ifdef __cplusplus
};
#endif
