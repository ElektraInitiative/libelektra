/**
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 * @brief Create key to type conversion function.
 *
 * This supermacro creates the following functions:
 * - int NAME_MACRO (TYPE_NAME) (Key * key, TYPE * variable)
 *
 * @param  TYPE             valid C type (e.g. int or kdb_short_t)
 * @param  TYPE_NAME        name suffix for the functions (e.g. Int or UnsignedLong)
 * @param  VALUE_TYPE       optional, defaults to TYPE. Ideally a larger type assigned to variable `value` for
 *                          checking the range before the variable is updated
 * @param  TO_VALUE         expression for converting `string` (variable containing the key value) to VALUE_TYPE
 * @param  CHECK_CONVERSION optional, defaults to true. A boolean expression. Allows to check the range after
 *                          conversion. Use ELEKTRA_TYPE_CHECK_CONVERSION to check if a conversion using
 *                          strto*()-functions was successful and ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (RANGE)
 *                          to check additionally for a specified range.
 * @param  PRE_CHECK_CONVERSION_BLOCK optional, defaults to empty. Allows to add additional code for pre-conversion checks
 *                                    (e.g. ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK)
 * @param  PRE_CHECK_CONVERSION       optional, defaults to true. A boolean expression to check the contents of `string` before conversion
 *                                    (e.g. ELEKTRA_TYPE_NEGATIVE_PRE_CHECK).
 * @param  DISABLE_UNDEF_PARAMETERS   define to disable undefining of parameters after the macro. Use if parameters
 *                                    are used within another supermacro.
 */
#ifndef TYPE
#error "You have to #define TYPE, TYPE_NAME, TO_VALUE and NAME_MACRO before including the type_create_to_value supermacro"
#endif
#ifndef VALUE_TYPE
// use type as default if not set
#define VALUE_TYPE TYPE
#endif
#ifndef TYPE_NAME
#error "You have to #define TYPE, TYPE_NAME, TO_VALUE and NAME_MACRO before including the type_create_to_value supermacro"
#endif
#ifndef NAME_MACRO
#error "You have to #define TYPE, TYPE_NAME, TO_VALUE and NAME_MACRO before including the type_create_to_value supermacro"
#endif
#ifndef TO_VALUE
#error "You have to #define TYPE, TYPE_NAME, TO_VALUE and NAME_MACRO before including the type_create_to_value supermacro"
#endif
#ifndef CHECK_CONVERSION
#define CHECK_CONVERSION 1
#endif
#ifndef PRE_CHECK_CONVERSION
#define PRE_CHECK_CONVERSION 1
#endif
#ifndef PRE_CHECK_BLOCK
#define PRE_CHECK_BLOCK
#endif

// These macros get defined at first inclusion
#ifndef ELEKTRA_TYPE_CONVERSION_MACROS
#define ELEKTRA_TYPE_CONVERSION_MACROS
#define ELEKTRA_TYPE_CHECK_CONVERSION (*end == 0 && errno == 0)
#define ELEKTRA_TYPE_CHECK_CONVERSION_RANGE(CHECK_RANGE) (ELEKTRA_TYPE_CHECK_CONVERSION && CHECK_RANGE)
#define ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK                                                                                              \
	const char * test = string;                                                                                                        \
	while (isspace (test[0]) || test[0] == 0)                                                                                          \
	{                                                                                                                                  \
		test++;                                                                                                                    \
	}
#define ELEKTRA_TYPE_NEGATIVE_PRE_CHECK (test[0] != '-')
#endif

#include <errno.h> // errno

#define TYPE_CONVERSION_SIGNATURE(TYPE, TYPE_NAME, NAME_MACRO) int NAME_MACRO (TYPE_NAME) (Key * key, TYPE * variable)

/**
 * Convert string to TYPE.
 *
 * The variable is only changed if no conversion error occured
 *
 * Example:
 * int variable = 1234;
 * if (!NAME_MACRO (TYPE_NAME) (key, &variable))
 * {
 *   // conversion failed
 *   // variable == 1234
 * }
 * // variable was changed
 *
 * @param key        key
 * @param variable   pointer to variable
 * @retval 1 on success
 * @retval 0 on conversion error
 */
TYPE_CONVERSION_SIGNATURE (TYPE, TYPE_NAME, NAME_MACRO)
{
	char * end ELEKTRA_UNUSED;
	const char * string = keyValue (key);
	errno = 0;
	PRE_CHECK_BLOCK
	if (!PRE_CHECK_CONVERSION)
	{
		ELEKTRA_LOG_WARNING ("pre-check for type conversion failed! string=%s", keyString (key));
		return 0;
	}
	// convert string to target type
	VALUE_TYPE value = TO_VALUE;
	if (CHECK_CONVERSION)
	{
		// only update if conversion was successful
		*(variable) = value;
		return 1;
	}
	else
	{
		ELEKTRA_LOG_WARNING ("type conversion failed! string=%s, stopped=%c errno=%d", keyString (key), *end, errno);
		return 0;
	}
}

#undef TYPE_CONVERSION_SIGNATURE

#ifndef DISABLE_UNDEF_PARAMETERS
#undef TYPE
#undef VALUE_TYPE
#undef TYPE_NAME
#undef NAME_MACRO
#undef TO_VALUE
#undef CHECK_CONVERSION
#undef PRE_CHECK_BLOCK
#undef PRE_CHECK_CONVERSION
#endif
#undef DISABLE_UNDEF_PARAMETERS
