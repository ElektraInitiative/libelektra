// lgtm [cpp/missing-header-guard]

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
 * @param  CHECK_FAIL_BLOCK optional, defaults to logging a warning in the key. The code to be executed (before returning 0), if
 *                          CHECK_CONVERSION evaluates to false.
 * @param  PRE_CHECK_CONVERSION_BLOCK optional, defaults to empty. Allows to add additional code for pre-conversion checks
 *                                    (e.g. ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK)
 * @param  PRE_CHECK_CONVERSION       optional, defaults to true. A boolean expression to check the contents of `string` before conversion
 *                                    (e.g. ELEKTRA_TYPE_NEGATIVE_PRE_CHECK).
 * @param  PRE_CHECK_FAIL_BLOCK       optional, defaults to logging a warning in the key. The code to be executed (before returning 0), if
 *                                    PRE_CHECK_CONVERSION evaluates to false.
 * @param  DISABLE_UNDEF_PARAMETERS   define to disable undefining of parameters after the macro. Use if parameters
 *                                    are used within another supermacro.
 * @param  CODE_ONLY           optional, defaults to 0. Set to 1 to only generate the function body. This is useful, if you want to create a
 *                             function with a custom signature for example.
 * @param  KEY_PARAM_NAME      must be set, #if CODE_ONLY, will be set to 'key' otherwise. The name of the variable/parameter containing
 *                             the Key, whose value will be converted
 * @param  VARIABLE_PARAM_NAME must be set, #if CODE_ONLY, will be set to 'variable' otherwise. The name of the variable/parameter
 *                             containing the pointer to where the result should be written
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
#ifndef PRE_CHECK_FAIL_BLOCK
#define PRE_CHECK_FAIL_BLOCK ELEKTRA_LOG_WARNING ("pre-check for type conversion failed! string=%s", keyString (key));
#endif
#ifndef CHECK_FAIL_BLOCK
#define CHECK_FAIL_BLOCK ELEKTRA_LOG_WARNING ("type conversion failed! string=%s, stopped=%c errno=%d", keyString (key), *end, errno);
#endif
#ifndef CODE_ONLY
#define CODE_ONLY 0
#endif

#if CODE_ONLY
#if (!defined(KEY_PARAM_NAME) || !defined(VARIABLE_PARAM_NAME))
#error "When CODE_ONLY is defined, you have to #define KEY_PARAM_NAME and VARIABLE_PARAM_NAME"
#endif
#else
#undef KEY_PARAM_NAME
#undef VARIABLE_PARAM_NAME
#define KEY_PARAM_NAME key
#define VARIABLE_PARAM_NAME variable
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

#if !(CODE_ONLY)
#include <errno.h> // errno

#define TYPE_CONVERSION_SIGNATURE(TYPE, TYPE_NAME, NAME_MACRO)                                                                             \
	int NAME_MACRO (TYPE_NAME) (const Key * KEY_PARAM_NAME, TYPE * VARIABLE_PARAM_NAME)

/**
 * Convert string to TYPE.
 *
 * The variable is only changed if no conversion error occurred
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
#endif
	char * end ELEKTRA_UNUSED;
	const char * string = keyValue (KEY_PARAM_NAME);
	errno = 0;
	PRE_CHECK_BLOCK
	if (!PRE_CHECK_CONVERSION)
	{
		PRE_CHECK_FAIL_BLOCK
		return 0;
	}
	// convert string to target type
	VALUE_TYPE value = TO_VALUE;
	if (CHECK_CONVERSION)
	{
		// only update if conversion was successful
		*(VARIABLE_PARAM_NAME) = (TYPE) value;
		return 1;
	}
	else
	{
		CHECK_FAIL_BLOCK
		return 0;
	}
#if !(CODE_ONLY)
}

#undef TYPE_CONVERSION_SIGNATURE
#endif

#ifndef DISABLE_UNDEF_PARAMETERS
#undef TYPE
#undef VALUE_TYPE
#undef TYPE_NAME
#undef NAME_MACRO
#undef TO_VALUE
#undef CHECK_CONVERSION
#undef PRE_CHECK_BLOCK
#undef PRE_CHECK_CONVERSION
#undef PRE_CHECK_FAIL_BLOCK
#undef CHECK_FAIL_BLOCK
#undef CODE_ONLY
#undef KEY_PARAM_NAME
#undef VARIABLE_PARAM_NAME
#endif
#undef DISABLE_UNDEF_PARAMETERS
