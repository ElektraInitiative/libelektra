/**
 * @file type.h
 *
 * @brief Used for validating types.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_TOML_TYPE_H
#define ELEKTRA_PLUGIN_TOML_TYPE_H

#include <regex.h>
#include <stdbool.h>

typedef struct
{
	regex_t regexBin;
	regex_t regexOct;
	regex_t regexDec;
	regex_t regexHex;
	regex_t regexFloat;
	regex_t regexFloatSpecial;
	// regex_t regexBare;
	regex_t regexOffsetDt;
	regex_t regexLocalDt;
	regex_t regexLocalDate;
	regex_t regexLocalTime;
} TypeChecker;

/*
 * @brief Creates a new type checker.
 *
 * The type checker compiles different regular expressions, used for the validation of types.
 * If one of the regular expressions can not be compiled, an assertion will be thrown, as long
 * as assertions are enabled. If not enabled, regex compilation may silently fail.
 *
 * @retval Pointer The created type checker.
 * @retval NULL When out-of-memory
 */
TypeChecker * createTypeChecker (void);

/*
 * @brief Frees the type checker and all resources allocation within it.
 *
 * @param checker TypeChecker to be freed.
 */
void destroyTypeChecker (TypeChecker * checker);

/*
 * @brief Checks if string is any number.
 *
 * Checks if the given number is binary, octal, decimal, hexadecimal or float.
 *
 * @param str String to be checked.
 *
 * @retval True If string is a number.
 * @retval False Otherwise.
 */
bool isNumber (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is binary.
 *
 * @param str String to be checked.
 *
 * @retval True If string is binary.
 * @retval False Otherwise.
 */
bool isBinary (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is octal.
 *
 * @param str String to be checked.
 *
 * @retval True If string is octal.
 * @retval False Otherwise.
 */
bool isOctal (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is decimal.
 *
 * @param str String to be checked.
 *
 * @retval True If string is decimal.
 * @retval False Otherwise.
 */
bool isDecimal (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is hexadecimal
 *
 * @param str String to be checked.
 *
 * @retval True If string is hexadecimal.
 * @retval False Otherwise.
 */
bool isHexadecimal (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is floating point.
 *
 * Considers +/- nan and +/- inf as floats.
 *
 * @param str String to be checked.
 *
 * @retval True If string is float.
 * @retval False Otherwise.
 */
bool isFloat (TypeChecker * checker, const char * str);

// bool isBareString(TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is a datetime.
 *
 * Checks if the syntax matches a RFC3339 datetime.
 *
 * @param str String to be checked.
 *
 * @retval True If string is a datetime.
 * @retval False Otherwise.
 */
bool isDateTime (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string could be an offset datetime.
 *
 * Checks only syntax.
 *
 * @param str String to be checked.
 *
 * @retval True If string is syntactically an offset datetime.
 * @retval False Otherwise.
 */
bool isOffsetDatetime (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is a local datetime.
 *
 * @param str String to be checked.
 *
 * @retval True If string is an local datetime.
 * @retval False Otherwise.
 */
bool isLocalDateTime (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is a date.
 *
 * @param str String to be checked.
 *
 * @retval True If string is a date.
 * @retval False Otherwise.
 */
bool isLocalDate (TypeChecker * checker, const char * str);

/*
 * @brief Checks if string is a local time.
 *
 * @param str String to be checked.
 *
 * @retval True If string is a local time.
 * @retval False Otherwise.
 */
bool isLocalTime (TypeChecker * checker, const char * str);

/*
 * @brief Checks the validity of datetime values.
 *
 * The supplied string must already be matched as a datetime,
 * otherwise invalid values may be returned or invalid memory accesses may happen.
 *
 * Will check for leap years. Ignores leap seconds, so a second value of 60 is always valid.
 *
 * @param str A datetime string.
 *
 * @retval True If Values are valid.
 * @retval False Otherwise.
 */
bool validOffsetDateTimeValues (const char * str);

/*
 * @brief Checks the validity of local datetime values.
 *
 * The supplied string must already be matched as a local datetime,
 * otherwise invalid values may be returned or invalid memory accesses may happen.
 *
 * Will check for leap years. Ignores leap seconds, so a second value of 60 is always valid.
 *
 * @param str A datetime string.
 *
 * @retval True If Values are valid.
 * @retval False Otherwise.
 */
bool validLocalDateTimeValues (const char * str);

/*
 * @brief Checks the validity of local date values.
 *
 * The supplied string must already be matched as a local date,
 * otherwise invalid values may be returned or invalid memory accesses may happen.
 *
 * Will check for leap years.
 *
 * @param str A datetime string.
 *
 * @retval True If Values are valid.
 * @retval False Otherwise.
 */

bool validLocalDateValues (const char * str);

/*
 * @brief Checks the validity of local time values.
 *
 * The supplied string must already be matched as a local time,
 * otherwise invalid values may be returned or invalid memory accesses may happen.
 *
 * Ignores leap seconds, so a second value of 60 is always valid.
 *
 * @param str A datetime string.
 *
 * @retval True If Values are valid.
 * @retval False Otherwise.
 */

bool validLocalTimeValues (const char * str);


bool isValidNumberBase (const char * str, unsigned long long base);

#endif // ELEKTRA_PLUGIN_TOML_TYPE_H
