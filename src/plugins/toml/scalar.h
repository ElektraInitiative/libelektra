/**
 * @file scalar.c
 *
 * @brief Functions for handling scalar key values, used on reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TOML_SCALAR_H
#define ELEKTRA_PLUGIN_TOML_SCALAR_H

#include <stdbool.h>
#include <stddef.h>

typedef enum
{
	SCALAR_INTEGER_DEC,
	SCALAR_INTEGER_HEX,
	SCALAR_INTEGER_OCT,
	SCALAR_INTEGER_BIN,
	SCALAR_BOOLEAN,
	SCALAR_FLOAT_NUM,
	SCALAR_FLOAT_INF,
	SCALAR_FLOAT_POS_INF,
	SCALAR_FLOAT_NEG_INF,
	SCALAR_FLOAT_NAN,
	SCALAR_FLOAT_POS_NAN,
	SCALAR_FLOAT_NEG_NAN,
	SCALAR_STRING_COMMENT,
	SCALAR_STRING_BARE,
	SCALAR_STRING_LITERAL,
	SCALAR_STRING_BASIC,
	SCALAR_STRING_ML_LITERAL,
	SCALAR_STRING_ML_BASIC,
	SCALAR_DATE_OFFSET_DATETIME,
	SCALAR_DATE_LOCAL_DATETIME,
	SCALAR_DATE_LOCAL_DATE,
	SCALAR_DATE_LOCAL_TIME,
} ScalarType;

typedef struct
{
	ScalarType type;
	char * str;
	char * orig;
	size_t leadingSpaces;
	size_t line;
} Scalar;

/*
 * @brief Creates a new scalar
 *
 * This function only creates a shallow copy of the supplied
 * scalarString takes ownership of it. Therefore, the supplied string
 * must not be freed in any other place than the freeScalar function.
 * If this cannot be guarenteed, use createScalarDup for a deep copy instead.
 *
 * This function is used in the lexer during the reading of string.s
 *
 * @param type The type of the scalar.
 * @param scalarString The raw string representation of the scalar.
 * @param origString The original bytes as part of the file.
 * @param line The line number, where the scalar was read from.
 *
 * @retval Pointer On success.
 * @retval NULL When out-of-memory.
 */
Scalar * createScalar (ScalarType type, char * scalarString, char * origString, size_t line);

/*
 * @brief Creates a new scalar
 *
 * Creates a deep copy of the given scalar string.
 *
 * This function is used in the lexer during the reading of non-string values.
 *
 * @param type The type of the scalar.
 * @param scalarString The string representation of the scalar.
 * @param origString The original bytes as part of the file.
 * @param line The line number, where the scalar was read from.
 *
 * @retval Pointer On success.
 * @retval NULL When out-of-memory.
 */
Scalar * createScalarDup (ScalarType type, const char * scalarString, const char * origString, size_t line);

/*
 * @brief Frees up a scalar and any memory pointed within it.
 *
 * @param scalar Scalar to be freed.
 */
void freeScalar (Scalar * scalar);


/*
 * @brief Converts the given scalar to format usable within Elektra.
 *
 * Converts binary, octal and hexadecimal scalars to decimal,
 * Handles special cases for floats (inf, nan) and strips underscores.
 * Applies escape characters to basic strings.
 *
 * @param scalar Scalar to be converted.
 *
 * @retval Pointer On Success.
 * @retval NULL When supplying unknown scalar type or on conversion errors.
 */
char * translateScalar (const Scalar * scalar);

/*
 * @brief Checks if the given string is bare.
 *
 * TOML bare strings only contain alphanumeric characters, underscores or hyphens.
 *
 * @param str String to be checked. Must not be NULL.
 *
 * @retval true If string only contains bare characters.
 * @retval false If contains non-bare characters.
 */
bool isValidBareString (const char * str);

/*
 * @brief Checks if the given scalar contains a valid RFC3339 datetime.
 *
 * @param scalar Scalare to be checked.
 *
 * @retval true If the scalar contains a valid RFC3339 datetime.
 * @retval false Otherwise.
 */
bool isValidDateTime (const Scalar * scalar);


#endif // ELEKTRA_PLUGIN_TOML_SCALAR_H
