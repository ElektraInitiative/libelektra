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
	SCALAR_DATE_LOCAL_TIME
} ScalarType;

typedef struct
{
	ScalarType type;
	char * str;
	size_t line;
} Scalar;

Scalar * createScalar (ScalarType type, char * comment, size_t line);
Scalar * createScalarDup (ScalarType type, const char * comment, size_t line);
const char * getTypeCheckerType (const Scalar * scalar);
char * translateScalar (const Scalar * scalar);

bool isValidBareString (const char * str);
bool isValidDateTime (const Scalar * scalar);

#endif // ELEKTRA_PLUGIN_TOML_SCALAR_H
