#include "type.h"

#include <kdbhelper.h>
#include <kdbassert.h>

const char * binStr = "^0b[01](_?[01])*$";
const char * octStr = "^0o[0-7](_?[0-7])*$";
const char * decStr = "^[+-]?[1-9](_?[0-9])*$";
const char * hexStr = "^0x[0-9a-fA-F](_?[0-9a-fA-F])*$";
const char * floatStr = "^[+-]?(0|([1-9](_?[0-9])*))(\\.[0-9](_?[0-9])*)?([eE][1-9](_?[0-9])*)?$";
const char * bareStr = "^[a-zA-Z0-9_-]+$";

TypeChecker * createTypeChecker(void) {
	int result = 0;
	TypeChecker * typeChecker = (TypeChecker *)elektraCalloc(sizeof(TypeChecker));
	if (typeChecker == NULL) {
		return NULL;
	}
	result |= regcomp(&typeChecker->regexBin, binStr, REG_EXTENDED);
	ELEKTRA_ASSERT(result == 0, "Binary regex could not be compiled: '%s'", binStr);
	result |= regcomp(&typeChecker->regexOct, octStr, REG_EXTENDED);
	ELEKTRA_ASSERT(result == 0, "Octal regex could not be compiled: '%s'", octStr);
	result |= regcomp(&typeChecker->regexDec, decStr, REG_EXTENDED);
	ELEKTRA_ASSERT(result == 0, "Decimal regex could not be compiled: '%s'", decStr);
	result |= regcomp(&typeChecker->regexHex, hexStr, REG_EXTENDED);
	ELEKTRA_ASSERT(result == 0, "Hex regex could not be compiled: '%s'", hexStr);

	result |= regcomp(&typeChecker->regexFloat, floatStr, REG_EXTENDED);
	ELEKTRA_ASSERT(result == 0, "Float regex could not be compiled: '%s'", floatStr);

	result |= regcomp(&typeChecker->regexBare, bareStr, REG_EXTENDED);
	ELEKTRA_ASSERT(result == 0, "Bare regex could not be compiled: '%s'", bareStr);
	return typeChecker;
}

void destroyTypeChecker(TypeChecker * checker) {
	if (checker != NULL) {
		regfree(&checker->regexBin);
		regfree(&checker->regexOct);
		regfree(&checker->regexDec);
		regfree(&checker->regexHex);
		regfree(&checker->regexFloat);
		regfree(&checker->regexBare);
		elektraFree(checker);
	}
}

bool isNumber(TypeChecker * checker, const char * str) {
	return isBinary(checker, str) ||
		   isOctal(checker, str) ||
		   isDecimal(checker, str) ||
		   isHexadecimal(checker, str) ||
		   isFloat(checker, str);
}

bool isBinary(TypeChecker * checker, const char * str) {
	return regexec(&checker->regexBin, str, 0, NULL, 0) == 0;
}

bool isOctal(TypeChecker * checker, const char * str) {
	return regexec(&checker->regexOct, str, 0, NULL, 0) == 0;
}

bool isDecimal(TypeChecker * checker, const char * str) {
	return regexec(&checker->regexDec, str, 0, NULL, 0) == 0;
}

bool isHexadecimal(TypeChecker * checker, const char * str) {
	return regexec(&checker->regexHex, str, 0, NULL, 0) == 0;
}

bool isFloat(TypeChecker * checker, const char * str) {
	return regexec(&checker->regexFloat, str, 0, NULL, 0) == 0;
}

bool isBareString(TypeChecker * checker, const char * str) {
	return regexec(&checker->regexBare, str, 0, NULL, 0) == 0;
} 

