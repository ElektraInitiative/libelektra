#ifndef ELEKTRA_PLUGIN_TOML_TYPE_H
#define ELEKTRA_PLUGIN_TOML_TYPE_H

#include <regex.h>
#include <stdbool.h>

typedef struct {
	regex_t regexBin;
	regex_t regexOct;
	regex_t regexDec;
	regex_t regexHex;
	regex_t regexFloat;
	regex_t regexBare;
} TypeChecker;

TypeChecker * createTypeChecker(void);
void destroyTypeChecker(TypeChecker * checker);
bool isNumber(TypeChecker * checker, const char * str);
bool isBinary(TypeChecker * checker, const char * str);
bool isOctal(TypeChecker * checker, const char * str);
bool isDecimal(TypeChecker * checker, const char * str);
bool isHexadecimal(TypeChecker * checker, const char * str);
bool isFloat(TypeChecker * checker, const char * str);
bool isBareString(TypeChecker * checker, const char * str);

#endif // ELEKTRA_PLUGIN_TOML_TYPE_H
