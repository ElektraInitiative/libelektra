%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <stdbool.h>

#include "parser.h"
#include "scalar.h"
#include "driver.h"

static char* readLiteral(Driver * driver);
static char* readBasic(Driver * driver);
static char* readMultilineLiteral(Driver * driver);
static char* readMultilineBasic(Driver * driver);
static char* readComment(Driver * driver);

static void appendChar(char**, size_t*, size_t*, char);
static void readNonAsciiChar(char**, size_t*, size_t*, Driver * driver);
static void readHex(char**, size_t*, size_t*, size_t, Driver * driver);
static int isHex(char);
static bool isValidfullDate(const char * fullDate);
static bool isValidDate(int year, int month, int day);
static bool isLeapYear(int year);

typedef struct _ConditionStack {
	int condition;
	struct _ConditionStack* next;
} ConditionStack;

static ConditionStack* stackRoot = NULL;

static void pushCondition(int);
static void popCondition(void);
static void popConditionIf(int);

%}

%option yylineno
%option noyywrap

alpha [a-zA-Z]
digit [0-9]
alnum {alpha}|{digit}

whitespace_char [\t ]
whitespace {whitespace_char}*
newline (\r\n)|\n

dec_char {digit}
dec_nzero [1-9]
hex_char [0-9a-fA-F]
oct_char [0-8]
bin_char [01]
decimal {dec_nzero}("_"?{dec_char})*
decimal_pm [+-]?{decimal}

bare_char [-_]|{alnum}

offset_datetime {full_date}[T ]{full_time}
local_datetime  {full_date}[T ]{partial_time}
local_date {full_date}
local_time {partial_time}

full_date {digit}{4}"-"{digit}{2}"-"{digit}{2}
full_time {partial_time}{time_offset}
partial_time {digit}{2}":"{digit}{2}":"{digit}{2}("."{digit}+)?
time_offset "Z"|{time_num_offset}
time_num_offset [+-]{digit}{2}":"{digit}{2}

%s RHS
%s IN_BRACKETS
%s IN_CURLY

%%

"=" {
	pushCondition(RHS);
	return EQUAL;
}

"." return DOT;
"," {
	popConditionIf(RHS);
	return COMMA;
}

<INITIAL,RHS,IN_BRACKETS>{newline} {
	popConditionIf(RHS);
	return NEWLINE;
}

"[" {
	pushCondition(IN_BRACKETS);
	return BRACKETS_OPEN;
}
"]" {
	popCondition();
	return BRACKETS_CLOSE;
}

"{" {
	pushCondition(IN_CURLY);
	return CURLY_OPEN;
}
"}" {
	popConditionIf(RHS);
	popCondition();
	return CURLY_CLOSE;
}

"#" {
	char* str = readComment(driver);
	unput('\n');
	yylval.scalar = createScalar(SCALAR_STRING_COMMENT, str, yylineno);
	return COMMENT;
}

"'" {
	char* str = readLiteral(driver);
	yylval.scalar = createScalar(SCALAR_STRING_LITERAL, str, yylineno);
	return LITERAL_STRING;
}

"'''" {
	int lineStart = yylineno;
	char* str = readMultilineLiteral(driver);
	yylval.scalar = createScalar(SCALAR_STRING_ML_LITERAL, str, lineStart);
	return MULTI_LITERAL_STRING;
}

"\"" {
	int lineStart = yylineno;
	char* str = readBasic(driver);
	yylval.scalar = createScalar(SCALAR_STRING_BASIC, str, lineStart);
	return BASIC_STRING;
}

"\"\"\"" {
	int lineStart = yylineno;
	char* str = readMultilineBasic(driver);
	yylval.scalar = createScalar(SCALAR_STRING_ML_BASIC, str, lineStart);
	return MULTI_BASIC_STRING;
}

<RHS,IN_BRACKETS>{offset_datetime} {
    if (!isValidfullDate (yytext)) {
        driverError (driver, yylineno, "Malformed input: Invalid date: '%s'", yytext);
    }
	yylval.scalar = createScalarDup(SCALAR_DATE_OFFSET_DATETIME, yytext, yylineno);
	return OFFSET_DATETIME;
}
<RHS,IN_BRACKETS>{local_datetime} {
    if (!isValidfullDate (yytext)) {
        driverError (driver, yylineno, "Malformed input: Invalid date: '%s'", yytext);
    }
	yylval.scalar = createScalarDup(SCALAR_DATE_LOCAL_DATETIME, yytext, yylineno);
	return LOCAL_DATETIME;
}
<RHS,IN_BRACKETS>{local_date} {
    if (!isValidfullDate (yytext)) {
        driverError (driver, yylineno, "Malformed input: Invalid date: '%s'", yytext);
    }
	yylval.scalar = createScalarDup(SCALAR_DATE_LOCAL_DATE, yytext, yylineno);
	return LOCAL_DATE;
}
<RHS,IN_BRACKETS>{local_time} {
	yylval.scalar = createScalarDup(SCALAR_DATE_LOCAL_TIME, yytext, yylineno);
	return LOCAL_TIME;
}

<RHS,IN_BRACKETS>{decimal_pm} {
	yylval.scalar = createScalarDup(SCALAR_INTEGER_DEC, yytext, yylineno);
	return DECIMAL;
}

<RHS,IN_BRACKETS>("+"|"-")?"0" {
	yylval.scalar = createScalarDup(SCALAR_INTEGER_DEC, yytext, yylineno);
	return DECIMAL;
}

<RHS,IN_BRACKETS>"0x"{hex_char}("_"?{hex_char})* {
	yylval.scalar = createScalarDup(SCALAR_INTEGER_HEX, yytext, yylineno);
	return HEXADECIMAL;
}

<RHS,IN_BRACKETS>"0o"{oct_char}("_"?{oct_char})* {
	yylval.scalar = createScalarDup(SCALAR_INTEGER_OCT, yytext, yylineno);
	return OCTAL;
}

<RHS,IN_BRACKETS>"0b"{bin_char}("_"?{bin_char})* {
	yylval.scalar = createScalarDup(SCALAR_INTEGER_BIN, yytext, yylineno);
	return BINARY;
}

<RHS,IN_BRACKETS>{decimal_pm}("."{decimal})?([eE]{decimal_pm})? {
	yylval.scalar = createScalarDup(SCALAR_FLOAT_NUM, yytext, yylineno);
	return FLOAT;
}
<RHS,IN_BRACKETS>("+"|"-")?"inf" {
	yylval.scalar = createScalarDup(SCALAR_FLOAT_NUM, yytext, yylineno);
	switch(yytext[0]) {
		case '+':   yylval.scalar->type = SCALAR_FLOAT_POS_INF; break;
		case '-':   yylval.scalar->type = SCALAR_FLOAT_NEG_INF; break;
		default:    yylval.scalar->type = SCALAR_FLOAT_INF;     break;
	}
	return FLOAT;
}
<RHS,IN_BRACKETS>("+"|"-")?"nan" {
	yylval.scalar = createScalarDup(SCALAR_FLOAT_NUM, yytext, yylineno);
	switch(yytext[0]) {
		case '+':   yylval.scalar->type = SCALAR_FLOAT_POS_NAN; break;
		case '-':   yylval.scalar->type = SCALAR_FLOAT_NEG_NAN; break;
		default:    yylval.scalar->type = SCALAR_FLOAT_NAN;     break;
	}
	return FLOAT;
}


<RHS,IN_BRACKETS>"true"|"false" {
	yylval.scalar = createScalarDup(SCALAR_BOOLEAN, yytext, yylineno);
	return BOOLEAN;
}

<INITIAL,IN_BRACKETS,IN_CURLY>{bare_char}+ {
	yylval.scalar = createScalarDup(SCALAR_STRING_BARE, yytext, yylineno);
	return BARE_STRING;
}

<IN_CURLY>{newline} {
    driverError (driver, yylineno, "Malformed input: Unexpected newline in inline table");
}

{whitespace} ;

. {
    driverError (driver, yylineno, "Malformed input: Unexpected sequence: '%s'", yytext);
}

%%


// scanned date must already have correct rfc 3339 shape
// functions checks only, if day is valid for their month/leapyear
static bool isValidfullDate(const char * fullDate) {
    int y, m, d;
    sscanf(fullDate, "%4d-%2d-%2d", &y, &m, &d);
    return isValidDate (y, m, d);
}

static bool isValidDate(int year, int month, int day) {
    if (year > 9999 ||
        month < 1 ||
        month > 12) {
        return false;
}
    if (day >= 1) {
        if (day <= 28) {
            return true;
        } else  {
            switch (month) {
                case 1:
                case 3:
                case 5:
                case 7:
                case 8:
                case 10:
                case 12:
                    return day <= 31;
                case 4:
                case 6:
                case 9:
                case 11:
                    return day <= 30;
                case 2:
                    return day <= isLeapYear(year) ? 29 : 28;
                default:
                    assert(0);
            }
        }
    }
    return false;
}

static bool isLeapYear(int year) {
    if (year % 4 == 0) {
        if (year % 100 == 0) {
            if (year % 400 == 0) {
                return true;
            } else {
                return false;
            }
        } else {
            return true;
        }
    }
    return false;
}

static void pushCondition(int condition) {
	if (stackRoot == NULL) {
		stackRoot = malloc(sizeof(ConditionStack));
		stackRoot->condition = condition;
		stackRoot->next = NULL;
	} else {
		ConditionStack* newRoot = malloc(sizeof(ConditionStack));
		newRoot->condition = condition;
		newRoot->next = stackRoot;
		stackRoot = newRoot;
	}
	BEGIN(condition);
}

static void popCondition(void) {
    assert (stackRoot != NULL);
    stackRoot = stackRoot->next;
    if (stackRoot == NULL) {
        BEGIN(INITIAL);
    } else {
        BEGIN(stackRoot->condition);
    }
}

static void popConditionIf(int condition) {
	if (YY_START == condition) {
		popCondition();
	}
}

static char* readLiteral(Driver * driver) {
	char* buffer = malloc(64 * sizeof(char));
	size_t size = 64;
	size_t index = 0;
	char c;

	while((c = input()) != 0x27) { // terminating ' character
		if (c == 0x09 || (c >= 0x20 && c <= 0x7E)) { // read valid ascii chars
			appendChar(&buffer, &index, &size, c);
		} else { // read valid non-ascii (utf8 chars)
			unput(c);
			readNonAsciiChar(&buffer, &index, &size, driver);
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static char* readBasic(Driver * driver) {
	char* buffer = malloc(64 * sizeof(char));
	size_t size = 64;
	size_t index = 0;
	char c;

	while((c = input()) != 0x22) { // terminating " character
		if (c == 0x09 || (c >= 0x20 && c <= 0x7E)) { // read valid ascii chars
			appendChar(&buffer, &index, &size, c);
			if (c == 0x5C) {    // backslash for escaping
				c = input();
				switch(c) {
					case 0x22:
					case 0x5C:
					case 0x62:
					case 0x66:
					case 0x6E:
					case 0x72:
					case 0x74:
						appendChar(&buffer, &index, &size, c);
						break;
					case 0x75:
						appendChar(&buffer, &index, &size, c);
						readHex(&buffer, &index, &size, 4, driver);
						break;
					case 0x55:
						appendChar(&buffer, &index, &size, c);
						readHex(&buffer, &index, &size, 8, driver);
						break;
					default:
                        driverError (driver, yylineno, "Malformed Input: Basic String with invalid escape char: \'%c'", c);
				}
			}
		} else { // read valid non-ascii (utf8 chars)
			unput(c);
			readNonAsciiChar(&buffer, &index, &size, driver);
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static char* readMultilineLiteral(Driver * driver) {
	char* buffer = malloc(64 * sizeof(char));
	size_t size = 64;
	size_t index = 0;
	char c;
	int terminatorCount = 0;
    
	while(terminatorCount < 3) {
		c = input();
		if (c == 0x27) {
			terminatorCount++;
		} else {
			while (terminatorCount > 0) {
				appendChar(&buffer, &index, &size, 0x27);
				terminatorCount--;
			}
			if (c == 0x09 || c == 0x0A || (c >= 0x20 && c <= 0x7E)) { // read valid ascii chars
				appendChar(&buffer, &index, &size, c);
			} else { // read valid non-ascii (utf8 chars)
				unput(c);
				readNonAsciiChar(&buffer, &index, &size, driver);
			}
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static char* readMultilineBasic(Driver * driver) {
	char* buffer = malloc(64 * sizeof(char));
	size_t size = 64;
	size_t index = 0;
	char c;
	int terminatorCount = 0;

	while(terminatorCount < 3) {
		c = input();
		if (c == 0x22) {
			terminatorCount++;
		} else {
			while (terminatorCount > 0) {
				appendChar(&buffer, &index, &size, 0x22);
				terminatorCount--;
			}
			if (c == 0x09 || c == 0x0A || (c >= 0x20 && c <= 0x7E)) { // read valid ascii chars
				appendChar(&buffer, &index, &size, c);
				if (c == 0x5C) {    // backslash for escaping
					c = input();
					switch(c) {
						case 0x0A:
						case 0x22:
						case 0x5C:
						case 0x62:
						case 0x66:
						case 0x6E:
						case 0x72:
						case 0x74:
							appendChar(&buffer, &index, &size, c);
							break;
						case 0x75:
							appendChar(&buffer, &index, &size, c);
							readHex(&buffer, &index, &size, 4, driver);
							break;
						case 0x55:
							appendChar(&buffer, &index, &size, c);
							readHex(&buffer, &index, &size, 8, driver);
							break;
						default:
                            driverError (driver, yylineno, "Malformed Input: Invalid escape sequence: \\'%c'", c);
					}
				}
			} else { // read valid non-ascii (utf8 chars)
				unput(c);
				readNonAsciiChar(&buffer, &index, &size, driver);
			}
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static char* readComment(Driver * driver) {
	char* buffer = malloc(64 * sizeof(char));
	size_t size = 64;
	size_t index = 0;
	char c;

	while((c = input()) != '\n') { // terminating ' character
		if (c == 0x09 || (c >= 0x20 && c <= 0x7E)) { // read valid ascii chars
			appendChar(&buffer, &index, &size, c);
		} else { // read valid non-ascii (utf8 chars)
			unput(c);
			readNonAsciiChar(&buffer, &index, &size, driver);
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static void appendChar(char** buffer, size_t* index, size_t* len, char c) {
	(*buffer)[*index] = c;
	*index = *index + 1;
	if (*index == *len) {
		*len *= 2;
		*buffer = realloc(*buffer, *len);
	}
}

static int isHex(char c) {
	return (c >= '0' && c <= '9') ||
		(c >= 'a' && c <= 'f') ||
		(c >= 'A' && c <= 'F');
}

static void readHex(char** buffer, size_t* index, size_t* len, size_t count, Driver * driver) {
	for (size_t i = 0; i < count; i++) {
		char c = input();
		if (!isHex(c)) {
            driverError (driver, yylineno, "Malformed Input: Invalid unicode escape char, expected hex char, got : '%c'", c);
		}
		appendChar(buffer, index, len, c);
	}
}


static void readNonAsciiChar(char** buffer, size_t* index, size_t* len, Driver * driver) {
	unsigned char c = (unsigned char) input();
	size_t utfLen;

	if (c >= 0xF0 && c < 0xF8) { // 11110xxx
		utfLen = 4;
	} else if (c >= 0xE0 && c < 0xF0) { // 1110xxxx
		utfLen = 3;
	} else if (c >= 0xC0 && c < 0xE0) { // 110xxxxx
		utfLen = 2;
	} else if (c < 0x80) {                        // 0xxxxxxx
        driverError(driver, yylineno, "Malformed Input: Expected non-ascii char, but found ascii char: '%c' (0x%02X)", c, c);
        // assert (0); // Unexpected ascii char (can happen on malformed user input)
	} else {
        driverError (driver, yylineno, "Malformed Input: Invalid utf-8 sequence");
	}
	appendChar(buffer, index, len, c);
	for (size_t i = 1; i < utfLen; i++) {
		c = input();
		if (c < 0x80 || c >= 0xC0) {
            driverError (driver, yylineno, "Malformed Input: Invalid utf-8 sequence");
		}
		appendChar(buffer, index, len, c);
	}
}
