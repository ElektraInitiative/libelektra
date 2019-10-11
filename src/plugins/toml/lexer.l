%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "parser.h"
#include "scalar.h"

static char* readLiteral(void);
static char* readBasic(void);
static char* readMultilineLiteral(void);
static char* readMultilineBasic(void);
static char* readComment(void);

static void appendChar(char**, size_t*, size_t*, char);
static void readNonAsciiChar(char**, size_t*, size_t*);
static void readHex(char**, size_t*, size_t*, size_t);
static int isHex(char);

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

full_date {digit}{4,4}"-"{digit}{2,2}"-"{digit}{2,2}
full_time {partial_time}{time_offset}
partial_time {digit}{2,2}":"{digit}{2,2}":"{digit}{2,2}("."{digit}+)?
time_offset "Z"|{time_num_offset}
time_num_offset [+-]{digit}{2,2}":"{digit}{2,2}

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
	char* str = readComment();
	unput('\n');
	yylval.scalar = createScalar(SCALAR_STRING_COMMENT, str, yylineno);
	return COMMENT;
}

"'" {
	char* str = readLiteral();
	yylval.scalar = createScalar(SCALAR_STRING_LITERAL, str, yylineno);
	return LITERAL_STRING;
}

"'''" {
	int lineStart = yylineno;
	char* str = readMultilineLiteral();
	yylval.scalar = createScalar(SCALAR_STRING_ML_LITERAL, str, lineStart);
	return MULTI_LITERAL_STRING;
}

"\"" {
	int lineStart = yylineno;
	char* str = readBasic();
	yylval.scalar = createScalar(SCALAR_STRING_BASIC, str, lineStart);
	return BASIC_STRING;
}

"\"\"\"" {
	int lineStart = yylineno;
	char* str = readMultilineBasic();
	yylval.scalar = createScalar(SCALAR_STRING_ML_BASIC, str, lineStart);
	return MULTI_BASIC_STRING;
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

<RHS,IN_BRACKETS>{offset_datetime} {
	yylval.scalar = createScalarDup(SCALAR_DATE_OFFSET_DATETIME, yytext, yylineno);
	return OFFSET_DATETIME;
}
<RHS,IN_BRACKETS>{local_datetime} {
	yylval.scalar = createScalarDup(SCALAR_DATE_LOCAL_DATETIME, yytext, yylineno);
	return LOCAL_DATETIME;
}
<RHS,IN_BRACKETS>{local_date} {
	yylval.scalar = createScalarDup(SCALAR_DATE_LOCAL_DATE, yytext, yylineno);
	return LOCAL_DATE;
}
<RHS,IN_BRACKETS>{local_time} {
	yylval.scalar = createScalarDup(SCALAR_DATE_LOCAL_TIME, yytext, yylineno);
	return LOCAL_TIME;
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
	printf("[LEX_ERROR] No newline in inline table allowed, but found newline\n");
	exit(1);
}

{whitespace} ;

. {
	printf("[LEX_ERROR] Unexpected sequence: %s\n", yytext);
	exit(1);
}

%%

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
	// printf("[LEX] push @ '%s' -> %d\n", yytext, YY_START);
}

static void popCondition(void) {
	if (stackRoot == NULL) {
		printf("[LEX_ERROR] Wanted to pop start condition, but stack is empty\n");
		exit(1);
	} else {
		stackRoot = stackRoot->next;
		if (stackRoot == NULL) {
			BEGIN(INITIAL);
		} else {
			BEGIN(stackRoot->condition);
		}
	}
	// printf("[LEX] pop @ '%s' -> %d\n", yytext, YY_START);
}

static void popConditionIf(int condition) {
	if (YY_START == condition) {
		popCondition();
	}
}

static char* readLiteral(void) {
	char* buffer = malloc(64 * sizeof(char));
	size_t size = 64;
	size_t index = 0;
	char c;

	while((c = input()) != 0x27) { // terminating ' character
		if (c == 0x09 || (c >= 0x20 && c <= 0x7E)) { // read valid ascii chars
			appendChar(&buffer, &index, &size, c);
		} else { // read valid non-ascii (utf8 chars)
			unput(c);
			readNonAsciiChar(&buffer, &index, &size);
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static char* readBasic(void) {
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
						readHex(&buffer, &index, &size, 4);
						break;
					case 0x55:
						appendChar(&buffer, &index, &size, c);
						readHex(&buffer, &index, &size, 8);
						break;
					default:
						printf("[LEX ERROR] invalid escape char: %c\n", c);
						exit(1);
				}
			}
		} else { // read valid non-ascii (utf8 chars)
			unput(c);
			readNonAsciiChar(&buffer, &index, &size);
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static char* readMultilineLiteral(void) {
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
				readNonAsciiChar(&buffer, &index, &size);
			}
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static char* readMultilineBasic(void) {
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
							readHex(&buffer, &index, &size, 4);
							break;
						case 0x55:
							appendChar(&buffer, &index, &size, c);
							readHex(&buffer, &index, &size, 8);
							break;
						default:
							printf("[LEX ERROR] invalid escape char: 0x%02X\n", c);
							exit(1);
					}
				}
			} else { // read valid non-ascii (utf8 chars)
				unput(c);
				readNonAsciiChar(&buffer, &index, &size);
			}
		}
	}
	buffer = realloc(buffer, index + 1);
    buffer[index] = 0;
	return buffer;
}

static char* readComment(void) {
	char* buffer = malloc(64 * sizeof(char));
	size_t size = 64;
	size_t index = 0;
	char c;

	while((c = input()) != '\n') { // terminating ' character
		if (c == 0x09 || (c >= 0x20 && c <= 0x7E)) { // read valid ascii chars
			appendChar(&buffer, &index, &size, c);
		} else { // read valid non-ascii (utf8 chars)
			unput(c);
			readNonAsciiChar(&buffer, &index, &size);
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

static void readHex(char** buffer, size_t* index, size_t* len, size_t count) {
	for (size_t i = 0; i < count; i++) {
		char c = input();
		if (!isHex(c)) {
			printf("[LEX ERROR] invalid unicode escape: expected hex character, got %c\n", c);
			exit(1);
		}
		appendChar(buffer, index, len, c);
	}
}


static void readNonAsciiChar(char** buffer, size_t* index, size_t* len) {
	unsigned char c = (unsigned char) input();
	size_t utfLen;

	if (c >= 0xF0 && c < 0xF8) { // 11110xxx
		utfLen = 4;
	} else if (c >= 0xE0 && c < 0xF0) { // 1110xxxx
		utfLen = 3;
	} else if (c >= 0xC0 && c < 0xE0) { // 110xxxxx
		utfLen = 2;
	} else if (c < 0x80) {                        // 0xxxxxxx
		// is ascii char
		printf("[LEX ERROR] utf8: unexpected char: ascii: %c, 0x%02X\n", c, c & 0xFF);
		exit(1);
	} else {
		printf("[LEX ERROR] utf8: invalid first byte: 0x%X", c);
		exit(1);
	}
	appendChar(buffer, index, len, c);
	for (size_t i = 1; i < utfLen; i++) {
		c = input();
		if (c < 0x80 || c >= 0xC0) {
			printf("[LEX ERROR] utf8: invalid byte #%lu: 0x%X\n", i, c);
			exit(1);
		}
		appendChar(buffer, index, len, c);
	}
}
