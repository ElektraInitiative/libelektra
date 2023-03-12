%{
/**
 * @file lexer.l
 *
 * @brief Contains the flex lexer.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "codepoint.h"
#include "driver.h"
#include "error.h"
#include "parser.h"
#include "scalar.h"
#include "utility.h"
#include "type.h"
#include "integer.h"

#define BASIC_TERMINATOR '\"'
#define LITERAL_TERMINATOR '\''
#define COMMENT_TERMINATOR '\n'
#define ESCAPE_CHAR '\\'
#define NORMAL_TERMINATOR_COUNT 1
#define MULTILINE_TERMINATOR_COUNT 3

#define YY_NO_INPUT
#define YY_NO_UNPUT

#define YY_USER_ACTION \
    yylloc->first_line = yylloc->last_line; \
    yylloc->first_column = yylloc->last_column; \
    for(int i = 0; yytext[i] != '\0'; i++) { \
        if(yytext[i] == '\n') { \
            yylloc->last_line++; \
            yylloc->last_column = 0; \
        } \
        else { \
            yylloc->last_column++; \
        } \
    }

typedef struct
{
	char * data;
	size_t index;
	size_t size;
} Buffer;

typedef struct
{
	Buffer * stringBuffer;
	Buffer * origBuffer;
} ExtraData;

#define YY_EXTRA_TYPE ExtraData *

static Buffer * bufferNew (const char * initial, size_t initialSize, Driver * driver);
static char * bufferConsume (Buffer * buffer, int lineno, Driver * driver);
static void bufferAddChar (Buffer * buffer, char c, Driver * driver);
static void bufferAddString (Buffer * buffer, const char * string, size_t stringLen, Driver * driver);
static void bufferAddEscaped (Buffer * buffer, char escapeCode, Driver * driver);
static void bufferAddUnicode (Buffer * buffer, const char * hexCode, int codeLength, Driver * driver);

#if YYDEBUG
#define printDebug(...) printf(__VA_ARGS__)
#else
#define printDebug(...)
#endif

#define YY_PUSH(x, scanner) printDebug("(%d:%d-%d:%d) PUSH %d->%d %s\n", yylloc.first_line, yylloc.first_column, yylloc.last_line, yylloc.last_column - 1, YYSTATE, x, yytext); yy_push_state(x, scanner)
#define YY_POP(scanner) printDebug("(%d:%d-%d:%d) POP %d->%d %s\n", yylloc.first_line, yylloc.first_column, yylloc.last_line, yylloc.last_column - 1, YYSTATE, yy_top_state(scanner), yytext); yy_pop_state(scanner); 

%}

%option noyywrap
%option stack
%option reentrant
%option bison-bridge
%option bison-locations

%s MODE_VALUE
%s MODE_INLINE_TABLE
%s MODE_ARRAY

%x MODE_BASIC_STRING
%x MODE_ML_BASIC_STRING
%x MODE_ML_LITERAL_STRING

alpha [a-zA-Z]
digit [0-9]
alnum {alpha}|{digit}

whitespace_char [\t ]
whitespace {whitespace_char}*
newline (\r\n)|\n

escape_char \\

dec_char {digit}
dec_nzero [1-9]

hex_prefix 0x
oct_prefix 0o
bin_prefix 0b

hex_char [0-9a-fA-F]
oct_char [0-8]
bin_char [01]

double_quote \"
single_quote [']

bare_char {alnum}|-|_
literal_char \t|[^'\n]

mlb_escaped_nl {escape_char}{whitespace}{newline}({whitespace}|{newline})*

basic_unescaped [^"\\\n]
basic_escaped {escape_char}[bfnrt\\"]
uni4_escape {escape_char}[uU]{hex_char}{4}
uni8_escape {escape_char}[uU]{hex_char}{8}

decimal {dec_nzero}("_"?{dec_char})*
decimal_leading_zeros {dec_char}("_"?{dec_char})*
decimal_pm [+-]?{decimal}
decimal_pm_leading_zeros [+-]?{decimal_leading_zeros}

offset_datetime {full_date}[T ]{full_time}
local_datetime	{full_date}[T ]{partial_time}
local_date {full_date}
local_time {partial_time}

full_date {digit}{4}"-"{digit}{2}"-"{digit}{2}
full_time {partial_time}{time_offset}
partial_time {digit}{2}":"{digit}{2}":"{digit}{2}("."{digit}+)?
time_offset "Z"|{time_num_offset}
time_num_offset [+-]{digit}{2}":"{digit}{2}

%%

"="			{ YY_PUSH(MODE_VALUE, yyscanner); return EQUAL; }
"."			return DOT;
<MODE_VALUE>","		{
	YY_POP(yyscanner);
	if (YYSTATE == MODE_ARRAY)
	{
		YY_PUSH(MODE_VALUE, yyscanner);
	}
	return COMMA;
}
<MODE_VALUE>"["		{ YY_PUSH(MODE_ARRAY, yyscanner); YY_PUSH(MODE_VALUE, yyscanner); return BRACKETS_OPEN; }
<INITIAL>"["		return BRACKETS_OPEN;
<MODE_VALUE>"]"		{ YY_POP(yyscanner); YY_POP(yyscanner); return BRACKETS_CLOSE; }
<INITIAL>"]"		return BRACKETS_CLOSE;
<MODE_VALUE>"{"		{ YY_PUSH(MODE_INLINE_TABLE, yyscanner); return CURLY_OPEN; }
<MODE_VALUE>"}"		{ YY_POP(yyscanner); YY_POP(yyscanner); return CURLY_CLOSE; }
<MODE_INLINE_TABLE>"}"	{ YY_POP(yyscanner); return CURLY_CLOSE; }
{newline}		{
	if (YYSTATE != INITIAL && yy_top_state(yyscanner) != MODE_ARRAY)
	{
		YY_POP(yyscanner);
	}
	return NEWLINE;
}

{double_quote}{basic_unescaped}*	{
	/* a new buffer for the string and switch to string mode */
	elektraFree (yyget_extra(yyscanner));
	ExtraData * extra = elektraMalloc (sizeof(ExtraData));
	extra->stringBuffer = bufferNew(yytext + 1, yyleng - 1, driver);
	extra->origBuffer = bufferNew(yytext + 1, yyleng - 1, driver);
	yyset_extra (extra, yyscanner);
	YY_PUSH (MODE_BASIC_STRING, yyscanner);
}

<MODE_BASIC_STRING>{
	{basic_unescaped}+		{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{basic_escaped}			{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddEscaped (extra->stringBuffer, yytext[1], driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{uni4_escape}|{uni8_escape}	{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddUnicode (extra->stringBuffer, yytext + 2, yyleng - 2, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}			{
		ExtraData * extra = yyget_extra (yyscanner);
		lval->scalar = createScalar (
			SCALAR_STRING_BASIC,
			bufferConsume(extra->stringBuffer, yylineno, driver),
			bufferConsume(extra->origBuffer, yylineno, driver),
			yylineno
		);
		YY_POP(yyscanner);
		return BASIC_STRING;
	}
	{newline}			{
		driverError (driver, ERROR_SYNTACTIC, yylineno,
			"Malformed Input: Found newline while reading a non-multiline string, but newline is only allowed in "
			"multiline strings.");
	}
	<<EOF>>				{
		driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed Input: Found unterminated string at end of file.");
	}

	.|\n {
		driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed input: Unexpected sequence: '%s' %d", yytext, yy_top_state(yyscanner));
	}
}

{double_quote}{3}{newline}?		{
	/* a new buffer for the string and switch to string mode */
	elektraFree (yyget_extra(yyscanner));
	ExtraData * extra = elektraMalloc (sizeof(ExtraData));
	extra->stringBuffer = bufferNew("", 0, driver);
	extra->origBuffer = bufferNew(yytext + 3, yyleng - 3, driver);
	yyset_extra (extra, yyscanner);
	YY_PUSH (MODE_ML_BASIC_STRING, yyscanner);
}

<MODE_ML_BASIC_STRING>{
	({basic_unescaped}|{newline})+		{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{basic_escaped}				{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddEscaped (extra->stringBuffer, yytext[1], driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{uni4_escape}|{uni8_escape}		{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddUnicode (extra->stringBuffer, yytext + 2, yyleng - 2, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}{1,2}({basic_unescaped})	{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}{1,2}({basic_escaped})	{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng - 2, driver);
		bufferAddEscaped (extra->stringBuffer, yytext[yyleng - 2], driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}{1,2}({uni4_escape})	{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng - 6, driver);
		bufferAddUnicode (extra->stringBuffer, yytext + yyleng - 4, 4, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}{1,2}({uni8_escape})	{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng - 10, driver);
		bufferAddUnicode (extra->stringBuffer, yytext + yyleng - 8, 8, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}{1,2}({newline})		{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng - 10, driver);
		bufferAddChar (extra->stringBuffer, '\n', driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}({mlb_escaped_nl})	{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddChar (extra->stringBuffer, '"', driver);
		bufferAddChar (extra->stringBuffer, ' ', driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}{2}({mlb_escaped_nl})	{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddChar (extra->stringBuffer, '"', driver);
		bufferAddChar (extra->stringBuffer, '"', driver);
		bufferAddChar (extra->stringBuffer, ' ', driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{mlb_escaped_nl}			{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{double_quote}{3}			{
		ExtraData * extra = yyget_extra (yyscanner);
		lval->scalar = createScalar (
			SCALAR_STRING_ML_BASIC,
			bufferConsume(extra->stringBuffer, yylineno, driver),
			bufferConsume(extra->origBuffer, yylineno, driver),
			yylineno
		);
		YY_POP(yyscanner);
		return MULTI_BASIC_STRING;
	}
	<<EOF>>					{
		driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed Input: Found unterminated string at end of file.");
	}
	.|\n {
		driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed input: Unexpected sequence: '%s' %d", yytext, yy_top_state(yyscanner));
	}
}

{single_quote}{literal_char}*{single_quote}	{
	lval->scalar = createScalar (SCALAR_STRING_LITERAL, strndup (yytext + 1, yyleng - 2), strndup (yytext + 1, yyleng - 2), yylineno);
	return LITERAL_STRING;
}

{single_quote}{3}{newline}?	{
	/* a new buffer for the string and switch to string mode */
	elektraFree (yyget_extra(yyscanner));
	ExtraData * extra = elektraMalloc (sizeof(ExtraData));
	extra->stringBuffer = bufferNew("", 0, driver);
	extra->origBuffer = bufferNew(yytext + 3, yyleng - 3, driver);
	yyset_extra (extra, yyscanner);
	YY_PUSH (MODE_ML_LITERAL_STRING, yyscanner);
}

<MODE_ML_LITERAL_STRING>{
	({literal_char}|{newline})+			{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{single_quote}{1,2}({literal_char}|{newline})	{
		ExtraData * extra = yyget_extra (yyscanner);
		bufferAddString (extra->stringBuffer, yytext, yyleng, driver);
		bufferAddString (extra->origBuffer, yytext, yyleng, driver);
	}
	{single_quote}{3}				{
		ExtraData * extra = yyget_extra (yyscanner);
		lval->scalar = createScalar (
			SCALAR_STRING_ML_LITERAL,
			bufferConsume(extra->stringBuffer, yylineno, driver),
			bufferConsume(extra->origBuffer, yylineno, driver),
			yylineno
		);
		YY_POP(yyscanner);
		return MULTI_LITERAL_STRING;
	}
	<<EOF>>						{
		driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed Input: Found unterminated string at end of file.");
	}

	.|\n {
		driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed input: Unexpected sequence: '%s' %d", yytext, yy_top_state(yyscanner));
	}
}

{whitespace}"#"([\t\x20-\xFF])* {
	lval->scalar = createScalarDup (SCALAR_STRING_COMMENT, strchr(yytext, '#') + 1, yytext, yylineno);
	return COMMENT;
}

<INITIAL,MODE_INLINE_TABLE>{bare_char}+ {
	lval->scalar = createScalarDup (SCALAR_STRING_BARE, yytext, yytext, yylineno);
	return BARE_STRING;
}

{offset_datetime} {
	lval->scalar = createScalarDup (SCALAR_DATE_OFFSET_DATETIME, yytext, yytext, yylineno);
	return OFFSET_DATETIME;
}
{local_datetime} {
	lval->scalar = createScalarDup (SCALAR_DATE_LOCAL_DATETIME, yytext, yytext, yylineno);
	return LOCAL_DATETIME;
}
{local_date} {
	lval->scalar = createScalarDup (SCALAR_DATE_LOCAL_DATE, yytext, yytext, yylineno);
	return LOCAL_DATE;
}
{local_time} {
	lval->scalar = createScalarDup (SCALAR_DATE_LOCAL_TIME, yytext, yytext, yylineno);
	return LOCAL_TIME;
}

{decimal_pm} {
	if (isValidInteger (yytext, 10))
	{
		lval->scalar = createScalarDup (SCALAR_INTEGER_DEC, yytext, yytext, yylineno);
	}
	else
	{
		lval->scalar = NULL;
		driverError (driver, ERROR_SEMANTIC, yylineno,
			"Found decimal number that is too big or small, must be in range [%lld, %lld], but found %s", LLONG_MIN,
			LLONG_MAX, yytext);
	}
	return DECIMAL;
}

("+"|"-")?"0" {
	lval->scalar = createScalarDup (SCALAR_INTEGER_DEC, yytext, yytext, yylineno);
	return DECIMAL;
}

"0x"{hex_char}("_"?{hex_char})* {
	if (isValidInteger (yytext, 16))
	{
		lval->scalar = createScalarDup (SCALAR_INTEGER_HEX, yytext, yytext, yylineno);
	}
	else
	{
		lval->scalar = NULL;
		driverError (driver, ERROR_SEMANTIC, yylineno, "Found hexadecimal number that is too big, maximum is 0x%llX, but found %s", ULLONG_MAX, yytext);
	}
	return HEXADECIMAL;
}

"0o"{oct_char}("_"?{oct_char})* {
	if (isValidInteger (yytext, 8))
	{
		lval->scalar = createScalarDup (SCALAR_INTEGER_OCT, yytext, yytext, yylineno);
	}
	else
	{
		lval->scalar = NULL;
		driverError (driver, ERROR_SEMANTIC, yylineno, "Found octal number that is too big, maximum is 0o%llo, but found %s", ULLONG_MAX, yytext);
	}
	return OCTAL;
}

"0b"{bin_char}("_"?{bin_char})* {
	if (isValidInteger (yytext, 2))
	{
		lval->scalar = createScalarDup (SCALAR_INTEGER_BIN, yytext, yytext, yylineno);
	}
	else
	{
		lval->scalar = NULL;
		driverError (driver, ERROR_SEMANTIC, yylineno, "Found binary number that is too big, maximum is 64 bits, but found %s", yytext);
	}
	return BINARY;
}

<MODE_VALUE>{decimal_pm}("."{decimal_leading_zeros})?([eE]{decimal_pm_leading_zeros})?  {
	lval->scalar = createScalarDup (SCALAR_FLOAT_NUM, yytext, yytext, yylineno);
	return FLOAT;
}

<MODE_VALUE>("+"|"-")?"0"("."{decimal_leading_zeros})?([eE]{decimal_pm_leading_zeros})?  {
	lval->scalar = createScalarDup (SCALAR_FLOAT_NUM, yytext, yytext, yylineno);
	return FLOAT;
}

("+"|"-")?"inf" {
	lval->scalar = createScalarDup (SCALAR_FLOAT_NUM, yytext, yytext, yylineno);
	switch (yytext[0])
	{
	case '+':
		lval->scalar->type = SCALAR_FLOAT_POS_INF;
		break;
	case '-':
		lval->scalar->type = SCALAR_FLOAT_NEG_INF;
		break;
	default:
		lval->scalar->type = SCALAR_FLOAT_INF;
		break;
	}
	return FLOAT;
}

("+"|"-")?"nan" {
	lval->scalar = createScalarDup (SCALAR_FLOAT_NUM, yytext, yytext, yylineno);
	switch (yytext[0])
	{
	case '+':
		lval->scalar->type = SCALAR_FLOAT_POS_NAN;
		break;
	case '-':
		lval->scalar->type = SCALAR_FLOAT_NEG_NAN;
		break;
	default:
		lval->scalar->type = SCALAR_FLOAT_NAN;
		break;
	}
	return FLOAT;
}

"true"|"false" {
	lval->scalar = createScalarDup (SCALAR_BOOLEAN, yytext, yytext, yylineno);
	return BOOLEAN;
}

{whitespace} {}

.|\n {
	driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed input: Unexpected sequence: '%s' %d", yytext, yy_top_state(yyscanner));
}

%%

void initializeLexer (FILE * file, yyscan_t * scanner)
{
	yylex_init_extra (NULL, scanner);
	YY_PUSH (INITIAL, *scanner);
	yyrestart (file, *scanner);
}

void clearLexer (yyscan_t scanner)
{
	elektraFree (yyget_extra (scanner));
	yylex_destroy (scanner);
}

static Buffer * bufferNew (const char * initial, size_t initialLen, Driver * driver)
{
	Buffer * buffer = (Buffer *) elektraCalloc (sizeof (Buffer));
	if (buffer == NULL)
	{
		driverErrorGeneric (driver, ERROR_MEMORY, "bufferNew", "elektraCalloc");
		return NULL;
	}
	buffer->size = 64;
	while (buffer->size < initialLen)
	{
		buffer->size <<= 2;
	}
	buffer->data = (char *) elektraCalloc (sizeof (char) * buffer->size);
	if (buffer->data == NULL)
	{
		driverErrorGeneric (driver, ERROR_MEMORY, "bufferNew", "elektraCalloc");
		elektraFree (buffer);
		return NULL;
	}
	strncpy (buffer->data, initial, initialLen);
	buffer->index = initialLen;
	return buffer;
}

static char * bufferConsume (Buffer * buffer, int lineno, Driver * driver)
{
	if (buffer != NULL)
	{
		char * data = buffer->data;
		size_t size = buffer->size;
		buffer->data = NULL;
		buffer->index = 0;
		buffer->size = 0;
		elektraFree (buffer);

		if (!isValidUtf8 ((uint8_t*) data, size))
		{
			driverError (driver, ERROR_SYNTACTIC, lineno, "Malformed Input: Detected invalid UTF-8.");
			elektraFree (data);
			return NULL;
		}

		return data;
	}
	else
	{
		return NULL;
	}
}

static bool bufferEnsureSize (Buffer * buffer, size_t minSize, Driver * driver)
{
	size_t oldSize = buffer->size;
	while (buffer->size < minSize)
	{
		buffer->size <<= 2;
	}

	if (elektraRealloc ((void **) &buffer->data, buffer->size) < 0)
	{
		driverErrorGeneric (driver, ERROR_MEMORY, "bufferAddChar", "elektraRealloc");
		buffer->size = oldSize;
		return false;
	}

	memset (buffer->data + buffer->index, 0, buffer->size - buffer->index);
	return true;
}

static void bufferAddChar (Buffer * buffer, char c, Driver * driver)
{
	ELEKTRA_ASSERT (buffer != NULL, "Buffer expected to be non-NULL, but was NULL");
	ELEKTRA_ASSERT (driver != NULL, "Driver expected to be non-NULL, but was NULL");

	if (bufferEnsureSize (buffer, buffer->index + 1, driver))
	{
		buffer->data[buffer->index++] = c;
	}
}

static void bufferAddString (Buffer * buffer, const char * string, size_t stringLen, Driver * driver)
{
	ELEKTRA_ASSERT (buffer != NULL, "Buffer expected to be non-NULL, but was NULL");
	ELEKTRA_ASSERT (driver != NULL, "Driver expected to be non-NULL, but was NULL");

	if (bufferEnsureSize (buffer, buffer->index + stringLen + 1, driver))
	{
		strncpy (buffer->data + buffer->index, string, stringLen);
		buffer->index += stringLen;
	}
}

static void bufferAddEscaped (Buffer * buffer, char escapeCode, Driver * driver)
{
	ELEKTRA_ASSERT (buffer != NULL, "Buffer expected to be non-NULL, but was NULL");
	ELEKTRA_ASSERT (driver != NULL, "Driver expected to be non-NULL, but was NULL");

	if (bufferEnsureSize (buffer, buffer->index + 1, driver))
	{
		char c;
		switch (escapeCode)
		{
			case 'b':
				c = '\b';
				break;
			case 'f':
				c = '\f';
				break;
			case 'n':
				c = '\n';
				break;
			case 'r':
				c = '\r';
				break;
			case 't':
				c = '\t';
				break;
			case '"':
			case '\\':
				c = escapeCode;
				break;
			default:
				ELEKTRA_ASSERT (0, "should be unreachable");
				return;
		}
		buffer->data[buffer->index++] = c;
	}
}

static void bufferAddUnicode (Buffer * buffer, const char * hexCode, int codeLength, Driver * driver)
{
	ELEKTRA_ASSERT (buffer != NULL, "Buffer expected to be non-NULL, but was NULL");
	ELEKTRA_ASSERT (driver != NULL, "Driver expected to be non-NULL, but was NULL");
	ELEKTRA_ASSERT (codeLength == 4 || codeLength == 8, "Invalid Unicode codeLength");

	char utf8[5];
	int len = utf8FromUnicode (hexCode, codeLength, utf8);
	utf8[len] = '\0';

	if (bufferEnsureSize (buffer, buffer->index + len + 1, driver))
	{
		strncpy (buffer->data + buffer->index, utf8, len);
		buffer->index += len;
	}
}
