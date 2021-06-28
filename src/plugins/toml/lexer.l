/**
 * @file lexer.l
 *
 * @brief Contains the flex lexer.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

%{
#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codepoint.h"
#include "driver.h"
#include "error.h"
#include "parser.h"
#include "scalar.h"
#include "utility.h"

#define BASIC_TERMINATOR '\"'
#define LITERAL_TERMINATOR '\''
#define COMMENT_TERMINATOR '\n'
#define ESCAPE_CHAR '\\'
#define NORMAL_TERMINATOR_COUNT 1
#define MULTILINE_TERMINATOR_COUNT 3

typedef struct
{
	char * data;
	size_t index;
	size_t size;
} Buffer;

static char * readString (char terminatorChar, bool isMultiline, Driver * driver);

static Buffer * bufferNew (void);
static char * bufferConsume (Buffer * buffer);
static void bufferAddChar (Buffer * buffer, char c, Driver * driver);

static void handleEscapedSequence (Buffer * buffer, bool isMultiline, Driver * driver);
static void handlePossibleLineEndingBackslash (Buffer * buffer, Driver * driver);
static void readHex (Buffer * buffer, size_t count, Driver * driver);
static void readNonAsciiChar (Buffer * buffer, Driver * driver);
static size_t whitespaceToSpaces (const char * whitespace);

static inline bool validAscii (char c);
static inline bool validHex (char c);
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
decimal_leading_zeros {dec_char}("_"?{dec_char})*
decimal_pm [+-]?{decimal}
decimal_pm_leading_zeros [+-]?{decimal_leading_zeros}

bare_char [-_]|{alnum}

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

"="		return EQUAL;
"."		return DOT;
","		return COMMA;
"["		return BRACKETS_OPEN;
"]"		return BRACKETS_CLOSE;
"{"		return CURLY_OPEN;
"}"		return CURLY_CLOSE;
{newline}	return NEWLINE;

{whitespace}*"#" {
	char * str = readString (COMMENT_TERMINATOR, false, driver);
	unput ('\n');
	size_t spaces = whitespaceToSpaces (yytext);
	yylval.scalar = createScalarComment (str, spaces, yylineno);
	return COMMENT;
}

"'" {
	char * str = readString (LITERAL_TERMINATOR, false, driver);
	yylval.scalar = createScalar (SCALAR_STRING_LITERAL, str, yylineno);
	return LITERAL_STRING;
}

"'''" {
	int lineStart = yylineno;
	char * str = readString (LITERAL_TERMINATOR, true, driver);
	yylval.scalar = createScalar (SCALAR_STRING_ML_LITERAL, str, lineStart);
	return MULTI_LITERAL_STRING;
}

"\"" {
	char * str = readString(BASIC_TERMINATOR, false, driver);
	yylval.scalar = createScalar (SCALAR_STRING_BASIC, str, yylineno);
	return BASIC_STRING;
}

"\"\"\"" {
	int lineStart = yylineno;
	char * str = readString(BASIC_TERMINATOR, true, driver);
	yylval.scalar = createScalar (SCALAR_STRING_ML_BASIC, str, lineStart);
	return MULTI_BASIC_STRING;
}

{offset_datetime} {
	yylval.scalar = createScalarDup (SCALAR_DATE_OFFSET_DATETIME, yytext, yylineno);
	return OFFSET_DATETIME;
}
{local_datetime} {
	yylval.scalar = createScalarDup (SCALAR_DATE_LOCAL_DATETIME, yytext, yylineno);
	return LOCAL_DATETIME;
}
{local_date} {
	yylval.scalar = createScalarDup (SCALAR_DATE_LOCAL_DATE, yytext, yylineno);
	return LOCAL_DATE;
}
{local_time} {
	yylval.scalar = createScalarDup (SCALAR_DATE_LOCAL_TIME, yytext, yylineno);
	return LOCAL_TIME;
}

{decimal_pm} {
	yylval.scalar = createScalarDup (SCALAR_INTEGER_DEC, yytext, yylineno);
	return DECIMAL;
}

("+"|"-")?"0" {
	yylval.scalar = createScalarDup (SCALAR_INTEGER_DEC, yytext, yylineno);
	return DECIMAL;
}

"0x"{hex_char}("_"?{hex_char})* {
	yylval.scalar = createScalarDup (SCALAR_INTEGER_HEX, yytext, yylineno);
	return HEXADECIMAL;
}

"0o"{oct_char}("_"?{oct_char})* {
	yylval.scalar = createScalarDup (SCALAR_INTEGER_OCT, yytext, yylineno);
	return OCTAL;
}

"0b"{bin_char}("_"?{bin_char})* {
	yylval.scalar = createScalarDup (SCALAR_INTEGER_BIN, yytext, yylineno);
	return BINARY;
}

{decimal_pm}("."{decimal_leading_zeros})?([eE]{decimal_pm_leading_zeros})?  {
	yylval.scalar = createScalarDup (SCALAR_FLOAT_NUM, yytext, yylineno);
	return FLOAT;
}

("+"|"-")?"0"("."{decimal_leading_zeros})?([eE]{decimal_pm_leading_zeros})?  {
	yylval.scalar = createScalarDup (SCALAR_FLOAT_NUM, yytext, yylineno);
	return FLOAT;
}

("+"|"-")?"inf" {
	yylval.scalar = createScalarDup (SCALAR_FLOAT_NUM, yytext, yylineno);
	switch (yytext[0])
	{
	case '+':
		yylval.scalar->type = SCALAR_FLOAT_POS_INF;
		break;
	case '-':
		yylval.scalar->type = SCALAR_FLOAT_NEG_INF;
		break;
	default:
		yylval.scalar->type = SCALAR_FLOAT_INF;
		break;
	}
	return FLOAT;
}
("+"|"-")?"nan" {
	yylval.scalar = createScalarDup (SCALAR_FLOAT_NUM, yytext, yylineno);
	switch (yytext[0])
	{
	case '+':
		yylval.scalar->type = SCALAR_FLOAT_POS_NAN;
		break;
	case '-':
		yylval.scalar->type = SCALAR_FLOAT_NEG_NAN;
		break;
	default:
		yylval.scalar->type = SCALAR_FLOAT_NAN;
		break;
	}
	return FLOAT;
}

"true"|"false" {
	yylval.scalar = createScalarDup (SCALAR_BOOLEAN, yytext, yylineno);
	return BOOLEAN;
}

{bare_char}+ {
	yylval.scalar = createScalarDup (SCALAR_STRING_BARE, yytext, yylineno);
	return BARE_STRING;
}

{whitespace}* {}

.  {
	driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed input: Unexpected sequence: '%s'", yytext);
}

%%

void initializeLexer (FILE * file)
{
	yylineno = 1;
	BEGIN (0);
	yyrestart (file);
	// yy_flush_buffer(YY_CURRENT_BUFFER);
	// yyin = file;
}

void clearLexer (void)
{
	// yy_delete_buffer(YY_CURRENT_BUFFER);
	yylex_destroy ();
}

static inline bool validAscii (char c)
{
	return c == '\t' || (c >= 0x20 && c <= 0x7E);
}

static inline bool validHex (char c)
{
	return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static char * readString (char terminatorChar, bool isMultiline, Driver * driver)
{
	Buffer * buffer = bufferNew ();
	if (buffer == NULL)
	{
		return NULL;
	}
	int terminatorCount = 0;
	int terminatorMax = isMultiline ? 3 : 1;
	bool hasEscaping = terminatorChar == BASIC_TERMINATOR;

	if (terminatorChar == BASIC_TERMINATOR || terminatorChar == LITERAL_TERMINATOR)
	{
		for (int i = 0; i < terminatorMax; i++)
		{
			bufferAddChar (buffer, terminatorChar, driver);
		}
	}

	while (terminatorCount < terminatorMax)
	{
		char c = (char) input ();
		if (c == terminatorChar)
		{
			if (terminatorChar == BASIC_TERMINATOR || terminatorChar == LITERAL_TERMINATOR)
			{
				bufferAddChar (buffer, c, driver);
			}
			terminatorCount++;
		}
		else if (!isMultiline && c == '\n')
		{
			driverError (driver, ERROR_SYNTACTIC, yylineno,
				     "Malformed Input: Found newline while reading a non-multiline string, but newline is only allowed in "
				     "multiline strings.");
			return bufferConsume (buffer);
		}
		else if (c == 0x00)
		{
			driverError (driver, ERROR_SYNTACTIC, yylineno,
				     "Malformed Input: Found end of file while reading string, expected to find closing string quotes.");
			return bufferConsume (buffer);
		}
		else if (validAscii (c))
		{
			bufferAddChar (buffer, c, driver);
			if (c == ESCAPE_CHAR)
			{
				if (hasEscaping)
				{
					handleEscapedSequence (buffer, isMultiline, driver);
				}
				else if (isMultiline)
				{
					handlePossibleLineEndingBackslash (buffer, driver);
				}
			}
		}
		else
		{
			unput (c);
			readNonAsciiChar (buffer, driver);
		}
		if (terminatorCount > 0 && c != terminatorChar)
		{
			terminatorCount = 0;
		}
	}
	return bufferConsume (buffer);
}

static Buffer * bufferNew (void)
{
	Buffer * buffer = (Buffer *) elektraCalloc (sizeof (Buffer));
	if (buffer == NULL)
	{
		return NULL;
	}
	buffer->size = 64;
	buffer->data = (char *) elektraCalloc (sizeof (char) * buffer->size);
	if (buffer->data == NULL)
	{
		elektraFree (buffer);
		return NULL;
	}
	return buffer;
}

static char * bufferConsume (Buffer * buffer)
{
	if (buffer != NULL)
	{
		char * data = buffer->data;
		buffer->data = NULL;
		buffer->index = 0;
		buffer->size = 0;
		elektraFree (buffer);
		return data;
	}
	else
	{
		return NULL;
	}
}

static void bufferAddChar (Buffer * buffer, char c, Driver * driver)
{
	ELEKTRA_ASSERT (driver != NULL, "Driver expected to be non-NULL, but was NULL");
	if (buffer != NULL)
	{
		buffer->data[buffer->index++] = c;
		if (buffer->index == buffer->size)
		{
			buffer->size *= 2;
			if (elektraRealloc ((void **) &buffer->data, buffer->size) < 0)
			{
				driverErrorGeneric (driver, ERROR_MEMORY, "bufferAddChar", "elektraRealloc");
				buffer->size /= 2;
			}
			else
			{
				memset (buffer->data + buffer->index, 0, buffer->size - buffer->index);
			}
		}
	}
}

static void handleEscapedSequence (Buffer * buffer, bool isMultiline, Driver * driver)
{
	char c = input ();
	switch (c)
	{
	case '\n': // possible line ending backslash characters
	case '\r':
	case ' ':
	case '\t':
		if (!isMultiline)
		{
			driverError (driver, ERROR_SYNTACTIC, yylineno,
				     "Malformed Input: Invalid escape sequence for single-line string: '%c' (0x%02X)\n", c, c);
			break;
		}
		else
		{
			unput (c);
			handlePossibleLineEndingBackslash (buffer, driver);
			break;
		}
	case '\"':
	case '\\':
	case 'b':
	case 'f':
	case 'n':
	case 'r':
	case 't':
		bufferAddChar (buffer, c, driver);
		break;
	case 'u':
		bufferAddChar (buffer, c, driver);
		readHex (buffer, 4, driver);
		if (!validUtf8FromUnicode (&buffer->data[buffer->index - 4], 4))
		{
			driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed Input: Invalid unicode escape sequence");
		}

		break;
	case 'U':
		bufferAddChar (buffer, c, driver);
		readHex (buffer, 8, driver);
		if (!validUtf8FromUnicode (&buffer->data[buffer->index - 8], 8))
		{
			driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed Input: Invalid unicode escape sequence");
		}
		break;
	default:
		driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed Input: Invalid escape sequence: \\'%c'", c);
		break;
	}
}

static void handlePossibleLineEndingBackslash (Buffer * buffer, Driver * driver)
{
	char c = input ();
	switch (c)
	{
	case '\n':
		bufferAddChar (buffer, c, driver);
		break;
	case '\r':
		bufferAddChar (buffer, c, driver);
		c = input ();
		if (c != '\n')
		{
			driverError (
				driver, ERROR_SYNTACTIC, yylineno,
				"Malformed Input: Invalid line ending sequence, expected newline after carriage return, but got 0x%02X", c);
		}
		else
		{
			bufferAddChar (buffer, c, driver);
		}
		break;
	case ' ':
	case '\t':
		bufferAddChar (buffer, c, driver);
		break;
	default:
		bufferAddChar (buffer, c, driver);
		break;
	}
}


static void readHex (Buffer * buffer, size_t count, Driver * driver)
{
	for (size_t i = 0; i < count; i++)
	{
		char c = input ();
		if (!validHex (c))
		{
			driverError (driver, ERROR_SYNTACTIC, yylineno,
				     "Malformed Input: Invalid unicode escape char, expected hexadecimal char, got : '%c'", c);
		}
		bufferAddChar (buffer, c, driver);
	}
}


static void readNonAsciiChar (Buffer * buffer, Driver * driver)
{
	unsigned char c = (unsigned char) input ();
	int utfLen = utf8LenFromHeadChar (c);
	if (utfLen > 0)
	{

		bufferAddChar (buffer, (char) c, driver);
		for (int i = 1; i < utfLen; i++)
		{
			c = (unsigned char) input ();
			if (c < 0x80 || c >= 0xC0)
			{
				driverError (
					driver, ERROR_SYNTACTIC, yylineno,
					"Malformed Input: Invalid utf8 sequence, expected char to be in range 0x80-0xBF, but was 0x%02X",
					c);
			}
			else
			{
				bufferAddChar (buffer, (char) c, driver);
			}
		}
	}
	else if (utfLen == 1)
	{
		driverError (driver, ERROR_SYNTACTIC, yylineno,
			     "Malformed input: Character '0x%02X' is not valid ascii, but utf-8 sequence is of len 1, expected length "
			     "greater than 1.",
			     c);
	}
	else
	{
		driverError (driver, ERROR_SYNTACTIC, yylineno, "Malformed input: Invalid utf8 head char: 0x%02X", c);
	}
}

static size_t whitespaceToSpaces (const char * whitespace)
{
	size_t spaces = 0;
	while (*whitespace != 0)
	{
		switch (*whitespace++)
		{
		case ' ':
			spaces++;
			break;
		case '\t':
			spaces += 4;
			break;
		default:
			return spaces;
		}
	}
	return spaces;
}
