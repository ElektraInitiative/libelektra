%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "scalar.h"
#include "driver.h"


extern int yylex(Driver * driver);
#define ABORT_ON_ERR(driver) {if(driver->errorSet) YYABORT;}

%}

%lex-param { Driver * driver }
%parse-param { Driver * driver }
%define parse.error verbose

%code requires {
#include "scalar.h"
#include "driver.h"
}

%code provides {
#define YY_DECL int yylex (Driver * driver)
YY_DECL;
}

%union {
	Scalar*			scalar;
}

%token <scalar> COMMENT
%token <scalar> DECIMAL
%token <scalar> HEXADECIMAL
%token <scalar> OCTAL
%token <scalar> BINARY
%token <scalar> FLOAT
%token <scalar> BOOLEAN
%token <scalar> BARE_STRING
%token <scalar> LITERAL_STRING
%token <scalar> BASIC_STRING
%token <scalar> MULTI_LITERAL_STRING
%token <scalar> MULTI_BASIC_STRING
%token <scalar> OFFSET_DATETIME
%token <scalar> LOCAL_DATETIME
%token <scalar> LOCAL_DATE
%token <scalar> LOCAL_TIME
%token NEWLINE
%token EQUAL
%token DOT
%token COMMA
%token BRACKETS_OPEN
%token BRACKETS_CLOSE
%token CURLY_OPEN
%token CURLY_CLOSE
%token SPACE
%token TAB

%type <scalar> Scalar
%type <scalar> IntegerScalar
%type <scalar> BooleanScalar
%type <scalar> FloatScalar
%type <scalar> StringScalar
%type <scalar> DateScalar
%type <scalar> SimpleKey


%start Toml

%%

Toml	:	AnyNewlines Nodes AnyNewlines { driverExitToml(driver); }
		|	%empty
		;

Nodes	:	Node
		|	Nodes Newlines Node
		;

Node	:	AnyWS COMMENT { driverExitComment (driver, $2); }
		|	AnyWS Table OptComment { driverExitOptCommentTable (driver); }
		|	AnyWS KeyPair OptComment { driverExitOptCommentKeyPair (driver); }
		;

OptComment	:	AnyWS COMMENT { driverExitComment (driver, $2); } AnySWS
			|	AnySWS
			;


Newlines	:	NEWLINE AnySWS
			|	Newlines NEWLINE AnySWS { driverExitNewline (driver); }
			;	

AnyNewlines	:	Newlines
			|	%empty
			;

Table		:	TableSimple
			|	TableArray
			;

TableSimple	:	BRACKETS_OPEN { driverEnterSimpleTable(driver); } AnySWS TopKey { driverExitSimpleTable(driver); } AnySWS BRACKETS_CLOSE
			;

TableArray	:	BRACKETS_OPEN BRACKETS_OPEN { driverEnterTableArray(driver); } AnySWS TopKey { driverExitTableArray(driver); } AnySWS BRACKETS_CLOSE BRACKETS_CLOSE
			;

KeyPair	:	TopKey AnySWS EQUAL AnySWS Value { driverExitKeyValue (driver); }
		;

TopKey	:	{ driverEnterKey (driver); } Key { driverExitKey (driver); }
		;

Key		:	SimpleKey
		|	SimpleKey DottedKeys
		;

DottedKeys	:	DOT SimpleKey
			|	DottedKeys DOT SimpleKey
			;

SimpleKey	:	Scalar { driverExitSimpleKey (driver, $1); }
			;

Value		:	Scalar { driverExitValue (driver, $1); }
			|	InlineTable
			|	Array
			;

InlineTable	:	CURLY_OPEN { driverEnterInlineTable(driver); } AnySWS InlineTableList AnySWS CURLY_CLOSE { driverExitInlineTable (driver); }
			|	CURLY_OPEN AnySWS CURLY_CLOSE { driverEmptyInlineTable(driver); }
			;

InlineTableList	:	KeyPair AnySWS
				|	InlineTableList COMMA AnySWS KeyPair
				;

Array		:	ArrayEmpty | ArrayNonEmpty
			;

ArrayNonEmpty	:	BRACKETS_OPEN { driverEnterArray (driver); } ArrayList ArrayEpilogue BRACKETS_CLOSE { driverExitArray (driver); };
ArrayEmpty	:	BRACKETS_OPEN BRACKETS_CLOSE { driverEmptyArray (driver); };


ArrayList	:	AnyCommentNL ArrayElement
			|	ArrayList COMMA AnyCommentNL ArrayElement
			;

ArrayElement	:	AnyWS Value { driverExitArrayElement (driver); }
				;

ArrayEpilogue	:	AnyCommentNL
				|	COMMA AnyCommentNL
				;

AnyCommentNL	:	AnyCommentNL NEWLINE { driverExitNewline (driver); }
				|	AnyCommentNL AnyWS COMMENT NEWLINE { driverExitComment (driver, $3); }
				|	AnySWS
				;

Scalar		:	IntegerScalar { $$ = $1; }
			|	BooleanScalar { $$ = $1; }
			|	FloatScalar { $$ = $1; }
			|	StringScalar { $$ = $1; }
			|	DateScalar { $$ = $1; }
			;

IntegerScalar	:	DECIMAL { $$ = $1; }
				|	HEXADECIMAL { $$ = $1; }
				|	OCTAL { $$ = $1; }
				|	BINARY { $$ = $1; }
				;

BooleanScalar	:	BOOLEAN { $$ = $1; }
				;

FloatScalar	:	FLOAT { $$ = $1; }
			;

StringScalar	:	LITERAL_STRING { $$ = $1; }
				|	BASIC_STRING { $$ = $1; }
				|	MULTI_LITERAL_STRING { $$ = $1; }
				|	MULTI_BASIC_STRING { $$ = $1; }
				|	BARE_STRING { $$ = $1; }
				;

DateScalar	:	OFFSET_DATETIME { $$ = $1; }
			|	LOCAL_DATETIME { $$ = $1; }
			|	LOCAL_DATE { $$ = $1; }
			|	LOCAL_TIME { $$ = $1; }
			;

AnyWS	:	%empty
		|	AnyWS Whitespace
		;
AnySWS	:	%empty
		|	AnySWS SilentWhitespace
		;
Whitespace	:	TAB {}
			|	SPACE {}
			;

SilentWhitespace	:	TAB
					|	SPACE
					;

%%
