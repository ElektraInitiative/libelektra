%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "scalar.h"
#include "driver.h"

extern int yylex(void);

%}

%code requires {
	#include "scalar.h"
	#include "driver.h"
}

%union {
	Scalar*     	scalar;
}

%token <scalar> COMMENT
%token <scalar> DECIMAL
%token <scalar> HEXADECIMAL
%token <scalar> OCTAL
%token <scalar> BINARY
%token <scalar> FLOAT
%token <scalar> BOOLEAN
%token <scalar> SCALAR
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

%type <scalar> Scalar
%type <scalar> IntegerScalar
%type <scalar> BooleanScalar
%type <scalar> FloatScalar
%type <scalar> StringScalar
%type <scalar> DateScalar
%type <scalar> SimpleKey

%start Toml

%parse-param { Driver* driver  }

%define parse.error verbose

%%

Toml	: 	AnyNewlines Nodes AnyNewlines {}
	    |	{}
	    ;
	

Nodes   : 	Node
        |	Nodes Newlines Node
        ;

Node	:	COMMENT { }
        | 	Table OptComment { }
        | 	KeyPair OptComment { }
        ;

OptComment	:
            |	COMMENT {}
            ;


Newlines	:	NEWLINE {}
            |	Newlines NEWLINE {}
            ;	

AnyNewlines :   Newlines | %empty;

Table	:	TableSimple {}
        |	TableArray {}	
        ;

TableSimple	:	BRACKETS_OPEN { driverEnterSimpleTable(driver); } Key { driverExitSimpleTable(driver); } BRACKETS_CLOSE {}
            ;

TableArray	:	BRACKETS_OPEN BRACKETS_OPEN { driverEnterTableArray(driver); } Key { driverExitTableArray(driver); } BRACKETS_CLOSE BRACKETS_CLOSE {}
            ;
KeyPair	:	{ driverEnterKeyValue (driver); } Key EQUAL Value { driverExitKeyValue (driver); }

Key     :	SimpleKey { driverExitSimpleKey (driver, $1); }
        |	SimpleKey { driverExitSimpleKey(driver, $1); } DottedKeys
        ;

DottedKeys	:	DOT SimpleKey { driverExitSimpleKey (driver, $2); }
            |	DottedKeys DOT SimpleKey { driverExitSimpleKey (driver, $3); }
            ;

SimpleKey	:	BARE_STRING { $$ = $1; }
            |	LITERAL_STRING { $$ = $1; }
            |	BASIC_STRING { $$ = $1; }
            ;

Value   :	Scalar { driverExitScalar (driver, $1); }
        |	InlineTable {}
        |	Array {}
        ;

InlineTable	:	CURLY_OPEN InlineTableList CURLY_CLOSE {}
            |	CURLY_OPEN CURLY_CLOSE {}	
            ;

InlineTableList	:	KeyPair {}
                |	COMMA KeyPair {}
                |	InlineTableList COMMA KeyPair {}
                ;
Array	:	BRACKETS_OPEN { driverEnterArray (driver); } ArrayList { driverExitArray (driver); } CommentNewline BRACKETS_CLOSE {}
        |	BRACKETS_OPEN { driverEnterArray (driver); driverExitArray (driver);  } BRACKETS_CLOSE {}
        ;
// TODO: trailing comma
ArrayList	:	CommentNewline { driverEnterArrayElement (driver); } Value { driverExitArrayElement (driver); }
            |	ArrayList COMMA CommentNewline { driverEnterArrayElement (driver); } Value  { driverExitArrayElement (driver); }
            ;

CommentNewline	:	CommentNewline NEWLINE {}
                |	CommentNewline COMMENT NEWLINE {}
                |	%empty {}
                ;

Scalar  :   IntegerScalar { $$ = $1; }
        |   BooleanScalar { $$ = $1; }
        |   FloatScalar { $$ = $1; }
        |   StringScalar { $$ = $1; }
        |   DateScalar { $$ = $1; }
        ;

IntegerScalar   :   DECIMAL { $$ = $1; }
                |   HEXADECIMAL { $$ = $1; }
                |   OCTAL { $$ = $1; }
                |   BINARY { $$ = $1; }
                ;

BooleanScalar   :   BOOLEAN { $$ = $1; }
                ;

FloatScalar     :   FLOAT { $$ = $1; }
                ;

StringScalar    :   LITERAL_STRING { $$ = $1; }
                |   BASIC_STRING { $$ = $1; }
                |   MULTI_LITERAL_STRING { $$ = $1; }
                |   MULTI_BASIC_STRING { $$ = $1; }
                ;

DateScalar      :   OFFSET_DATETIME { $$ = $1; }
                |   LOCAL_DATETIME { $$ = $1; }
                |   LOCAL_DATE { $$ = $1; }
                |   LOCAL_TIME { $$ = $1; }
                ;
%%
