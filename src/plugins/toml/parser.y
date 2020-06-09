%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "scalar.h"
#include "driver.h"


extern int yylex(Driver * driver);

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

%%

Toml	: 	AnyNewlines Nodes AnyNewlines { driverExitToml(driver); }
        |   %empty
        ;

Nodes   : 	Node
        |	Nodes Newlines Node
        ;

Node	:	COMMENT { driverExitComment (driver, $1); }
        | 	Table OptComment { driverExitOptCommentTable (driver); }
        | 	KeyPair OptComment { driverExitOptCommentKeyPair (driver); }
        ;

OptComment	:   COMMENT { driverExitComment (driver, $1); }
            |   %empty
            ;


Newlines	:	NEWLINE { /*driverExitNewline (driver);*/ }
            |	Newlines NEWLINE { driverExitNewline (driver); }
            ;	

AnyNewlines :   Newlines | %empty;

Table	:	TableSimple
        |	TableArray
        ;

TableSimple	:	BRACKETS_OPEN { driverEnterSimpleTable(driver); } TopKey { driverExitSimpleTable(driver); } BRACKETS_CLOSE
            ;

TableArray	:	BRACKETS_OPEN BRACKETS_OPEN { driverEnterTableArray(driver); } TopKey { driverExitTableArray(driver); } BRACKETS_CLOSE BRACKETS_CLOSE
            ;

KeyPair :	TopKey EQUAL Value { driverExitKeyValue (driver); }
        ;

TopKey  :   { driverEnterKey (driver); } Key { driverExitKey (driver); }
        ;

Key     :	SimpleKey
        |	SimpleKey DottedKeys
        ;

DottedKeys	:	DOT SimpleKey
            |	DottedKeys DOT SimpleKey
            ;

SimpleKey	:	BARE_STRING { driverExitSimpleKey (driver, $1); }
            |	LITERAL_STRING { driverExitSimpleKey (driver, $1); }
            |	BASIC_STRING { driverExitSimpleKey (driver, $1); }
            ;

Value   :	Scalar { driverExitScalar (driver, $1); }
        |	InlineTable
        |	Array
        ;

InlineTable	:	CURLY_OPEN { driverEnterInlineTable(driver); } InlineTableList CURLY_CLOSE { driverExitInlineTable (driver); }
            |	CURLY_OPEN CURLY_CLOSE { driverEmptyInlineTable(driver); }
            ;

InlineTableList	:	KeyPair
                |	InlineTableList COMMA KeyPair
                ;

Array	:	ArrayEmpty | ArrayNonEmpty;

ArrayNonEmpty   :   BRACKETS_OPEN { driverEnterArray (driver); } ArrayList ArrayEpilogue BRACKETS_CLOSE { driverExitArray (driver); };
ArrayEmpty      :   BRACKETS_OPEN BRACKETS_CLOSE { driverEmptyArray (driver); };


ArrayList	:   AnyCommentNewline ArrayElement
            |	ArrayList COMMA AnyCommentNewline ArrayElement
            ;

ArrayElement    :   Value { driverExitArrayElement (driver); };

ArrayEpilogue   :   %empty
                |   AnyCommentNewline
                |   COMMA AnyCommentNewline
                ;

AnyCommentNewline	:	AnyCommentNewline NEWLINE { driverExitNewline (driver); }
                    |	AnyCommentNewline COMMENT NEWLINE { driverExitComment (driver, $2); /* No exit newline here because comments imply a newline*/  }
                    |	%empty 
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
