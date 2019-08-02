# This script changes the names of symbols used by ANTLR in error messages. For that purpose the script
#
# - reads the generated parser code stored in the file `PARSER_SOURCE_FILE`,
# - replaces the symbol names with a more human readable form, and
# - stores the result in the file `PARSER_MODIFIED_SOURCE_FILE`
#
# .

file (READ ${PARSER_SOURCE_FILE} PARSER_SOURCE)

string (REGEX
	REPLACE "\"STREAM_START\""
		"\"start of document\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")
string (REGEX
	REPLACE "\"STREAM_END\""
		"\"end of document\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"COMMENT\""
		"\"comment\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"PLAIN_SCALAR\""
		"\"plain scalar\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"SINGLE_QUOTED_SCALAR\""
		"\"single quoted scalar\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"DOUBLE_QUOTED_SCALAR\""
		"\"double quoted scalar\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"MAP_START\""
		"\"start of map\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")
string (REGEX
	REPLACE "\"MAP_END\""
		"\"end of map\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"KEY\""
		"\"key\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"VALUE\""
		"\"value\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"SEQUENCE_START\""
		"\"start of sequence\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"SEQUENCE_END\""
		"\"end of sequence\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

string (REGEX
	REPLACE "\"ELEMENT\""
		"\"element\""
		PARSER_SOURCE
		"${PARSER_SOURCE}")

file (WRITE ${PARSER_MODIFIED_SOURCE_FILE} "${PARSER_SOURCE}")
