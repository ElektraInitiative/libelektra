/**
 * @file
 *
 * @brief This file contains the implementation of a class that stores data
 *        about a token produced by a lexer.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "token.hpp"

using std::string;

using yawn::Token;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace
{

/**
 * @brief This function returns the sting representation of a token type.
 *
 * @param type This variable specifies the token type that should be converted.
 *
 * @return A string representation of the given token type
 */
string typeToString (int const type)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high cyclomatic complexity]")))
#endif
{
	switch (type)
	{
	case Token::STREAM_START:
		return "STREAM_START";
	case Token::STREAM_END:
		return "STREAM_END";
	case Token::COMMENT:
		return "COMMENT";
	case Token::PLAIN_SCALAR:
		return "PLAIN_SCALAR";
	case Token::SINGLE_QUOTED_SCALAR:
		return "SINGLE_QUOTED_SCALAR";
	case Token::DOUBLE_QUOTED_SCALAR:
		return "DOUBLE_QUOTED_SCALAR";
	case Token::MAPPING_START:
		return "MAPPING_START";
	case Token::MAP_END:
		return "MAP_END";
	case Token::KEY:
		return "KEY";
	case Token::VALUE:
		return "VALUE";
	case Token::SEQUENCE_START:
		return "SEQUENCE_START";
	case Token::SEQUENCE_END:
		return "SEQUENCE_END";
	case Token::ELEMENT:
		return "ELEMENT";
	default:
		break;
	}
	return "EOF";
}

} // namespace

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/**
 * @brief This constructor creates a token from the given arguments.
 *
 * @param type This number specifies the type of the token.
 * @param location This number specifies the location of the token in the
		   scanned text.
 * @param text This variable specifies the content that should be stored
 *             in the token.
 */
Token::Token (int const type, Location const & location, std::string const & text) : _location{ location }, _type{ type }, _text{ text }
{
}

/**
 * @brief This method returns the type of the token.
 *
 * @return A number specifying the type of this token
 */
int Token::getType () const
{
	return _type;
}

/**
 * @brief This method returns the current start position of the token.
 *
 * @return The start position of this token
 */
Position Token::getStart () const
{
	return _location.begin;
}

/**
 * @brief This method returns the content of the token.
 *
 * @return The text stored inside this token
 */
string Token::getText () const
{
	return _text;
}

/**
 * @brief This function returns a string representation of a token.
 *
 * @param token This argument specifies the token for which this function
 *              creates a string representation.
 *
 * @return A text representing the given token
 **/
string to_string (Token const token)
{
	using std::to_string;

	return "<Token, " + typeToString (token.getType ()) + ", " + token.getText () + ", " + to_string (token._location.begin.line) +
	       ":" + to_string (token._location.begin.column) + "–" + to_string (token._location.end.line) + ":" +
	       to_string (token._location.end.column) + ">";
}

} // namespace yawn
