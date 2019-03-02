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
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[high cyclomatic complexity]")))
#endif
{
	switch (type)
	{
	case Token::STREAM_START:
		return "start of stream";
	case Token::STREAM_END:
		return "end of stream";
	case Token::COMMENT:
		return "comment";
	case Token::PLAIN_SCALAR:
		return "plain scalar";
	case Token::SINGLE_QUOTED_SCALAR:
		return "single quoted scalar";
	case Token::DOUBLE_QUOTED_SCALAR:
		return "double quoted scalar";
	case Token::MAP_START:
		return "start of map";
	case Token::MAP_END:
		return "end of map";
	case Token::KEY:
		return "key";
	case Token::VALUE:
		return "value";
	case Token::SEQUENCE_START:
		return "start of sequence";
	case Token::SEQUENCE_END:
		return "end of sequence";
	case Token::ELEMENT:
		return "element";
	default:
		break;
	}
	return "end of file";
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
 * @brief This method returns the location of the token.
 *
 * @return The location data of this token
 */
Location Token::getLocation () const
{
	return _location;
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
	auto const & text = token.getText ();
	return text.length () > 0 ? text : typeToString (token.getType ());
}

} // namespace yawn
