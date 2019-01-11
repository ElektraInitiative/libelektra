/**
 * @file
 *
 * @brief This file contains the declaration of a class that stores data
 *        about a token produced by a lexer.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAWN_TOKEN_HPP
#define ELEKTRA_PLUGIN_YAWN_TOKEN_HPP

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <string>

#include "location.hpp"
#include "token.hpp"

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/** This class represents a token emitted by a lexer. */
class Token
{

	/** This variable stores the location of this token in the scanned text. */
	Location _location;

	/** This attribute stores a number representing the kind of the token. */
	int _type;

	/** This variable stores the content stored inside the token. */
	std::string _text;

public:
	/** This token type starts the YAML stream. */
	static const int STREAM_START = 1;
	/** This token type ends the YAML stream. */
	static const int STREAM_END = 2;
	/** This token type specifies that the token stores a plain scalar. */
	static const int PLAIN_SCALAR = 3;
	/** This token type indicates the start of a mapping key. */
	static const int KEY = 4;
	/** This token type indicates the start of a mapping value. */
	static const int VALUE = 5;
	/** This token type indicates the start of a mapping. */
	static const int MAPPING_START = 6;
	/** This token type indicates the end of a block collection. */
	static const int BLOCK_END = 7;
	/** This token type indicates a list element. */
	static const int ELEMENT = 8;
	/** This token type indicates the start of a sequence. */
	static const int SEQUENCE_START = 9;
	/** This token type specifies that the token stores a double quoted scalar. */
	static const int DOUBLE_QUOTED_SCALAR = 10;
	/** This token type specifies that the token stores a (line) comment. */
	static const int COMMENT = 11;
	/** This token type specifies that the token stores a single quoted scalar. */
	static const int SINGLE_QUOTED_SCALAR = 12;

	/**
	 * @brief This function returns a string representation of a token.
	 *
	 * @param token This argument specifies the token for which this function
	 *              creates a string representation.
	 *
	 * @return A text representing the given token
	 **/
	friend std::string to_string (Token token);

	/**
	 * @brief This constructor creates a token from the given arguments.
	 *
	 * @param type This number specifies the type of the token.
	 * @param location This number specifies the location of the token in the
	 *                 scanned text.
	 * @param text This variable specifies the content that should be stored
	 *             in the token.
	 */
	Token (int const type, Location const & location, std::string const & text);

	/**
	 * @brief This method returns the type of the token.
	 *
	 * @return A number specifying the type of this token
	 */
	int getType () const;

	/**
	 * @brief This method returns the current start position of the token.
	 *
	 * @return The start position of this token
	 */
	Position getStart () const;

	/**
	 * @brief This method returns the content of the token.
	 *
	 * @return The text stored inside this token
	 */
	std::string getText () const;
};

} // namespace yawn

#endif // ELEKTRA_PLUGIN_YAWN_TOKEN_HPP
