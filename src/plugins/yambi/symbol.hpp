/**
 * @file
 *
 * @brief This file contains a wrapper for Bison’s `symbol_type`.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef SYMBOL_HPP
#define SYMBOL_HPP

// -- Imports ------------------------------------------------------------------

#include "parser.hpp"

using std::string;
using std::to_string;

using yy::parser;
using yy::position;

using location_type = parser::location_type;
using symbol_type = parser::symbol_type;
using token_type = parser::token_type;
using token = parser::token;

// -- Macros -------------------------------------------------------------------

#define switchToken(TOK)                                                                                                                   \
	case token::TOKEN_##TOK:                                                                                                           \
		return parser::make_##TOK (text, placement)

// -- Class --------------------------------------------------------------------

/**
 * This class acts as wrapper for `symbol_type`, which we can not use inside a
 * queue, since its copy assignment operator is private.
 */
class Symbol
{
	/** This variable stores the location of this symbol. */
	location_type placement;

	/** This variable specifies the token type of this symbol. */
	token_type tokenType;

	/** This variable stores the actual value of the symbol. */
	string text;

public:
	/**
	 * @brief This constructor creates a symbol from the given arguments.
	 *
	 * @param type This argument specifies the token type of the symbol.
	 * @param location This argument specifies the location of the symbol.
	 * @param value This variable stores the value of this symbol.
	 */
	Symbol (token_type const & type, location_type const & location, string const & value = "")
	: placement{ location }, tokenType{ type }, text{ value }
	{
	}

	/**
	 * @brief This method returns the Bison symbol represented by this object.
	 *
	 * @return A symbol representing this object
	 */
	symbol_type get () const
	{
		switch (tokenType)
		{
			switchToken (STREAM_START);
			switchToken (STREAM_END);
			switchToken (COMMENT);
			switchToken (PLAIN_SCALAR);
			switchToken (SINGLE_QUOTED_SCALAR);
			switchToken (DOUBLE_QUOTED_SCALAR);
			switchToken (MAPPING_START);
			switchToken (KEY);
			switchToken (VALUE);
			switchToken (SEQUENCE_START);
			switchToken (ELEMENT);
			switchToken (BLOCK_END);
		default:
			return parser::make_END (placement);
		}
	}

	/**
	 * @brief This method returns the start position of this symbol.
	 *
	 * @return The start position of this symbol
	 */
	position getStart () const
	{
		return placement.begin;
	}

	/**
	 * @brief This method returns a string representation of this symbol.
	 *
	 * @return A string representing this symbol
	 */
	string toString () const
	{
		return "<" + to_string (placement.begin.line) + ":" + to_string (placement.begin.column) + "," +
		       to_string (placement.end.line) + ":" + to_string (placement.end.column) + "," + text + ">";
	}
};

#endif // SYMBOL_HPP
