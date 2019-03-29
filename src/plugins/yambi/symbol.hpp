/**
 * @file
 *
 * @brief This file contains a wrapper for Bison’s `symbol_type`.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAMBI_SYMBOL_HPP
#define ELEKTRA_PLUGIN_YAMBI_SYMBOL_HPP

// -- Imports ------------------------------------------------------------------

#include "parser.hpp"

typedef yambi::Parser Parser;

typedef Parser::location_type location_type;
typedef Parser::symbol_type symbol_type;
typedef Parser::token_type token_type;
typedef Parser::token token;

// -- Macros -------------------------------------------------------------------

#define ELEKTRA_SWITCH_TOKEN(TOK)                                                                                                          \
	case token::TOK:                                                                                                                   \
		return Parser::make_##TOK (text, placement)

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
	std::string text;

public:
	/**
	 * @brief This constructor creates a symbol from the given arguments.
	 *
	 * @param type This argument specifies the token type of the symbol.
	 * @param location This argument specifies the location of the symbol.
	 * @param value This variable stores the value of this symbol.
	 */
	Symbol (token_type const & type, location_type const & location, std::string const & value = "")
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
			ELEKTRA_SWITCH_TOKEN (STREAM_START);
			ELEKTRA_SWITCH_TOKEN (STREAM_END);
			ELEKTRA_SWITCH_TOKEN (COMMENT);
			ELEKTRA_SWITCH_TOKEN (PLAIN_SCALAR);
			ELEKTRA_SWITCH_TOKEN (SINGLE_QUOTED_SCALAR);
			ELEKTRA_SWITCH_TOKEN (DOUBLE_QUOTED_SCALAR);
			ELEKTRA_SWITCH_TOKEN (MAP_START);
			ELEKTRA_SWITCH_TOKEN (MAP_END);
			ELEKTRA_SWITCH_TOKEN (KEY);
			ELEKTRA_SWITCH_TOKEN (VALUE);
			ELEKTRA_SWITCH_TOKEN (SEQUENCE_START);
			ELEKTRA_SWITCH_TOKEN (SEQUENCE_END);
			ELEKTRA_SWITCH_TOKEN (ELEMENT);
		default:
			return Parser::make_END (placement);
		}
	}

	/**
	 * @brief This method returns the location of this symbol.
	 *
	 * @return The location of this symbol
	 */
	yambi::location getLocation () const
	{
		return placement;
	}

	/**
	 * @brief This method returns a string representation of this symbol.
	 *
	 * @return A string representing this symbol
	 */
	std::string toString () const
	{
		return "<" + std::to_string (placement.begin.line) + ":" + std::to_string (placement.begin.column) + "," +
		       std::to_string (placement.end.line) + ":" + std::to_string (placement.end.column) + "," + text + ">";
	}
};

#endif // ELEKTRA_PLUGIN_YAMBI_SYMBOL_HPP
