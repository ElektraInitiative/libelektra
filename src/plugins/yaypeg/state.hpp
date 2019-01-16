/**
 * @file
 *
 * @brief This file contains the definition of a struct used by the
 *        parser to store contextual data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAYPEG_CONTEXT_HPP
#define ELEKTRA_PLUGIN_YAYPEG_CONTEXT_HPP

// -- Imports ------------------------------------------------------------------

#include <deque>
#include <stack>
#include <string>

#include <kdb.hpp>

// -- Class --------------------------------------------------------------------

namespace yaypeg
{

/**
 * @brief This custom state stores contextual data used during the parsing
 *        process.
 */
struct State
{

	/**
	 * @brief This enum specifies possible values for the current context in the
	 *        YAML stream
	 */
	enum class Context
	{
		BLOCK_IN,  ///< The parser is currently inside a block sequence.
		BLOCK_OUT, ///< The parser is currently outside a block sequence.

		BLOCK_KEY, ///< The current (flow) node is part of a block key.
		FLOW_KEY,  ///< The current (flow) node is part of a flow key.
		FLOW_IN,   ///< The current (flow) node is part of a value inside a flow
			   ///< collection.
		FLOW_OUT   ///< The current (flow) node is part of a value outside a flow
			   ///< collection.
	};

	/** @brief This stack stores the current contexts. */
	std::stack<Context> context;

	/**
	 * @brief We use this double ended queue as stack to store the indentation
	 *        levels.
	 *
	 * We use a `deque` instead of a stack since we need access to both the last
	 * element and the element before that.
	 */
	std::deque<long long> indentation{ std::initializer_list<long long>{ -1 } };

	/**
	 * @brief This method converts the state to a string.
	 *
	 * @return A string representation of the current state.
	 */
	std::string toString () const noexcept;

private:
	/**
	 * @brief This method converts the current context to a string.
	 *
	 * @return The string representation of the context on top of the context
	 * stack.
	 */
	std::string contextToString () const noexcept;
};

} // namespace yaypeg

#endif // ELEKTRA_PLUGIN_YAYPEG_CONTEXT_HPP
