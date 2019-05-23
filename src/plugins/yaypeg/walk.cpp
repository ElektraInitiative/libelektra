/**
 * @file
 *
 * @brief This file contains a tree walker function.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include <iostream>

#include <kdblogger.h>

#include "listener.hpp"
#include "walk.hpp"

using tao::TAO_PEGTL_NAMESPACE::parse_tree::node;

// -- Functions ----------------------------------------------------------------

namespace
{

using std::string;

using yaypeg::Listener;

/**
 * @brief This function checks if a given string ends with another string
 *
 * @param text This parameter stores the string that should be checked for the
 *             ending stored in `ending`.
 * @param ending This variable stores the text that will be compared with the
 *               end of `text`.
 *
 * @retval true If `text` ends with `ending`
 * @retval false Otherwise
 */
bool ends_with (std::string const & text, std::string const & ending)
{
	return ending.size () > text.size () ? false : std::equal (ending.rbegin (), ending.rend (), text.rbegin ());
}

#ifdef HAVE_LOGGER

/**
 * @brief This function returns the string representation of a tree node.
 *
 * @param node This argument stores the tree node that this function converts to
 *             a string.
 * @param indent This variable stores the string representation of the current
 *               depth of the node.
 *
 * @return A string representation of the given node
 */
string toString (node const & node, string const indent = "")
{
	string representation;

	representation += node.is_root () ? "root" : indent + node.name ();
	if (!node.is_root () && node.has_content ())
	{
		representation += ": “" + node.content () + "”";
	}

	if (!node.children.empty ())
	{
		for (auto & child : node.children)
		{
			representation += "\n" + toString (*child, indent + "  ");
		}
	}
	return representation;
}

#endif

/**
 * @brief This function will be called before the walker enters a tree node.
 *
 * @param listener The function calls methods of this class when it encounters
 *                 a node with a certain name.
 * @param node This argument stores the tree node
 */
void executeEnter (Listener & listener, node const & node)
{
	if (node.is_root ())
	{
		return;
	}

	if (ends_with (node.name (), "c_l_block_seq_entry"))
	{
		listener.enterElement ();
	}
	else if (ends_with (node.name (), "l_plus_block_sequence"))
	{
		listener.enterSequence ();
	}
}

/**
 * @brief This function will be called after the walker exits a tree node.
 *
 * @param listener The function calls methods of this class when it encounters
 *                 a node with a certain name.
 * @param node This argument stores the parse tree node
 */
void executeExit (Listener & listener, node const & node)
{
	if (node.is_root ())
	{
		return;
	}

	if (ends_with (node.name (), "ns_s_block_map_implicit_key"))
	{
		listener.exitKey (node.children.back ()->content ());
	}
	else if (ends_with (node.name (), "c_l_block_map_implicit_value") && ends_with (node.children.back ()->name (), "node"))
	{
		listener.exitValue (node.children.back ()->content ());
	}
	else if (ends_with (node.name (), "ns_l_block_map_implicit_entry"))
	{
		listener.exitPair ();
	}
	else if (ends_with (node.name (), "l_plus_block_sequence"))
	{
		listener.exitSequence ();
	}
	else if (ends_with (node.name (), "c_l_block_seq_entry"))
	{
		if (ends_with (node.children.back ()->name (), "node"))
		{
			listener.exitValue (node.children.back ()->content ());
		}
		listener.exitElement ();
	}
}

/**
 * @brief This function traverses a tree executing methods of a listener class.
 *
 * @param listener The function calls methods of this class while it traverses
 *                 the tree.
 * @param node This argument stores the tree node that this function traverses.
 */
void executeListenerMethods (Listener & listener, node const & node)
{

	executeEnter (listener, node);

	if (!node.children.empty ())
	{
		for (auto & child : node.children)
		{
			executeListenerMethods (listener, *child);
		}
	}

	executeExit (listener, node);
}

} // namespace

namespace yaypeg
{

/**
 * @brief This function walks a tree calling methods of the given listener.
 *
 * @param listener This argument specifies the listener which this function
 *                 uses to convert the tree to a key set.
 * @param root This variable stores the root of the tree this function
 *             visits.
 */
void walk (Listener & listener, node const & node)
{
#ifdef HAVE_LOGGER
	ELEKTRA_LOG_DEBUG ("Parse Tree");
	ELEKTRA_LOG_DEBUG ("——————————");

	std::istringstream stream (toString (node));
	for (string line; std::getline (stream, line);)
	{
		ELEKTRA_LOG_DEBUG ("%s", line.c_str ());
	}

	ELEKTRA_LOG_DEBUG ("——————————");
#endif

	// If the document contains only a single value we call `exitValue`
	// for that function. We need to handle that special case to not add
	// value multiple times for maps (once for `c_l_block_map_implicit_value`
	// and `c_l_block_seq_entry`) and once for the child of
	// `c_l_block_map_implicit_value`).
	if (node.is_root () && !node.children.empty () && ends_with (node.children.back ()->name (), "node"))
	{
		listener.exitValue (node.children.back ()->content ());
		return;
	}

	executeListenerMethods (listener, node);
}

} // namespace yaypeg
