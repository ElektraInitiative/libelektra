/**
 * @file
 *
 * @brief This file contains a tree walker function.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <iostream>
#include <memory>

#include <kdbassert.h>
#include <kdblogger.h>

#include "listener.hpp"
#include "token.hpp"
#include "walk.hpp"

using std::cerr;
using std::cout;
using std::endl;
using std::move;
using std::string;
using std::to_string;
using std::unique_ptr;

using yawn::Listener;
using yawn::Token;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace
{

#ifdef HAVE_LOGGER

/**
 * @brief This function returns the string representation of a tree node.
 *
 * @param node This argument stores the tree node that this function converts to
 *             a string.
 *
 * @return A string representation of the given node
 */
string toString (yaep_tree_node const * const node, string const indent = "")
{
	switch (node->type)
	{
	case yaep_tree_node_type::YAEP_NIL:
		return indent + "<Nil>";
	case yaep_tree_node_type::YAEP_ERROR:
		return indent + "<Error>";
	case yaep_tree_node_type::YAEP_ALT:
		return indent + "<Alternative>";
	default:
		break;
	}

	if (node->type == yaep_tree_node_type::YAEP_TERM)
	{
		return indent + to_string (**(static_cast<unique_ptr<Token> *> (node->val.term.attr)));
	}

	// Node is abstract
	yaep_anode anode = node->val.anode;
	string representation = indent + string ("<Abstract Node, ") + anode.name + ", " + to_string (anode.cost) + ">";
	yaep_tree_node ** children = anode.children;

	for (size_t child = 0; children[child]; child++)
	{
		representation += "\n" + toString (children[child], indent + "  ");
	}

	return representation;
}

#endif // HAVE_LOGGER

/**
 * @brief This function will be called before the walker enters an abstract
 *        tree node.
 *
 * @param listener The function calls methods of this class when it encounters
 *                 an abstract node with a certain name.
 * @param node This argument stores the abstract tree node
 */
void executeEnter (Listener & listener, yaep_anode const & anode)
{
	if (string (anode.name) == "sequence")
	{
		listener.enterSequence ();
	}
	else if (string (anode.name) == "element")
	{
		listener.enterElement ();
	}
	else if (string (anode.name) == "empty")
	{
		listener.enterEmpty ();
	}
}

/**
 * @brief This function will be called after the walker exits an abstract
 *        tree node.
 *
 * @param listener The function calls methods of this class when it encountered
 *                 an abstract node with a certain name.
 * @param node This argument stores the abstract tree node
 */
void executeExit (Listener & listener, yaep_anode const & anode)
{
	if (string (anode.name) == "value")
	{
		auto token = anode.children[0]->val.term.attr;
		listener.exitValue ((**static_cast<unique_ptr<Token> *> (token)).getText ());
	}
	else if (string (anode.name) == "key")
	{
		auto token = anode.children[0]->val.term.attr;
		listener.exitKey ((**static_cast<unique_ptr<Token> *> (token)).getText ());
	}
	else if (string (anode.name) == "pair")
	{
		bool matchedValue = anode.children[1]->type != yaep_tree_node_type::YAEP_NIL;
		listener.exitPair (matchedValue);
	}
	else if (string (anode.name) == "sequence")
	{
		listener.exitSequence ();
	}
	else if (string (anode.name) == "element")
	{
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
void executeListenerMethods (Listener & listener, yaep_tree_node const * node)
{
	if (node->type == yaep_tree_node_type::YAEP_TERM || node->type == yaep_tree_node_type::YAEP_NIL)
	{
		return;
	}
	ELEKTRA_ASSERT (node->type == yaep_tree_node_type::YAEP_ANODE, "Found unexpected node type");

	// Node is abstract
	yaep_anode anode = node->val.anode;

	executeEnter (listener, anode);

	yaep_tree_node ** children = anode.children;
	for (size_t child = 0; children[child]; child++)
	{
		executeListenerMethods (listener, children[child]);
	}

	executeExit (listener, anode);
}

} // namespace

namespace yawn
{

/**
 * @brief This function walks a syntax tree calling methods of the given
 *        listener.
 *
 * @param listener This argument specifies the listener which this function
 *                 uses to convert the syntax tree to a key set.
 * @param root This variable stores the root of the tree this function visits.
 */
void walk (Listener & listener, yaep_tree_node const * root)
{
#ifdef HAVE_LOGGER
	string tree = "\n— Syntax Tree —\n\n" + toString (root) + "\n";
	ELEKTRA_LOG_DEBUG ("%s", tree.c_str ());
#endif

	executeListenerMethods (listener, root);
}

} // namespace yawn
