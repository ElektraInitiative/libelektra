/**
 * @file
 *
 * @brief This file contains the declaration of a basic listener.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAYPEG_LISTENER_HPP
#define ELEKTRA_PLUGIN_YAYPEG_LISTENER_HPP

// -- Imports ------------------------------------------------------------------

#include <stack>

#include <kdb.hpp>

// -- Class --------------------------------------------------------------------

namespace yaypeg
{

/**
 * @brief This class contains methods to create a key set.
 *
 * The tree walker (`walker`) calls the various methods of this class to create
 * a key set from the syntax tree created by the parser (`convert`).
 */
class Listener
{

	/** @brief This variable stores the key set that this listener creates. */
	kdb::KeySet keys;

	/**
	 * @brief This stack stores a key for each level of the current key name below
	 *        parent.
	 */
	std::stack<kdb::Key> parents;

	/**
	 * @brief This stack stores indices for the next array elements.
	 */
	std::stack<uintmax_t> indices;

public:
	/**
	 * @brief This constructor creates a Listener using the given parent key.
	 *
	 * @param parent This argument specifies the parent key of the key set this
	 *               listener produces.
	 */
	Listener (kdb::Key const & parent);

	/**
	 * @brief This function will be called after the walker exits a value node.
	 *
	 * @param text This variable contains the text stored in the value.
	 */
	void exitValue (std::string const & text);

	/**
	 * @brief This function will be called after the walker exits a key node.
	 *
	 * @param text This variable contains the text of the key.
	 */
	void exitKey (std::string const & text);

	/**
	 * @brief This function will be called after the walker exits the node for a
	 *        key-value pair.
	 */
	void exitPair ();

	/**
	 * @brief This function will be called before the walker enters an empty document (that might contain comments).
	 */
	void enterEmpty ();

	/**
	 * @brief This function will be called before the walker enters a sequence
	 *        node.
	 */
	void enterSequence ();

	/**
	 * @brief This function will be called after the walker exits a sequence node.
	 */
	void exitSequence ();

	/**
	 * @brief This function will be called before the walker enters an element
	 *        node.
	 */
	void enterElement ();

	/**
	 * @brief This function will be called after the walker exits a sequence node.
	 */
	void exitElement ();

	/**
	 * @brief This method returns the key set of the listener.
	 *
	 * @return A key set created by the walker by calling methods of this class
	 */
	kdb::KeySet getKeySet () const;
};

} // namespace yaypeg

#endif // ELEKTRA_PLUGIN_YAYPEG_LISTENER_HPP
