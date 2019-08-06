/**
 * @file
 *
 * @brief This file contains the implementation of a basic listener.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include "listener.hpp"

using std::string;

using kdb::Key;

// -- Functions ----------------------------------------------------------------

namespace
{

using std::to_string;

/**
 * @brief This function converts a given number to an array base name.
 *
 * @param index This number specifies the index of the array entry.
 *
 * @return A string representing the given indices as Elektra array name.
 */
string indexToArrayBaseName (uintmax_t const index)
{
	size_t digits = 1;

	for (uintmax_t value = index; value > 9; digits++)
	{
		value /= 10;
	}

	return "#" + string (digits - 1, '_') + to_string (index);
}

/**
 * @brief This function converts a YAML scalar to a string.
 *
 * @param text This string contains a YAML scalar (including quote
 *             characters).
 *
 * @return A string without leading and trailing quote characters
 */
string scalarToText (string const & text)
{
	if (text.length () == 0)
	{
		return text;
	}
	if (*(text.begin ()) == '"' || *(text.begin ()) == '\'')
	{
		return text.substr (1, text.length () - 2);
	}
	return text;
}

} // namespace

// -- Class --------------------------------------------------------------------

namespace yaypeg
{

using std::overflow_error;

/**
 * @brief This constructor creates a Listener using the given parent key.
 *
 * @param parent This argument specifies the parent key of the key set this
 *               listener produces.
 */
Listener::Listener (Key const & parent)
{
	parents.push (parent.dup ());
}

/**
 * @brief This function will be called after the walker exits a value node.
 *
 * @param text This variable contains the text stored in the value.
 */
void Listener::exitValue (string const & text)
{
	Key key = parents.top ();
	if (text.length () == 0)
	{
		key.setBinary (NULL, 0);
	}
	else if (text == "true" || text == "false")
	{
		key.set<bool> (text == "true");
	}
	else
	{
		key.set<string> (scalarToText (text));
	}
	keys.append (key);
}

/**
 * @brief This function will be called after the walker exits a key node.
 *
 * @param text This variable contains the text of the key.
 */
void Listener::exitKey (string const & text)
{
	// Entering a mapping such as `part: …` means that we need to add `part` to
	// the key name
	Key child{ parents.top ().getName (), KEY_END };
	child.addBaseName (scalarToText (text));
	parents.push (child);
}

/**
 * @brief This function will be called after the walker exits the node for a
 *        key-value pair.
 */
void Listener::exitPair ()
{
	// Returning from a mapping such as `part: …` means that we need need to
	// remove the key for `part` from the stack.
	parents.pop ();
}

/**
 * @brief This function will be called before the walker enters an empty document (that might contain comments).
 */
void Listener::enterEmpty ()
{
	// We represent an empty file (`null`) as empty parent key.
	keys.append (Key{ parents.top ().getName (), KEY_BINARY, KEY_END });
}

/**
 * @brief This function will be called before the walker enters a sequence
 *        node.
 */
void Listener::enterSequence ()
{
	indices.push (0);
	parents.top ().setMeta ("array", ""); // We start with an empty array
}

/**
 * @brief This function will be called after the walker exits a sequence node.
 */
void Listener::exitSequence ()
{
	// We add the parent key of all array elements after we leave the sequence
	keys.append (parents.top ());
	indices.pop ();
}

/**
 * @brief This function will be called before the walker enters an element
 *        node.
 */
void Listener::enterElement ()
{
	Key key{ parents.top ().getName (), KEY_END };
	if (indices.top () >= UINTMAX_MAX) throw overflow_error ("Unable to increase array index for array “" + key.getName () + "”");

	key.addBaseName (indexToArrayBaseName (indices.top ()));

	uintmax_t index = indices.top ();
	indices.pop ();
	index++;
	indices.push (index);

	parents.top ().setMeta ("array", key.getBaseName ());
	parents.push (key);
}

/**
 * @brief This function will be called after the walker exits a sequence node.
 */
void Listener::exitElement ()
{
	parents.pop (); // Remove the key for the current array entry
}

/**
 * @brief This method returns the key set of the listener.
 *
 * @return A key set created by the walker by calling methods of this class
 **/
kdb::KeySet Listener::getKeySet () const
{
	return keys;
}

} // namespace yaypeg
