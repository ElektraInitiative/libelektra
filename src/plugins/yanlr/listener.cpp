/**
 * @file
 *
 * @brief A listener reacting to matches for the grammar rules defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include <kdbconfig.h>

#include "listener.hpp"

using std::string;

// -- Functions ----------------------------------------------------------------

namespace
{

/**
 * @brief This function converts a given number to an array base name.
 *
 * @param index This number specifies the index of the array entry.
 *
 * @return A string representing the given indices as Elektra array name.
 */
string indexToArrayBaseName (uintmax_t const index)
{
	using std::to_string;

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

namespace yanlr
{

using kdb::Key;
using kdb::KeySet;

using ElementContext = yanlr::YAML::ElementContext;
using EmptyContext = yanlr::YAML::EmptyContext;
using PairContext = YAML::PairContext;
using ValueContext = YAML::ValueContext;
using SequenceContext = yanlr::YAML::SequenceContext;

/**
 * @brief This constructor creates a new empty key storage using the given
 *        parent key.
 *
 * @param parent This key specifies the parent of all keys stored in the
 *               object.
 */
KeyListener::KeyListener (Key parent) : keys{}
{
	parents.push (parent.dup ());
}

/**
 * @brief This function returns the data read by the parser.
 *
 * @return The key set representing the data from the textual input
 */
KeySet KeyListener::keySet ()
{
	return keys;
}

/**
 * @brief This function will be called when the listener enters an empty file (that might contain comments).
 *
 * @param context The context specifies data matched by the rule.
 */
void KeyListener::enterEmpty (EmptyContext * context ELEKTRA_UNUSED)
{
	// We add a parent key that stores nothing representing an empty file.
	keys.append (Key{ parents.top ().getName (), KEY_BINARY, KEY_END });
}

/**
 * @brief This function will be called after the parser exits a value.
 *
 * @param context The context specifies data matched by the rule.
 */
void KeyListener::exitValue (ValueContext * context)
{
	Key key = parents.top ();
	string value = context->getText ();
	if (value == "true" || value == "false")
	{
		key.set<bool> (value == "true");
	}
	else
	{
		key.setString (scalarToText (value));
	}
	keys.append (key);
}

/**
 * @brief This function will be called after the parser enters a key-value pair.
 *
 * @param context The context specifies data matched by the rule.
 */
void KeyListener::enterPair (PairContext * context)
{
	// Entering a mapping such as `part: …` means that we need to add `part` to
	// the key name
	Key child{ parents.top ().getName (), KEY_END };
	child.addBaseName (scalarToText (context->key ()->getText ()));
	parents.push (child);
	if (!context->child ())
	{
		// Add key with empty value
		// The parser does not visit `exitValue` in that case
		child.setBinary (NULL, 0);
		keys.append (child);
	}
}

/**
 * @brief This function will be called after the parser exits a key-value pair.
 *
 * @param context The context specifies data matched by the rule.
 */
void KeyListener::exitPair (PairContext * context __attribute__ ((unused)))
{
	// Returning from a mapping such as `part: …` means that we need need to
	// remove the key for `part` from the stack.
	parents.pop ();
}

/**
 * @brief This function will be called after the parser enters a sequence.
 *
 * @param context The context specifies data matched by the rule.
 */
void KeyListener::enterSequence (SequenceContext * context __attribute__ ((unused)))
{
	indices.push (0);
	parents.top ().setMeta ("array", ""); // We start with an empty array
}

/**
 * @brief This function will be called after the parser exits a sequence.
 *
 * @param context The context specifies data matched by the rule.
 */
void KeyListener::exitSequence (SequenceContext * context __attribute__ ((unused)))
{
	// We add the parent key of all array elements after we leave the sequence
	keys.append (parents.top ());
	indices.pop ();
}

/**
 * @brief This function will be called after the parser recognizes an element
 *        of a sequence.
 *
 * @param context The context specifies data matched by the rule.
 */
void KeyListener::enterElement (ElementContext * context __attribute__ ((unused)))
{

	Key key{ parents.top ().getName (), KEY_END };
	key.addBaseName (indexToArrayBaseName (indices.top ()));

	uintmax_t index = indices.top ();
	indices.pop ();
	if (index < UINTMAX_MAX)
	{
		index++;
	}
	indices.push (index);

	parents.top ().setMeta ("array", key.getBaseName ());
	parents.push (key);
}

/**
 * @brief This function will be called after the parser read an element of a
 *        sequence.
 *
 * @param context The context specifies data matched by the rule.
 */
void KeyListener::exitElement (ElementContext * context __attribute__ ((unused)))
{
	parents.pop (); // Remove the key for the current array entry
}
}
