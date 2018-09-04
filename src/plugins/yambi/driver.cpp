/**
 * @file
 *
 * @brief This file specifies auxiliary functions and data used by a Bison
 *        parser to convert YAML data to a key set.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include <cerrno>
#include <fstream>

#include "driver.hpp"

using std::cerr;
using std::endl;

using kdb::Key;
using kdb::KeySet;

using yy::parser;

// -- Macros -------------------------------------------------------------------

#define DEBUG_LEVEL 0

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

/**
 * This constructor creates a new driver for the given parent key.
 *
 * @param parent This key specifies the parent of the key set the parser
 *               creates.
 */
Driver::Driver (Key const & parent)
{
	parents.push (parent);
}

/**
 * @brief This function parses the current YAML file.
 *
 * @param filename This parameter stores the path of the file the driver
 *                 should parse.
 *
 * @retval -3 if the given file could not be opened
 *         -2 if parsing was unsuccessful due to memory exhaustion
 *         -1 if the given file contains a syntax error
 *          0 if parsing was successful
 */
int Driver::parse (const string & filepath)
{
	filename = filepath;

	ifstream input{ filename };
	if (!input.good ())
	{
		perror (string ("Unable to open file “" + filename + "”").c_str ());
		return -3;
	}

	Lexer lexer{ input };
	parser parser{ lexer, *this };
	parser.set_debug_level (DEBUG_LEVEL);

	return -parser.parse ();
}

/**
 * @brief This method retrieves the current key set produced by the driver.
 *
 * @return A key set representing the YAML data produced by the last call of
 *         the method `parse`
 */
KeySet Driver::getKeySet () const
{
	return keys;
}

/**
 * @brief This function will be called by the Bison parser to indicate an error.
 *
 * @param location This value specifies the location of the erroneous input.
 * @param message This value stores the error message emitted by the Bison
 *                parser.
 */
void Driver::error (const location_type & location, const string & message)
{
	errorMessage = filename + ":" + to_string (location.begin.line) + ":" + to_string (location.begin.column) + ": " + message;
}

/**
 * @brief This function returns the last error message produced by the parser.
 *
 * @return A string containing an error message describing a syntax error
 */
string Driver::getErrorMessage ()
{
	return errorMessage;
}

// ===========
// = Actions =
// ===========

/**
 * @brief This function will be called after the parser exits a value.
 *
 * @param text This variable contains the text stored in the value.
 */
void Driver::exitValue (string const & text)
{
	Key key = parents.top ();
	key.setString (scalarToText (text));
	keys.append (key);
}

/**
 * @brief This function will be called after the parser found a key.
 *
 * @param text This variable contains the text of the key.
 */
void Driver::exitKey (string const & text)
{
	// Entering a mapping such as `part: …` means that we need to add `part` to
	// the key name
	Key child{ parents.top ().getName (), KEY_END };
	child.addBaseName (scalarToText (text));
	parents.push (child);
}

/**
 * @brief This function will be called after the parser exits a key-value
 *        pair.
 *
 * @param matchedValue This variable specifies if the pair contains a value
 *                     or not.
 */
void Driver::exitPair (bool const matchedValue)
{
	if (!matchedValue)
	{
		// Add key with empty value
		keys.append (parents.top ());
	}
	// Returning from a mapping such as `part: …` means that we need need to
	// remove the key for `part` from the stack.
	parents.pop ();
}

/**
 * @brief This function will be called after the parser enters a sequence.
 */
void Driver::enterSequence ()
{
	indices.push (0);
	parents.top ().setMeta ("array", ""); // We start with an empty array
}

/**
 * @brief This function will be called after the parser exits a sequence.
 */
void Driver::exitSequence ()
{
	// We add the parent key of all array elements after we leave the sequence
	keys.append (parents.top ());
	indices.pop ();
}

/**
 * @brief This function will be called after the parser recognizes an element
 *        of a sequence.
 */
void Driver::enterElement ()
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
 */
void Driver::exitElement ()
{
	parents.pop (); // Remove the key for the current array entry
}
