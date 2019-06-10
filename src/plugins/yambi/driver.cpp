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
#include <stdexcept>

#include <kdbconfig.h>

#include "driver.hpp"

using std::ifstream;
using std::overflow_error;
using std::string;
using std::to_string;

using kdb::Key;
using kdb::KeySet;

using yambi::Parser;

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

/**
 * @brief This function returns a Clang-like error message for a given error.
 *
 * @param location This parameter stores the location of the error.
 * @param input This value stores the textual input where the error occurred.
 * @param prefix This variable stores as prefix that this function prepends
 *               to every line of the visualized error message.
 *
 * @return A string representation of the error
 */
string visualizeError (location_type const & location, string const & input, string const & prefix)
{
	string::size_type start = 0;
	string::size_type end = 0;
	for (size_t currentLine = 1; currentLine <= location.begin.line; currentLine++)
	{
		size_t offset = (end == 0 ? 0 : 1);
		start = end + offset;
		end = input.find ("\n", end + offset);
	}

	string errorLine = input.substr (start, end - start);

	errorLine = prefix + errorLine + "\n" + prefix + string (location.begin.column - 1, ' ');
	// We assume that an error does not span more than one line
	start = location.begin.column;
	end = location.end.column - 1;
	errorLine += "^"; // Show at least one caret, even if the token is 0 characters long
	for (size_t current = start; current < end; current++)
	{
		errorLine += "^";
	}

	return errorLine;
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
	parents.push (parent.dup ());
}

/**
 * @brief This function parses the current YAML file.
 *
 * @param filename This parameter stores the path of the file the driver
 *                 should parse.
 *
 * @retval -3 if the given file could not be opened
 * @retval -2 if parsing was unsuccessful due to memory exhaustion
 * @retval -1 if the given file contains a syntax error
 * @retval  0 if parsing was successful
 */
int Driver::parse (const string & filepath)
{
	filename = filepath;

	ifstream input{ filename };
	if (!input.good ()) return -3;

	Lexer lexer{ input };
	Parser parser{ lexer, *this };

#if DEBUG
	parser.set_debug_level (1);
#endif
	numberOfErrors = 0;
	auto status = parser.parse ();
	if (status == 0 && numberOfErrors > 0)
	{
		status = 1;
	}

	return -status;
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
 * @param input This value stores the current input of the lexer/parser as text
 */
void Driver::error (const location_type & location, const string & message, string const & input)
{
	numberOfErrors++;
	auto position = filename + ":" + to_string (location.begin.line) + ":" + to_string (location.begin.column) + ": ";
	auto indent = string (position.length (), ' ');

	errorMessage += "\n" + position + message + "\n";
	errorMessage += visualizeError (location, input, indent);
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
 * @brief This function will be called before the parser enters an empty file (that might contain comments).
 */
void Driver::enterEmpty ()
{
	// We add a parent key that stores nothing representing an empty file.
	keys.append (Key{ parents.top ().getName (), KEY_BINARY, KEY_END });
}

/**
 * @brief This function will be called after the parser exits a value.
 *
 * @param text This variable contains the text stored in the value.
 */
void Driver::exitValue (string const & text)
{
	Key key = parents.top ();
	if (text == "true" || text == "false")
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
		parents.top ().setBinary (NULL, 0);
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
 * @brief This function will be called after the parser read an element of a
 *        sequence.
 */
void Driver::exitElement ()
{
	parents.pop (); // Remove the key for the current array entry
}
