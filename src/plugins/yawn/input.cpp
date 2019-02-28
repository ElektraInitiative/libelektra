/**
 * @file
 *
 * @brief This file contains definitions for a class that represents textual
 *        input.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <codecvt>
#include <locale>
#include <sstream>
#include <stdexcept>

#include "input.hpp"

using std::codecvt_utf8;
using std::ifstream;
using std::out_of_range;
using std::string;
using std::stringstream;
using std::wstring_convert;

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/**
 * @brief This constructor creates an input from the given stream.
 *
 * @param stream This parameter stores the text this object operates on.
 */
Input::Input (ifstream const & stream)
{
	stringstream stringStream;
	stringStream << stream.rdbuf ();
	input = wstring_convert<codecvt_utf8<char32_t>, char32_t>{}.from_bytes (stringStream.str ());
}

/**
 * @brief This function returns a character that was not consumed yet.
 *
 * @param offset This variable specifies the index of the character
 *               this method should retrieve as offset to the last consumed
 *               character.
 *
 * @return A character which is `offset` positions away from the last
 *         consumed character
 */
size_t Input::LA (size_t const offset) const
{
	if (offset == 0 || position + offset > input.size ())
	{
		return 0;
	}
	return input[position + offset - 1];
}

/**
 * @brief This method consumes the next character of `input`.
 */
void Input::consume ()
{
	if (position + 1 > input.size ())
	{
		throw out_of_range ("Unable to consume EOF");
		return;
	}
	position++;
}

/**
 * @brief Retrieve the current position inside the input.
 *
 * @return The current position in number of characters
 */
size_t Input::index () const
{
	return position;
}

/**
 * @brief This method retrieves the text between `start` (inclusive) and the
 *        current position (exclusive).
 *
 * @param start This parameter specifies the start index of the string this
 *              functions returns.
 *
 * @return A UTF-8 encoded substring of input starting at `start` and ending
 *         one character before the current position in the input
 */
string Input::getText (size_t const start) const
{
	string text = wstring_convert<codecvt_utf8<char32_t>, char32_t>{}.to_bytes (input.substr (start, position - start));
	return text;
}

/**
 * @brief This method retrieves the whole text represented by this input object.
 *
 * @return A UTF-8 encoded string that stores the data of the input
 */
string Input::toString () const
{
	return wstring_convert<std::codecvt_utf8<char32_t>, char32_t>{}.to_bytes (input);
}

} // namespace yawn
