/**
 * @file
 *
 * @brief This file contains the declaration of a class that represents textual
 *        input.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAWN_INPUT_HPP
#define ELEKTRA_PLUGIN_YAWN_INPUT_HPP

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <fstream>

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/**
 * @brief This class provides methods for a lexer to analyze textual input.
 */
class Input
{
	/** This variable stores the input represented by this class. */
	std::u32string input;

	/** This variable stores the current position inside `input`. */
	size_t position = 0;

public:
	/**
	 * @brief This constructor creates an input from the given stream.
	 *
	 * @param stream This parameter stores the text this object operates on.
	 */
	Input (std::ifstream const & stream);

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
	size_t LA (size_t const offset) const;

	/**
	 * @brief This method consumes the next character of `input`.
	 */
	void consume ();

	/**
	 * @brief Retrieve the current position inside the input.
	 *
	 * @return The current position in number of characters
	 */
	size_t index () const;

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
	std::string getText (size_t const start) const;

	/**
	 * @brief This method retrieves the whole text represented by this input object.
	 *
	 * @return A UTF-8 encoded string that stores the data of the input
	 */
	std::string toString () const;
};

} // namespace yawn

#endif // ELEKTRA_PLUGIN_YAWN_INPUT_HPP
