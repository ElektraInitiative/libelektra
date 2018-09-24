/**
 * @file
 *
 * @brief This file contains a class specifying the location of a token in a
 *        text.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAWN_LOCATION_HPP
#define ELEKTRA_PLUGIN_YAWN_LOCATION_HPP

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "position.hpp"

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/** This class represents the location of a token. */
class Location
{
public:
	/** This variable stores the start position of the token. */
	Position begin;
	/** This attribute stores the position where the token ends. */
	Position end;

	/**
	 * @brief This method sets the start position to the current end position.
	 */
	void step ()
	{
		begin = end;
	}

	/**
	 * @brief This method increases the column number of the end position by a
	 *        certain amount.
	 *
	 * @param offset This variable specifies how much the current column number
	 *               should be increased.
	 *
	 * @return A location that mirrors the old location number, except for the
	 *         column number of the end position, which was increased by the
	 *         amount `offset`
	 */
	Location & operator+= (size_t const offset)
	{
		end.column += offset;
		return *this;
	}

	/**
	 * @brief This method increases the line number of the end position by one.
	 */
	void lines ()
	{
		end.line++;
	}
};

} // namespace yawn

#endif // ELEKTRA_PLUGIN_YAWN_LOCATION_HPP
