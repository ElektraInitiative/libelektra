/**
 * @file
 *
 * @brief This file contains a class representing a position inside a text.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef ELEKTRA_PLUGIN_YAWN_POSITION_HPP
#define ELEKTRA_PLUGIN_YAWN_POSITION_HPP

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/** This class represents a position inside a text. */
class Position
{
public:
	/** This variables stores the column of the position. */
	size_t column = 1;
	/** This variables specifies the line of the position. */
	size_t line = 1;
};

} // namespace yawn

#endif // ELEKTRA_PLUGIN_YAWN_POSITION_HPP
