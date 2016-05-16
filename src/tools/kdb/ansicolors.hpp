/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ANSI_COLORS_HPP
#define ANSI_COLORS_HPP

#include <string>

enum class ANSI_COLOR
{
	RESET,
	BOLD,
	UNDERSCORE,
	BLACK,
	RED,
	GREEN,
	YELLOW,
	MAGENTA,
	BLUE,
	CYAN,
	WHITE
};

enum class ANSI_COLOR_LAYER
{
	FG,
	BG
};

std::string getColorEscape (ANSI_COLOR color, ANSI_COLOR_LAYER layer = ANSI_COLOR_LAYER::FG);

#endif
