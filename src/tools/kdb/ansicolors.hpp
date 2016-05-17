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

std::string & colors ();
/*YYY: If you add colored escaped to your command remembter to add the C option
       If you want colored warnings error include coloredkdbio.hpp before kdb.hpp */
std::string getColorEscape (ANSI_COLOR color, ANSI_COLOR_LAYER layer = ANSI_COLOR_LAYER::FG);
std::string getErrorColor (ANSI_COLOR color, ANSI_COLOR_LAYER layer = ANSI_COLOR_LAYER::FG);
std::string getStdColor (ANSI_COLOR color, ANSI_COLOR_LAYER layer = ANSI_COLOR_LAYER::FG);

#endif
