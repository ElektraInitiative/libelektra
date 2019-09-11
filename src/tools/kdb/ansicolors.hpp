/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ANSI_COLORS_HPP
#define ANSI_COLORS_HPP

#include <string>
/**
 *  An enum type representing different ansi color escape sequences.
 */
enum class ANSI_COLOR
{
	RESET, ///< remove previous color properties
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
/**
 *  An enum type representing different ansi color escape layers.
 */
enum class ANSI_COLOR_LAYER
{
	FG,
	BG
};

/**
 * @brief Query if stderr has colors
 *
 * @param c to init with a string (never|auto), done by Cmdline, can only be done once
 *
 * @return if stderr has colors
 */
bool hasErrorColor (std::string const & c = "");


/**
 * @brief Query if stdout has colors
 *
 * @param c to init with a string (never|auto), done by Cmdline, can only be done once
 *
 * @return if stdout has colors
 */
bool hasStdColor (std::string const & c = "");

/**
 *  getColorEscape returns the ansi escape sequence for the requested color and
 *  the requested layer and shift it to the desired output channel. Do not forget
 *  to reset it afterwards.
 *  @param color the desired color or highlite. Use \p ANSI_COLOR::RESET to reset the style.
 *  @param layer the desired layer: foreground = ANSI_COLOR_LAYER::FG or background = ANSI_COLOR_LAYER::BG.
 *  @return ansi color escape sequence for tty
 */
std::string getColorEscape (ANSI_COLOR color, ANSI_COLOR_LAYER layer = ANSI_COLOR_LAYER::FG);

/**
 *  @see getColorEscape
 *  Only returns a color escape sequence if colors="always" or colors="auto" (default) and the cerr handle is a tty
 *  @param color the desired color or highlite. Use \p ANSI_COLOR::RESET to reset the style.
 *  @param layer the desired layer: foreground = ANSI_COLOR_LAYER::FG or background = ANSI_COLOR_LAYER::BG.
 *  @return ansi color escape sequence for tty if colors="always" or "auto" and cerr is a tty, it returns an empty string otherwise
 */
std::string getErrorColor (ANSI_COLOR color, ANSI_COLOR_LAYER layer = ANSI_COLOR_LAYER::FG);

/**
 *  @see getColorEscape
 *  Only returns a color escape sequence if colors="always" or colors="auto" (default) and the stdout is a tty
 *  @param color the desired color or highlite. Use \p ANSI_COLOR::RESET to reset the style.
 *  @param layer the desired layer: foreground = ANSI_COLOR_LAYER::FG or background = ANSI_COLOR_LAYER::BG.
 *  @return ansi color escape sequence for tty if colors="always" or "auto" and stdout is a tty, it returns an empty string otherwise
 */
std::string getStdColor (ANSI_COLOR color, ANSI_COLOR_LAYER layer = ANSI_COLOR_LAYER::FG);

#endif
