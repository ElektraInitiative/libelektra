/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "ansicolors.hpp"
#ifdef _WIN32
#include <io.h>
#include <stdio.h>
#undef STDERR_FILENO
#undef STDOUT_FILENO
#define STDERR_FILENO _fileno (stderr)
#define STDOUT_FILENO _fileno (stdout)
#define isatty _isatty
#else
#include <unistd.h>
#endif
std::string & colors ()
{
	static std::string nocolors = "auto";
	return nocolors;
}

std::string getColorEscape (ANSI_COLOR color, ANSI_COLOR_LAYER layer)
{
	if (color == ANSI_COLOR::RESET) return "\x1b[0m";
	if (color == ANSI_COLOR::BOLD) return "\x1b[1m";
	if (color == ANSI_COLOR::UNDERSCORE) return "\x1b[4m";
	if (layer == ANSI_COLOR_LAYER::FG)
	{
		switch (color)
		{
		case ANSI_COLOR::BLACK:
			return "\x1b[30m";
			break;
		case ANSI_COLOR::RED:
			return "\x1b[31m";
			break;
		case ANSI_COLOR::GREEN:
			return "\x1b[32m";
			break;
		case ANSI_COLOR::YELLOW:
			return "\x1b[33m";
			break;
		case ANSI_COLOR::BLUE:
			return "\x1b[34m";
			break;
		case ANSI_COLOR::MAGENTA:
			return "\x1b[35m";
			break;
		case ANSI_COLOR::CYAN:
			return "\x1b[36m";
			break;
		case ANSI_COLOR::WHITE:
			return "\x1b[37m";
			break;
		default:
			return "";
		}
	}
	else
	{
		switch (color)
		{
		case ANSI_COLOR::BLACK:
			return "\x1b[40m";
			break;
		case ANSI_COLOR::RED:
			return "\x1b[41m";
			break;
		case ANSI_COLOR::GREEN:
			return "\x1b[42m";
			break;
		case ANSI_COLOR::YELLOW:
			return "\x1b[43m";
			break;
		case ANSI_COLOR::BLUE:
			return "\x1b[44m";
			break;
		case ANSI_COLOR::MAGENTA:
			return "\x1b[45m";
			break;
		case ANSI_COLOR::CYAN:
			return "\x1b[46m";
			break;
		case ANSI_COLOR::WHITE:
			return "\x1b[47m";
			break;
		default:
			return "";
		}
	}
}

std::string getErrorColor (ANSI_COLOR color, ANSI_COLOR_LAYER layer)
{
	if (colors ().compare ("never") == 0 || (colors ().compare ("auto") == 0 && !isatty (STDERR_FILENO))) return "";
	return getColorEscape (color, layer);
}
std::string getStdColor (ANSI_COLOR color, ANSI_COLOR_LAYER layer)
{
	if (colors ().compare ("never") == 0 || (colors ().compare ("auto") == 0 && !isatty (STDOUT_FILENO))) return "";
	return getColorEscape (color, layer);
}

#ifdef _WIN32
#undef isatty
#undef STDERR_FILENO
#undef STDOUT_FILENO
#endif
