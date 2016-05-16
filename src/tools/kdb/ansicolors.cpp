#include "ansicolors.hpp"

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
		case ANSI_COLOR::MAGENTA:
			return "\x1b[33m";
			break;
		case ANSI_COLOR::BLUE:
			return "\x1b[34m";
			break;
		case ANSI_COLOR::CYAN:
			return "\x1b[35m";
			break;
		case ANSI_COLOR::WHITE:
			return "\x1b[36m";
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
		case ANSI_COLOR::MAGENTA:
			return "\x1b[44m";
			break;
		case ANSI_COLOR::BLUE:
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
