/**
 * @file
 *
 * @brief Header for colored CLI output
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_COLOR_H
#define ELEKTRA_KDB_COLOR_H

#include <kdb.h>

#define RESET "\x1b[0m"

// colors
#define COLOR_BLACK "\x1b[30m"
#define COLOR_RED "\x1b[31m"
#define COLOR_GREEN "\x1b[32m"
#define COLOR_YELLOW "\x1b[33m"
#define COLOR_BLUE "\x1b[34m"
#define COLOR_MAGENTA "\x1b[35m"
#define COLOR_CYAN "\x1b[36m"
#define COLOR_WHITE "\x1b[37m"

// background colors
#define BG_COLOR_BLACK "\x1b[40m"
#define BG_COLOR_RED "\x1b[41m"
#define BG_COLOR_GREEN "\x1b[42m"
#define BG_COLOR_YELLOW "\x1b[43m"
#define BG_COLOR_BLUE "\x1b[44m"
#define BG_COLOR_MAGENTA "\x1b[45m"
#define BG_COLOR_CYAN "\x1b[46m"
#define BG_COLOR_WHITE "\x1b[47m"

// formatting
#define FORMAT_BOLD "\x1b[1m"
#define FORMAT_UNDERSCORE "\x1b[4m"

#define FORMAT_TEXT(escapeCode, text) formatString (&fmtBuffer, colorMode, escapeCode, text)

#define BLACK(text) FORMAT_TEXT (COLOR_BLACK, text)
#define RED(text) FORMAT_TEXT (COLOR_RED, text)
#define GREEN(text) FORMAT_TEXT (COLOR_GREEN, text)
#define YELLOW(text) FORMAT_TEXT (COLOR_YELLOW, text)
#define BLUE(text) FORMAT_TEXT (COLOR_BLUE, text)
#define MAGENTA(text) FORMAT_TEXT (COLOR_MAGENTA, text)
#define CYAN(text) FORMAT_TEXT (COLOR_CYAN, text)
#define WHITE(text) FORMAT_TEXT (COLOR_WHITE, text)

#define BG_BLACK(text) FORMAT_TEXT (BG_COLOR_BLACK, text)
#define BG_RED(text) FORMAT_TEXT (BG_COLOR_RED, text)
#define BG_GREEN(text) FORMAT_TEXT (BG_COLOR_GREEN, text)
#define BG_YELLOW(text) FORMAT_TEXT (BG_COLOR_YELLOW, text)
#define BG_BLUE(text) FORMAT_TEXT (BG_COLOR_BLUE, text)
#define BG_MAGENTA(text) FORMAT_TEXT (BG_COLOR_MAGENTA, text)
#define BG_CYAN(text) FORMAT_TEXT (BG_COLOR_CYAN, text)
#define BG_WHITE(text) FORMAT_TEXT (BG_COLOR_WHITE, text)

#define BOLD(text) FORMAT_TEXT (FORMAT_BOLD, text)
#define UNDERSCORE(text) FORMAT_TEXT (FORMAT_UNDERSCORE, text)

const char * formatString (char ** buffer, int colorMode, const char * color, const char * text);

enum COLOR_MODE
{
	CLI_COLOR_NEVER,
	CLI_COLOR_AUTO,
	CLI_COLOR_ALWAYS
};

#endif // ELEKTRA_KDB_COLOR_H
