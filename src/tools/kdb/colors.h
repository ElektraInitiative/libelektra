/**
 * @file
 *
 * @brief Header for colored CLI output in the contex of a command, after the GET_BASIC_OPTIONS macro
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

/**
 * @def FORMAT_TEXT
 * @brief Formats the text according to the given escape code. Used by the color/formatting macros below.
 *
 * `char * fmtBuffer` and `int colorMode` must be defined in the calling context.
 * Normally, this is achieved by first using #GET_BASIC_OPTIONS
 *
 * @param escapeCode The escape code for the desired formatting.
 * @param text The text to be formatted.
 */
#define FORMAT_TEXT(escapeCode, text) formatString (&fmtBuffer, colorMode, escapeCode, text)

// must be in the contex of a command, so GET_BASIC_OPTIONS has to be in front of using any of these macros
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

/**
 * @brief Formats the string with specified escape code based on the color mode.
 *
 * This function formats a text string based on a provided escape code
 * (color, background color, bold, underscore, etc.) and the current color mode.
 * If colorMode is set to CLI_COLOR_NEVER, the function returns the original text.
 * In case of successful formatting, the function returns a pointer to the formatted text.
 * In case of memory allocation error, it returns an error message.
 *
 * @param buffer Pointer to the buffer that will hold the formatted string.
 * @param colorMode Integer representing the current color mode.
 * @param escapeCode Escape code (color, background color, bold, underscore, etc.) for formatting the text.
 * @param text Original text to be formatted.
 * @return A pointer to the formatted text, original text if colorMode is CLI_COLOR_NEVER, or an error message if memory allocation fails.
 */
const char * formatString (char ** buffer, int colorMode, const char * color, const char * text);

enum COLOR_MODE
{
	CLI_COLOR_NEVER,
	CLI_COLOR_AUTO,
	CLI_COLOR_ALWAYS
};

#endif // ELEKTRA_KDB_COLOR_H
