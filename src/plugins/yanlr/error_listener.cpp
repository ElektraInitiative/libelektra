/**
 * @file
 *
 * @brief A error listener reacting to mismatches of the grammar defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include <iostream>

#include "error_listener.hpp"

// -- Class --------------------------------------------------------------------

/**
 * @brief This method will be called if the parsing process fails.
 *
 * @param recognizer This parameter stores the current recognizer used to
 *                   parse the input.
 * @param offendingSymbol This token caused the failure of the parsing
 *                        process.
 * @param line This number specifies the line where the parsing process
 *             failed.
 * @param charPositionInLine This number specifies the character position in
 *                           `line`, where the parsing process failed.
 * @param message This text describes the parsing failure.
 * @param error This parameter stores the exception caused by the parsing
 *              failure.
 */
void ErrorListener::syntaxError (Recognizer * recognizer __attribute__ ((unused)), Token * offendingSymbol __attribute__ ((unused)),
				 size_t line __attribute__ ((unused)), size_t charPositionInLine __attribute__ ((unused)),
				 const std::string & message, std::exception_ptr error __attribute__ ((unused)))
{
	errorMessage = message;
}

/**
 * @brief This method returns the last error message saved by the error listener.
 *
 * @return A string containing an error message produced by the parser
 */
char const * ErrorListener::message ()
{
	return errorMessage.c_str ();
}
