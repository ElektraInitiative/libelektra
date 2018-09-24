/**
 * @file
 *
 * @brief This file implements an error listener for the YAML parser.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <memory>
#include <string>

#include <yaep.h>

#include "error_listener.hpp"
#include "token.hpp"

using std::string;
using std::to_string;
using std::unique_ptr;

// -- Class --------------------------------------------------------------------------------------------------------------------------------

/**
 * @brief This method reacts to syntax errors reported by YAEP’s parsing
 *        engine.
 *
 * @param errorTokenNumber This number specifies the token where the error
 *                         occurred.
 * @param errorTokenData This variable stores the data contained in
 *                       `errorToken`.
 * @param ignoredToken This number specifies the first token that was ignored
 *                     during error recovery.
 * @param ignoredTokenData This variable stores the data contained in
 *                         `ignoredToken`.
 * @param recoveredToken This number specifies the first included token after
 *                       the error recovery has taken place.
 * @param recoveredTokenData This variable stores the data contained in
 *                           `recoveredToken`.
 */
void ErrorListener::syntaxError (int errorTokenNumber, void * errorTokenData, int ignoredToken,
				 void * ignoredTokenData __attribute__ ((unused)), int recoveredToken,
				 void * recoveredTokenData __attribute__ ((unused)))
{
	errors++;
	message = "Syntax error on token number " + to_string (errorTokenNumber) + ": “" +
		  to_string (**static_cast<unique_ptr<Token> *> (errorTokenData)) + "”\n";
	if (ignoredToken > 0)
	{
		message += "Ignoring " + to_string (recoveredToken - ignoredToken) + " tokens starting with token number " +
			   to_string (ignoredToken);
	}
}

/**
 * @brief This method returns a description of the last syntax error.
 *
 * @return A text describing the last error
 */
string ErrorListener::getErrorMessage () const
{
	return message;
}

/**
 * @brief This method returns the number of syntax errors reported by YAEP.
 *
 * @return The number of syntax errors found in the parsed input
 */
size_t ErrorListener::getNumberOfErrors () const
{
	return errors;
}
