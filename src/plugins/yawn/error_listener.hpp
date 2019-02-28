/**
 * @file
 *
 * @brief This file implements an error listener for the YAML parser.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAWN_ERROR_LISTENER_HPP
#define ELEKTRA_PLUGIN_YAWN_ERROR_LISTENER_HPP

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <yaep.h>

#include "location.hpp"

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/** This class reacts to errors reported by the YAML parser. */
class ErrorListener
{

	/** This attribute stores the number of encountered syntax errors. */
	size_t errors = 0;

	/** This variable stores the last error message produced by the parser. */
	std::string message;

	/** This member stores the source of the error (e.g. a path to the parsed file). */
	std::string source;

	/** This member stores the content of `source` as UTF-8 encoded string. */
	std::string input;

	/**
	 * @brief This method returns a Clang-like error message for a given error.
	 *
	 * @param location This parameter stores the location of the error.
	 * @param prefix This variable stores as prefix that this function prepends
	 *               to every line of the visualized error message.
	 *
	 * @return A string representation of the error
	 */
	std::string visualizeError (Location const & location, std::string const & prefix);

public:
	/**
	 * @brief This constructor creates a new error listener using the given arguments.
	 *
	 * @param errorSource This text stores an identifier, usually the filename, that identifies the source of an error.
	 * @param text This variable stores the content of `errorSource` as UTF-8 encoded string.
	 */
	ErrorListener (std::string const & errorSource, std::string const & text);

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
	void syntaxError (int errorToken, void * errorTokenData, int ignoredToken, void * ignoredTokenData, int recoveredToken,
			  void * recoveredTokenData);

	/**
	 * @brief This method returns a description of the last syntax error.
	 *
	 * @return A text describing the last error
	 */
	std::string getErrorMessage () const;

	/**
	 * @brief This method returns the number of syntax errors reported by YAEP.
	 *
	 * @return The number of syntax errors found in the parsed input
	 */
	size_t getNumberOfErrors () const;
};

} // namespace yawn

#endif // ELEKTRA_PLUGIN_YAWN_ERROR_LISTENER_HPP
