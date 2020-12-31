/**
 * @file
 *
 * @brief an error listener reacting to mismatches of the grammar defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include <antlr4-runtime/antlr4-runtime.h>

namespace yanlr
{

// -- Class --------------------------------------------------------------------

/**
 * @brief This class specifies methods to alter error messages.
 */
class ErrorListener : public antlr4::BaseErrorListener
{
	/** This variable stores the last error message emitted via the function `syntaxError`. */
	std::string errorMessage;

	/** This attribute stores the source, for which the error listener reports an error. */
	std::string source;

public:
	/**
	 * @brief This constructor creates a new error listener using the given arguments.
	 *
	 * @param errorSource This text stores an identifier, usually the filename, that identifies the source of an error.
	 */
	ErrorListener (std::string const & errorSource);

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
	void syntaxError (antlr4::Recognizer * recognizer, antlr4::Token * offendingSymbol, size_t line, size_t charPositionInLine,
			  const std::string & message, std::exception_ptr error);

	/**
	 * @brief This method returns the last error message saved by the error listener.
	 *
	 * @return A string containing an error message produced by the parser
	 */
	char const * message ();
};
}
