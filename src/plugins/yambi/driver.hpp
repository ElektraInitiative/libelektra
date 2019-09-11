/**
 * @file
 *
 * @brief This file specifies auxiliary functions and data used by a Bison
 *        parser to convert YAML data to a key set.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAMBI_DRIVER_HPP
#define ELEKTRA_PLUGIN_YAMBI_DRIVER_HPP

// -- Imports ------------------------------------------------------------------

#include <stack>
#include <string>

#include <kdb.hpp>

#include "lexer.hpp"
#include "parser.hpp"

typedef yambi::Parser::location_type location_type;

// -- Class --------------------------------------------------------------------

/**
 * @brief This class provides a communication interface between the
 *        parser (`parser`) and the parser user (`convert`).
 */
class Driver
{

	/** This variable stores the key set the drive creates from the given YAML
	    file */
	kdb::KeySet keys;

	/**
	 * This stack stores a key for each level of the current key name below
	 * parent.
	 */
	std::stack<kdb::Key> parents;

	/**
	 * This stack stores indices for the next array elements.
	 */
	std::stack<uintmax_t> indices;

	/**
	 * This variable stores all error message produced by the parser.
	 */
	std::string errorMessage;

	/**
	 * This variable stores the number of errors caught by the parser.
	 */
	size_t numberOfErrors;

public:
	/** This variable stores the path of the YAML file the driver is parsing. */
	std::string filename;

	/**
	 * This constructor creates a new driver for the given parent key.
	 *
	 * @param parent This key specifies the parent of the key set the parser
	 *               creates.
	 */
	Driver (kdb::Key const & parent);

	/**
	 * @brief This function parses the current YAML file.
	 *
	 * @param filename This parameter stores the path of the file the driver
	 *                 should parse.
	 *
	 * @retval -3 if the given file could not be opened
	 * @retval -2 if parsing was unsuccessful due to memory exhaustion
	 * @retval -1 if the given file contains a syntax error
	 * @retval  0 if parsing was successful
	 */
	int parse (const std::string & filepath);

	/**
	 * @brief This method retrieves the current key set produced by the driver.
	 *
	 * @return A key set representing the YAML data produced by the last call of
	 *         the method `parse`
	 */
	kdb::KeySet getKeySet () const;

	/**
	 * @brief This function will be called by the Bison parser to indicate an
	 *        error.
	 *
	 * @param location This value specifies the location of the erroneous input.
	 * @param message This value stores the error message emitted by the Bison
	 *                parser.
	 * @param input This value stores the current input of the lexer/parser as text
	 */
	void error (const location_type & location, const std::string & message, std::string const & input);

	/**
	 * @brief This function returns the last error message produced by the parser.
	 *
	 * @return A string containing an error message describing a syntax error
	 */
	std::string getErrorMessage ();

	// ===========
	// = Actions =
	// ===========

	/**
	 * @brief This function will be called before the parser enters an empty file (that might contain comments).
	 */
	void enterEmpty ();

	/**
	 * @brief This function will be called after the parser exits a value.
	 *
	 * @param text This variable contains the text stored in the value.
	 */
	void exitValue (std::string const & text);

	/**
	 * @brief This function will be called after the parser found a key.
	 *
	 * @param text This variable contains the text of the key.
	 */
	void exitKey (std::string const & text);

	/**
	 * @brief This function will be called after the parser exits a key-value
	 *        pair.
	 *
	 * @param matchedValue This variable specifies if the pair contains a value
	 *                     or not.
	 */
	void exitPair (bool const matchedValue);

	/**
	 * @brief This function will be called after the parser enters a sequence.
	 */
	void enterSequence ();

	/**
	 * @brief This function will be called after the parser exits a sequence.
	 */
	void exitSequence ();

	/**
	 * @brief This function will be called after the parser recognizes an element
	 *        of a sequence.
	 */
	void enterElement ();

	/**
	 * @brief This function will be called after the parser read an element of a
	 *        sequence.
	 */
	void exitElement ();
};

#endif // ELEKTRA_PLUGIN_YAMBI_DRIVER_HPP
