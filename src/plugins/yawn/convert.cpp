/**
 * @file
 *
 * @brief This file contains a basic YAML to `KeySet` converter function.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>

#include <yaep.h>

#include <kdberrors.h>

#include "convert.hpp"
#include "error_listener.hpp"
#include "lexer.hpp"
#include "listener.hpp"
#include "walk.hpp"

using std::cerr;
using std::cout;
using std::endl;
using std::ifstream;
using std::string;
using std::stringstream;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;
using ckdb::keyNew;

using yawn::ErrorListener;
using yawn::Lexer;

namespace
{

// -- Globals ------------------------------------------------------------------------------------------------------------------------------

ErrorListener * errorListenerAdress;
Lexer * lexerAddress;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

/**
 * @brief This function returns the next token produced by the lexer.
 *
 * If the lexer found the end of the input, then this function returns `-1`.
 *
 * @param attribute The parser uses this parameter to store auxiliary data for
 *                  the returned token.
 *
 * @return A number specifying the type of the first token the parser has not
 *         emitted yet
 */
int nextToken (void ** attribute)
{
	return lexerAddress->nextToken (attribute);
}

/**
 * @brief This function reacts to syntax errors reported by YAEP’s parsing
 *        engine.
 *
 * @param errorToken This number specifies the token where the error occurred.
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
		  void * recoveredTokenData)
{
	errorListenerAdress->syntaxError (errorToken, errorTokenData, ignoredToken, ignoredTokenData, recoveredToken, recoveredTokenData);
}

/**
 * @brief This function parses the YAML grammar contained in `yaml.bnf`.
 *
 * @param parser This variable stores the YAEP parser that uses the YAML grammar contained in `yaml.bnf` to parse input.
 * @param error This function stores error information in this key, if it was unable to parse the grammar.
 *
 * @return A string containing the content of `yaml.bnf`, if parsing of the grammar was successful, or an empty string otherwise
 */
string parseGrammar (yaep & parser, CppKey & error)
{
	string grammar =
#include "yaml_grammar.h"
		;

	if (parser.parse_grammar (1, grammar.c_str ()) != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_PARSING, error.getKey (), "Unable to parse grammar: %s", parser.error_message ());
		return "";
	}
	return grammar;
}

/**
 * @brief This function creates a stream containing the content of a given file.
 *
 * @param filename This variable stores location of the file for which this function creates an input stream
 * @param error This function stores an error message in this key, if it was unable to access `filename`.
 *
 * @return an input stream that contains the content of `filename`, if creating the stream was successful
 */
ifstream openFile (string const & filename, CppKey & error)
{
	ifstream input{ filename };
	if (!input.good ())
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_RESOURCE, error.getKey (), "Unable to open file “%s”", filename.c_str ());
	}
	return input;
}

/**
 * @brief This function stores error information in `error`, if parsing was unsuccessful.
 *
 * @param ambiguousOutput This variable is true, when YAEP was unable to create a unique syntax tree from the given input.
 * @param errorListener This object stores information about errors that occurred while YAEP parsed the input.
 * @param filename This variable stores the location of the file YAEP parsed.
 * @param grammar This argument stores the YAML grammar used to parse the input.
 * @param error This function will use this variable to store information about errors that occurred while parsing.
 *
 * @retval -1 If there was an error parsing the last input
 * @retval  0 Otherwise
 */
int handleErrors (int const ambiguousOutput, ErrorListener const & errorListener, string const & filename, string const & grammar,
		  CppKey & error)
{
	if (ambiguousOutput)
	{
		ELEKTRA_SET_ERRORF (
			ELEKTRA_ERROR_PARSING, error.getKey (),
			"The content of file “%s” showed that the grammar:\n%s\nproduces ambiguous output!\n"
			"Please fix the grammar, to make sure it produces only one unique syntax tree for every kind of YAML input.",
			filename.c_str (), grammar.c_str ());
		return -1;
	}

	if (errorListener.getNumberOfErrors () > 0)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_PARSING, error.getKey (), errorListener.getErrorMessage ().c_str ());
		return -1;
	}
	return 0;
}

} // namespace

namespace yawn
{

/**
 * @brief This function converts the given YAML file to keys and adds the
 *        result to `keySet`.
 *
 * @param keySet The function adds the converted keys to this variable.
 * @param parent The function uses this parent key of `keySet` to emit error
 *               information.
 * @param filename This parameter stores the path of the YAML file this
 *                 function converts.
 *
 * @retval -2 if the file could not be opened for reading
 * @retval -1 if there was an error converting the YAML file
 * @retval  0 if parsing was successful and the function did not change the
 *            given key set
 * @retval  1 if parsing was successful and the function did change `keySet`
 */
int addToKeySet (CppKeySet & keySet, CppKey & parent, string const & filename)
{
	yaep parser;

	auto grammar = parseGrammar (parser, parent);
	if (grammar.size () <= 0) return -1;

	auto input = openFile (filename, parent);
	if (!input.good ()) return -1;

	Lexer lexer{ input };
	lexerAddress = &lexer;

	ErrorListener errorListener{ filename, lexer.getText () };
	errorListenerAdress = &errorListener;

	int ambiguousOutput;
	struct yaep_tree_node * root = nullptr;

	parser.parse (nextToken, syntaxError, nullptr, nullptr, &root, &ambiguousOutput);

	if (handleErrors (ambiguousOutput, errorListener, filename, grammar, parent) < 0)
	{
		yaep::free_tree (root, nullptr, nullptr);
		return -1;
	}

	Listener listener{ parent };
	walk (listener, root);
	keySet.append (listener.getKeySet ());

	yaep::free_tree (root, nullptr, nullptr);

	return listener.getKeySet ().size () > 0;
}

} // namespace yawn
