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

#include "convert.hpp"
#include "error_listener.hpp"
#include "lexer.hpp"
#include "listener.hpp"
#include "memory.hpp"
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

namespace
{
// -- Globals ------------------------------------------------------------------------------------------------------------------------------

ErrorListener * errorListenerAdress;
Lexer * lexerAddress;
Memory * parserMemoryAddress;

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
	return errorListenerAdress->syntaxError (errorToken, errorTokenData, ignoredToken, ignoredTokenData, recoveredToken,
						 recoveredTokenData);
}

/**
 * This function allocates a memory region of the given size.
 *
 * @param size This variable specifies the amount of data this method should
 *             allocate.
 *
 * @return A pointer to a memory region of the specified size
 */
void * alloc (int size)
{
	return parserMemoryAddress->allocate (size);
}

} // namespace

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
 * @retval -1 if there was a error converting the YAML file
 * @retval  0 if parsing was successful and the function did not change the
 *            given keyset
 * @retval  1 if parsing was successful and the function did change `keySet`
 */
int addToKeySet (CppKeySet & keySet, CppKey & parent, string const & filename)
{
	string const grammar =
#include "yaml.h"
		;

	yaep parser;
	if (parser.parse_grammar (1, grammar.c_str ()) != 0)
	{
		cerr << "Unable to parse grammar:" << parser.error_message () << endl;
		return EXIT_FAILURE;
	}

	Memory memory;
	parserMemoryAddress = &memory;

	ErrorListener errorListener;
	errorListenerAdress = &errorListener;

	ifstream input{ filename };
	if (!input.good ())
	{
		perror (string ("Unable to open file “" + filename + "”").c_str ());
		return -2;
	}

	Lexer lexer{ input };
	lexerAddress = &lexer;

	int ambiguousOutput;
	struct yaep_tree_node * root = nullptr;

	parser.parse (nextToken, syntaxError, alloc, nullptr, &root, &ambiguousOutput);

	if (ambiguousOutput)
	{
		cerr << "The content of file “" + filename + "” showed that the grammar:\n" + grammar +
				"\nproduces ambiguous output! Please fix the grammar to make "
				"sure it produces only one unique syntax tree for every kind "
				"of YAML input.";
		return -1;
	}

	if (errorListener.getNumberOfErrors () > 0)
	{
		cerr << "Unable to parse input: " << errorListener.getErrorMessage () << endl;
		return -1;
	}

	Listener listener{ parent };
	walk (listener, root);
	keySet.append (listener.getKeySet ());

	return 0;
}
