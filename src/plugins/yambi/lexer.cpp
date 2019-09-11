/**
 * @file
 *
 * @brief This file contains a lexer that scans YAML data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include <fstream>
#include <stdexcept>

#include <kdblogger.h>

#include "lexer.hpp"

using std::deque;
using std::ifstream;
using std::make_pair;
using std::runtime_error;
using std::string;
using std::unique_ptr;

using yambi::Parser;
using location_type = Parser::location_type;
using token = Parser::token;

// -- Class --------------------------------------------------------------------

/**
 * @brief This method consumes characters from the input stream keeping
 *        track of line and column numbers.
 *
 * @param characters This parameter specifies the number of characters the
 *                   the function should consume.
 */
void Lexer::forward (size_t const characters = 1)
{
	ELEKTRA_LOG_DEBUG ("Forward %zu characters", characters);

	for (size_t charsLeft = characters; charsLeft > 0; charsLeft--)
	{
		if (input.LA (1) == 0)
		{
			ELEKTRA_LOG_DEBUG ("Hit EOF!");
			return;
		}

		location += 1;
		if (input.LA (1) == '\n')
		{
			location.end.column = 1;
			location.lines ();
		}
		input.consume ();
	}
}

/**
 * @brief This function adds an indentation value if the given value is smaller
 *        than the current indentation.
 *
 * @param lineIndex This parameter specifies the indentation value that this
 *                  function compares to the current indentation.
 *
 * @param type This value specifies the block collection type that
 *             `lineIndex` might start.
 *
 * @retval true If the function added an indentation value
 *         false Otherwise
 */
bool Lexer::addIndentation (size_t const lineIndex, Level::Type type)
{
	if (lineIndex > levels.top ().indent)
	{
		ELEKTRA_LOG_DEBUG ("Add indentation %zu", lineIndex);
		levels.push (Level{ lineIndex, type });
		return true;
	}
	return false;
}

/**
 * @brief This function checks if the lexer needs to scan additional tokens.
 *
 * @retval true If the lexer should fetch additional tokens
 * @retval false Otherwise
 */
bool Lexer::needMoreTokens () const
{
	if (done)
	{
		return false;
	}

	bool keyCandidateExists = simpleKey.first != nullptr;
	return keyCandidateExists || tokens.empty ();
}

/**
 * @brief This method removes uninteresting characters from the input.
 */
void Lexer::scanToNextToken ()
{
	ELEKTRA_LOG_DEBUG ("Scan to next token");
	bool found = false;
	while (!found)
	{
		while (input.LA (1) == ' ')
		{
			forward ();
		}
		ELEKTRA_LOG_DEBUG ("Skipped whitespace");
		if (input.LA (1) == '\n')
		{
			forward ();
			ELEKTRA_LOG_DEBUG ("Skipped newline");
		}
		else
		{
			found = true;
			ELEKTRA_LOG_DEBUG ("Found next token");
		}
	}
}

/**
 * @brief This method adds new tokens to the token queue.
 */
void Lexer::fetchTokens ()
{
	scanToNextToken ();
	location.step ();
	addBlockEnd (location.begin.column);
	ELEKTRA_LOG_DEBUG ("Fetch new token at location: %u:%u", location.begin.line, location.begin.column);

	if (input.LA (1) == 0)
	{
		scanEnd ();
		return;
	}
	else if (isValue ())
	{
		scanValue ();
		return;
	}
	else if (isElement ())
	{
		scanElement ();
		return;
	}
	else if (input.LA (1) == '"')
	{
		scanDoubleQuotedScalar ();
		return;
	}
	else if (input.LA (1) == '\'')
	{
		scanSingleQuotedScalar ();
		return;
	}
	else if (input.LA (1) == '#')
	{
		scanComment ();
		return;
	}

	scanPlainScalar ();
}

/**
 * @brief This method checks if the input at the specified offset starts a key
 *        value token.
 *
 * @param offset This parameter specifies an offset to the current position,
 *               where this function will look for a key value token.
 *
 * @retval true If the input matches a key value token
 * @retval false Otherwise
 */
bool Lexer::isValue (size_t const offset) const
{
	return (input.LA (offset) == ':') && (input.LA (offset + 1) == '\n' || input.LA (offset + 1) == ' ' || input.LA (offset + 1) == 0);
}

/**
 * @brief This method checks if the current input starts a list element.
 *
 * @retval true If the input matches a list element token
 * @retval false Otherwise
 */
bool Lexer::isElement () const
{
	return (input.LA (1) == '-') && (input.LA (2) == '\n' || input.LA (2) == ' ');
}

/**
 * @brief This method checks if the input at the specified offset starts a line
 *        comment.
 *
 * @param offset This parameter specifies an offset to the current position,
 *               where this function will look for a comment token.
 *
 * @retval true If the input matches a comment token
 * @retval false Otherwise
 */
bool Lexer::isComment (size_t const offset) const
{
	return (input.LA (offset) == '#') && (input.LA (offset + 1) == '\n' || input.LA (offset + 1) == ' ');
}

/**
 * @brief This method saves a token for a simple key candidate located at the
 *        current input position.
 */
void Lexer::addSimpleKeyCandidate ()
{
	size_t position = tokens.size () + tokensEmitted;
	simpleKey = make_pair (unique_ptr<Symbol> (new Symbol{ token::KEY, location, "KEY" }), position);
}

/**
 * @brief This method adds block closing tokens to the token queue, if the
 *        indentation decreased.
 *
 * @param lineIndex This parameter specifies the column (indentation in number
 *                  of spaces) for which this method should add block end
 *                  tokens.
 */
void Lexer::addBlockEnd (size_t const lineIndex)
{
	while (lineIndex < levels.top ().indent)
	{
		ELEKTRA_LOG_DEBUG ("Add block end");
		tokens.push_back (levels.top ().type == Level::Type::MAP ? Symbol (token::MAP_END, location, "MAP END") :
									   Symbol (token::SEQUENCE_END, location, "SEQUENCE END"));
		levels.pop ();
	}
}

/**
 * @brief This method adds the token for the start of the YAML stream to
 *        `tokens`.
 */
void Lexer::scanStart ()
{
	ELEKTRA_LOG_DEBUG ("Scan start token");
	tokens.push_back (Symbol (token::STREAM_START, location, "STREAM START"));
}

/**
 * @brief This method adds the token for the end of the YAML stream to
 *        the token queue.
 */
void Lexer::scanEnd ()
{
	ELEKTRA_LOG_DEBUG ("Scan end token");
	addBlockEnd (0);
	tokens.push_back (Symbol (token::STREAM_END, location, "STREAM END"));
	tokens.push_back (Symbol (token::END, location));
	done = true;
}

/**
 * @brief This method scans a single quoted scalar and adds it to the token
 *        queue.
 */
void Lexer::scanSingleQuotedScalar ()
{
	ELEKTRA_LOG_DEBUG ("Scan single quoted scalar");

	size_t start = input.index ();
	// A single quoted scalar can start a simple key
	addSimpleKeyCandidate ();

	forward (); // Include initial single quote
	while (input.LA (1) != '\'' || input.LA (2) == '\'')
	{
		forward ();
	}
	forward (); // Include closing single quote
	tokens.push_back (Symbol (token::SINGLE_QUOTED_SCALAR, location, input.getText (start)));
}

/**
 * @brief This method scans a double quoted scalar and adds it to the token
 *        queue.
 */
void Lexer::scanDoubleQuotedScalar ()
{
	ELEKTRA_LOG_DEBUG ("Scan double quoted scalar");
	size_t start = input.index ();

	// A double quoted scalar can start a simple key
	addSimpleKeyCandidate ();

	forward (); // Include initial double quote
	while (input.LA (1) != '"')
	{
		forward ();
	}
	forward (); // Include closing double quote
	tokens.push_back (Symbol (token::DOUBLE_QUOTED_SCALAR, location, input.getText (start)));
}

/**
 * @brief This method scans a plain scalar and adds it to the token queue.
 */
void Lexer::scanPlainScalar ()
{
	ELEKTRA_LOG_DEBUG ("Scan plain scalar");
	// A plain scalar can start a simple key
	addSimpleKeyCandidate ();

	size_t lengthSpace = 0;
	size_t start = input.index ();

	size_t lengthNonSpace;
	while ((lengthNonSpace = countPlainNonSpace (lengthSpace)) > 0)
	{
		forward (lengthSpace + lengthNonSpace);
		lengthSpace = countPlainSpace ();
	}

	tokens.push_back (Symbol (token::PLAIN_SCALAR, location, input.getText (start)));
}

/**
 * @brief This method counts the number of non space characters that can be part
 *        of a plain scalar at position `offset`.
 *
 * @param offset This parameter specifies an offset to the current input
 *               position, where this function searches for non space
 *               characters.
 *
 * @return The number of non-space characters at the input position `offset`
 */
size_t Lexer::countPlainNonSpace (size_t const offset) const
{
	ELEKTRA_LOG_DEBUG ("Scan non space characters");
	string const stop = " \n";

	size_t lookahead = offset + 1;
	while (stop.find (input.LA (lookahead)) == string::npos && input.LA (lookahead) != 0 && !isValue (lookahead) &&
	       !isComment (lookahead))
	{
		lookahead++;
	}

	ELEKTRA_LOG_DEBUG ("Found %zu non-space characters", lookahead - offset - 1);
	return lookahead - offset - 1;
}

/**
 * @brief This method counts the number of space characters that can be part
 *        of a plain scalar at the current input position.
 *
 * @return The number of space characters at the current input position
 */
size_t Lexer::countPlainSpace () const
{
	ELEKTRA_LOG_DEBUG ("Scan spaces");
	size_t lookahead = 1;
	while (input.LA (lookahead) == ' ')
	{
		lookahead++;
	}
	ELEKTRA_LOG_DEBUG ("Found %zu space characters", lookahead - 1);
	return lookahead - 1;
}

/**
 * @brief This method scans a comment and adds it to the token queue.
 */
void Lexer::scanComment ()
{
	ELEKTRA_LOG_DEBUG ("Scan comment");
	size_t start = input.index ();
	while (input.LA (1) != '\n' && input.LA (1) != 0)
	{
		forward ();
	}
	tokens.push_back (Symbol (token::COMMENT, location, input.getText (start)));
}

/**
 * @brief This method scans a mapping value token and adds it to the token
 *        queue.
 */
void Lexer::scanValue ()
{
	ELEKTRA_LOG_DEBUG ("Scan value");
	forward (1);
	tokens.push_back (Symbol (token::VALUE, location, input.getText (input.index () - 1)));
	if (input.LA (1)) forward (1);
	if (simpleKey.first == nullptr)
	{
		throw runtime_error ("Unable to locate key for value");
	}
	size_t offset = simpleKey.second - tokensEmitted;
	tokens.insert (tokens.begin () + offset, *simpleKey.first);
	auto mapStartLocation = simpleKey.first->getLocation ();
	simpleKey.first = nullptr; // Remove key candidate
	if (addIndentation (mapStartLocation.begin.column, Level::Type::MAP))
	{
		mapStartLocation.end = mapStartLocation.begin;
		tokens.insert (tokens.begin () + offset, Symbol (token::MAP_START, mapStartLocation, "MAP START"));
	}
}

/**
 * @brief This method scans a list element token and adds it to the token
 *        queue.
 */
void Lexer::scanElement ()
{
	ELEKTRA_LOG_DEBUG ("Scan element");
	if (addIndentation (location.end.column, Level::Type::SEQUENCE))
	{
		tokens.push_back (Symbol (token::SEQUENCE_START, location, "SEQUENCE START"));
	}
	forward (1);
	tokens.push_back (Symbol (token::ELEMENT, location, input.getText (input.index () - 1)));
	forward (1);
}

/**
 * @brief This constructor initializes a lexer with the given input.
 *
 * @param stream This stream specifies the text which this lexer analyzes.
 */
Lexer::Lexer (ifstream & stream) : input{ stream }
{
	ELEKTRA_LOG_DEBUG ("Init lexer");

	scanStart ();
	fetchTokens ();
}

/**
 * @brief This method returns the next token the lexer produced from `input`.
 *
 * @return The next token the parser has not emitted yet
 */
Parser::symbol_type Lexer::nextToken ()
{
	while (needMoreTokens ())
	{
		fetchTokens ();
	}
#ifdef HAVE_LOGGER
	string output;
	ELEKTRA_LOG_DEBUG ("Tokens:");
	for (auto symbol : tokens)
	{
		ELEKTRA_LOG_DEBUG ("\t%s", symbol.toString ().c_str ());
	}
	ELEKTRA_LOG_DEBUG ("%s", output.c_str ());
#endif

	// If `fetchTokens` was unable to retrieve a token (error condition), we emit
	// an end token.
	if (tokens.size () <= 0)
	{
		tokens.push_back (Symbol (token::END, location));
	}
	Symbol symbol = tokens.front ();
	tokens.pop_front ();
	tokensEmitted++;
	return symbol.get ();
}

/**
 * @brief This method returns the current input of the lexer
 *
 * @return A UTF-8 encoded string version of the parser input
 */
string Lexer::getText ()
{
	return input.toString ();
}
