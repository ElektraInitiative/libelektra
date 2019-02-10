/**
 * @file
 *
 * @brief This file specifies a lexer that scans a subset of YAML.
 *
 * The lexer uses the same idea as the scanner of `libyaml` (and various other
 * YAML libs) to detect simple keys (keys with no `?` prefix).
 *
 * For a detailed explanation of the algorithm, I recommend to take a look at
 * the scanner of
 *
 * - SnakeYAML Engine:
 *   https://bitbucket.org/asomov/snakeyaml-engine
 * - or LLVM’s YAML library:
 *   https://github.com/llvm-mirror/llvm/blob/master/lib/Support/YAMLParser.cpp
 *
 * .
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include "yaml_lexer.hpp"

using std::make_pair;

using antlr4::ParseCancellationException;

// -- Class --------------------------------------------------------------------

/**
 * @brief This constructor creates a new YAML lexer for the given input.
 *
 * @param input This character stream stores the data this lexer scans.
 */
YAMLLexer::YAMLLexer (CharStream * input)
{
	this->input = input;
	this->source = make_pair (this, input);
	scanStart ();
}

/**
 * @brief This function checks if the lexer needs to scan additional tokens.
 *
 * @retval true If the lexer should fetch additional tokens
 *         false Otherwise
 */
bool YAMLLexer::needMoreTokens () const
{
	if (done) return false;

	bool keyCandidateExists = simpleKey.first != nullptr;
	return keyCandidateExists || tokens.empty ();
}

/**
 * @brief This method retrieves the current (not already emitted) token
 *        produced by the lexer.
 *
 * @return A token of the token stream produced by the lexer
 */
unique_ptr<Token> YAMLLexer::nextToken ()
{
	ELEKTRA_LOG_DEBUG ("Retrieve next token");
	while (needMoreTokens ())
	{
		fetchTokens ();
#ifdef HAVE_LOGGER
		ELEKTRA_LOG_DEBUG ("Tokens:");
		for (unique_ptr<CommonToken> const & token : tokens)
		{
			ELEKTRA_LOG_DEBUG ("\t %s", token->toString ().c_str ());
		}
#endif
	}

	// If `fetchTokens` was unable to retrieve a token (error condition), we emit `EOF`.
	if (tokens.size () <= 0)
	{
		tokens.push_back (commonToken (Token::EOF, input->index (), input->index (), "EOF"));
	}
	unique_ptr<CommonToken> token = move (tokens.front ());
	tokens.pop_front ();
	tokensEmitted++;
	ELEKTRA_LOG_DEBUG ("Emit token %s", token->toString ().c_str ());
	return token;
}

/**
 * @brief This method retrieves the current line index.
 *
 * @return The index of the line the lexer is currently scanning
 */
size_t YAMLLexer::getLine () const
{
	return line;
}

/**
 * @brief This method returns the position in the current line.
 *
 * @return The character index in the line the lexer is scanning
 */
size_t YAMLLexer::getCharPositionInLine ()
{
	return column;
}

/**
 * @brief This method returns the source the lexer is scanning.
 *
 * @return The input of the lexer
 */
CharStream * YAMLLexer::getInputStream ()
{
	return input;
}

/**
 * @brief This method retrieves the name of the source the lexer is currently
 *        scanning.
 *
 * @return The name of the current input source
 */
std::string YAMLLexer::getSourceName ()
{
	return input->getSourceName ();
}

/**
 * @brief This setter changes the token factory of the lexer.
 *
 * @param tokenFactory This parameter specifies the factory that the scanner
 *                     should use to create tokens.
 */
template <typename T1>
void YAMLLexer::setTokenFactory (TokenFactory<T1> * tokenFactory)
{
	factory = tokenFactory;
}

/**
 * @brief Retrieve the current token factory.
 *
 * @return The factory the scanner uses to create tokens
 */
Ref<TokenFactory<CommonToken>> YAMLLexer::getTokenFactory ()
{
	return factory;
}

// ===========
// = Private =
// ===========

/**
 * @brief This function creates a new token with the specified parameters.
 *
 * @param type This parameter specifies the type of the token this function
 *             should create.
 * @param start This number specifies the start index of the returned token
 *              inside the character stream `input`.
 * @param stop This number specifies the stop index of the returned token
 *             inside the character stream `input`.
 *
 * @return A token with the specified parameters
 */
unique_ptr<CommonToken> YAMLLexer::commonToken (size_t type, size_t start, size_t stop)
{
	return factory->create (source, type, "", Token::DEFAULT_CHANNEL, start, stop, line, column);
}

/**
 * @brief This function creates a new token with the specified parameters.
 *
 * @param type This parameter specifies the type of the token this function
 *             should create.
 * @param start This number specifies the start index of the returned token
 *              inside the character stream `input`.
 * @param stop This number specifies the stop index of the returned token
 *             inside the character stream `input`.
 * @param text This string specifies the text of the returned token.
 *
 * @return A token with the specified parameters
 */
unique_ptr<CommonToken> YAMLLexer::commonToken (size_t type, size_t start, size_t stop, string text)
{
	return factory->create (source, type, text, Token::DEFAULT_CHANNEL, start, stop, line, column);
}

/**
 * @brief This function adds an indentation value if the given value is smaller
 *        than the current indentation.
 *
 * @param lineIndex This parameter specifies the indentation value that this
 *                  function compares to the current indentation.
 *
 * @retval true If the function added an indentation value
 *         false Otherwise
 */
bool YAMLLexer::addIndentation (size_t const lineIndex, Level::Type type)
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
 * @brief This method adds new tokens to the token stream.
 */
void YAMLLexer::fetchTokens ()
{
	scanToNextToken ();

	addBlockEnd (column);

	if (input->LA (1) == Token::EOF)
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
	else if (input->LA (1) == '"')
	{
		scanDoubleQuotedScalar ();
		return;
	}
	else if (input->LA (1) == '\'')
	{
		scanSingleQuotedScalar ();
		return;
	}
	else if (input->LA (1) == '#')
	{
		scanComment ();
		return;
	}

	scanPlainScalar ();
}

/**
 * @brief This method consumes characters from the input stream keeping
 *        track of line and column numbers.
 *
 * @param characters This parameter specifies the number of characters the
 *                   the function should consume.
 */
void YAMLLexer::forward (size_t const characters = 1)
{
	ELEKTRA_LOG_DEBUG ("Forward %zu characters", characters);

	for (size_t charsLeft = characters; charsLeft > 0; charsLeft--)
	{
		if (input->LA (1) == Token::EOF)
		{
			ELEKTRA_LOG_DEBUG ("Hit EOF!");
			return;
		}

		column++;
		if (input->LA (1) == '\n')
		{
			column = 1;
			line++;
		}
		input->consume ();
	}
}

/**
 * @brief This method removes uninteresting characters from the input.
 */
void YAMLLexer::scanToNextToken ()
{
	ELEKTRA_LOG_DEBUG ("Scan to next token");
	bool found = false;
	while (!found)
	{
		while (input->LA (1) == ' ')
		{
			forward ();
		}
		ELEKTRA_LOG_DEBUG ("Skipped whitespace");
		if (input->LA (1) == '\n')
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
 * @brief This method checks if the input at the specified offset starts a key
 *        value token.
 *
 * @param offset This parameter specifies an offset to the current position,
 *               where this function will look for a key value token.
 *
 * @retval true If the input matches a key value token
 *         false Otherwise
 */
bool YAMLLexer::isValue (size_t const offset) const
{
	return (input->LA (offset) == ':') && (input->LA (offset + 1) == '\n' || input->LA (offset + 1) == ' ');
}

/**
 * @brief This method checks if the current input starts a list element.
 *
 * @retval true If the input matches a list element token
 *         false Otherwise
 */
bool YAMLLexer::isElement () const
{
	return (input->LA (1) == '-') && (input->LA (2) == '\n' || input->LA (2) == ' ');
}

/**
 * @brief This method checks if the input at the specified offset starts a line
 *        comment.
 *
 * @param offset This parameter specifies an offset to the current position,
 *               where this function will look for a comment token.
 *
 * @retval true If the input matches a comment token
 *         false Otherwise
 */
bool YAMLLexer::isComment (size_t const offset) const
{
	return (input->LA (offset) == '#') && (input->LA (offset + 1) == '\n' || input->LA (offset + 1) == ' ');
}

/**
 * @brief This method saves a token for a simple key candidate located at the
 *        current input position.
 */
void YAMLLexer::addSimpleKeyCandidate ()
{
	size_t position = tokens.size () + tokensEmitted;
	size_t index = input->index ();
	simpleKey = make_pair (commonToken (KEY, index, index, "KEY"), position);
}

/**
 * @brief This method adds block closing tokens to the token queue, if the
 *        indentation decreased.
 *
 * @param lineIndex This parameter specifies the column (indentation in number
 *                  of spaces) for which this method should add block end
 *                  tokens.
 */
void YAMLLexer::addBlockEnd (size_t const lineIndex)
{
	while (lineIndex < levels.top ().indent)
	{
		ELEKTRA_LOG_DEBUG ("Add block end");
		size_t index = input->index ();
		tokens.push_back (levels.top ().type == Level::Type::MAP ? commonToken (MAP_END, index, index, "MAP END") :
									   commonToken (SEQUENCE_END, index, index, "SEQUENCE END"));
		levels.pop ();
	}
}

/**
 * @brief This method adds the token for the start of the YAML stream to
 *        `tokens`.
 */
void YAMLLexer::scanStart ()
{
	ELEKTRA_LOG_DEBUG ("Scan start");
	auto start = commonToken (STREAM_START, input->index (), input->index (), "START");
	tokens.push_back (move (start));
}

/**
 * @brief This method adds the end markers to the token queue.
 */
void YAMLLexer::scanEnd ()
{
	addBlockEnd (0);
	tokens.push_back (commonToken (STREAM_END, input->index (), input->index (), "END"));
	tokens.push_back (commonToken (Token::EOF, input->index (), input->index (), "EOF"));
	done = true;
}

/**
 * @brief This method scans a single quoted scalar and adds it to the token
 *        queue.
 */
void YAMLLexer::scanSingleQuotedScalar ()
{
	ELEKTRA_LOG_DEBUG ("Scan single quoted scalar");

	size_t start = input->index ();
	// A single quoted scalar can start a simple key
	addSimpleKeyCandidate ();

	forward (); // Include initial single quote
	while (input->LA (1) != '\'' || input->LA (2) == '\'')
	{
		forward ();
	}
	forward (); // Include closing single quote
	tokens.push_back (commonToken (SINGLE_QUOTED_SCALAR, start, input->index () - 1));
}

/**
 * @brief This method scans a double quoted scalar and adds it to the token
 *        queue.
 */
void YAMLLexer::scanDoubleQuotedScalar ()
{
	ELEKTRA_LOG_DEBUG ("Scan double quoted scalar");
	size_t start = input->index ();

	// A double quoted scalar can start a simple key
	addSimpleKeyCandidate ();

	forward (); // Include initial double quote
	while (input->LA (1) != '"')
	{
		forward ();
	}
	forward (); // Include closing double quote
	tokens.push_back (commonToken (DOUBLE_QUOTED_SCALAR, start, input->index () - 1));
}

/**
 * @brief This method scans a plain scalar and adds it to the token queue.
 */
void YAMLLexer::scanPlainScalar ()
{
	ELEKTRA_LOG_DEBUG ("Scan plain scalar");
	size_t start = input->index ();
	// A plain scalar can start a simple key
	addSimpleKeyCandidate ();

	size_t lengthSpace = 0;
	size_t lengthNonSpace = 0;
	while (true)
	{
		lengthNonSpace = countPlainNonSpace (lengthSpace);
		if (lengthNonSpace == 0)
		{
			break;
		}
		forward (lengthSpace + lengthNonSpace);
		lengthSpace = countPlainSpace ();
	}

	tokens.push_back (commonToken (PLAIN_SCALAR, start, input->index () - 1));
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
size_t YAMLLexer::countPlainNonSpace (size_t const offset) const
{
	ELEKTRA_LOG_DEBUG ("Scan non space characters");
	string const stop = " \n";

	size_t lookahead = offset + 1;
	while (stop.find (input->LA (lookahead)) == string::npos && input->LA (lookahead) != Token::EOF && !isValue (lookahead) &&
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
size_t YAMLLexer::countPlainSpace () const
{
	ELEKTRA_LOG_DEBUG ("Scan spaces");
	size_t lookahead = 1;
	while (input->LA (lookahead) == ' ')
	{
		lookahead++;
	}
	ELEKTRA_LOG_DEBUG ("Found %zu space characters", lookahead - 1);
	return lookahead - 1;
}

/**
 * @brief This method scans a comment and adds it to the token queue.
 */
void YAMLLexer::scanComment ()
{
	ELEKTRA_LOG_DEBUG ("Scan comment");
	size_t start = input->index ();

	while (input->LA (1) != '\n' && input->LA (1) != Token::EOF)
	{
		forward ();
	}
	tokens.push_back (commonToken (COMMENT, start, input->index () - 1));
}

/**
 * @brief This method scans a mapping value token and adds it to the token
 *        queue.
 */
void YAMLLexer::scanValue ()
{
	ELEKTRA_LOG_DEBUG ("Scan value");
	tokens.push_back (commonToken (VALUE, input->index (), input->index () + 1));
	forward (2);
	if (simpleKey.first == nullptr)
	{
		throw ParseCancellationException ("Unable to locate key for value");
	}
	size_t start = simpleKey.first->getCharPositionInLine ();
	tokens.insert (tokens.begin () + simpleKey.second - tokensEmitted, move (simpleKey.first));
	if (addIndentation (start, Level::Type::MAP))
	{
		tokens.push_front (commonToken (MAP_START, start, column, "MAP START"));
	}
}

/**
 * @brief This method scans a list element token and adds it to the token
 *        queue.
 */
void YAMLLexer::scanElement ()
{
	ELEKTRA_LOG_DEBUG ("Scan element");
	if (addIndentation (column, Level::Type::SEQUENCE))
	{
		tokens.push_back (commonToken (SEQUENCE_START, input->index (), column, "SEQUENCE START"));
	}
	tokens.push_back (commonToken (ELEMENT, input->index (), input->index () + 1));
	forward (2);
}
