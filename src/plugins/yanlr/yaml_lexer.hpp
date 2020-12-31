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

#include <antlr4-runtime/antlr4-runtime.h>

#include <kdblogger.h>

using std::deque;
using std::pair;
using std::shared_ptr;
using std::stack;
using std::string;
using std::unique_ptr;

namespace yanlr
{

// -- Class --------------------------------------------------------------------

class YAMLLexer : public antlr4::TokenSource
{
	/** This class stores information about indentation that starts a new block node. */
	class Level
	{
	public:
		/** This enumeration specifies the type of a block node. */
		enum class Type
		{
			MAP,	  ///< The current indentation starts a block map
			SEQUENCE, ///< The current indentation starts a block sequence
			OTHER	  ///< The current indentation starts a block scalar
		};
		size_t indent = 0;
		Type type = Level::Type::OTHER;

		/**
		 * @brief This constructor creates a level object from the given arguments.
		 *
		 * @param indentation This number specifies the number of spaces used to start this level object.
		 * @param levelType This argument specifies the type of node `indentation` created.
		 */
		Level (size_t indentation, Level::Type levelType = Level::Type::OTHER) : indent{ indentation }, type{ levelType }
		{
		}
	};

	/** This structure represents the position inside the input. */
	struct Position
	{
		/** This parameter stores the offset to the start of the input in bytes. */
		size_t index;
		/** This parameter stores the line number. */
		size_t line;
		/** This parameter stores the column offset inside `line`. */
		size_t column;

		/**
		 * @brief This constructor creates a position from the given arguments.
		 *
		 * @param byteIndex This number specifies the byte offset of the position relative to the start of the input.
		 * @param lineNumber This number specifies the line number of the position.
		 * @param columnOffset This number specifies the offset to the beginning of the line.
		 */
		Position (size_t byteIndex, size_t lineNumber, size_t columnOffset);
	};

	/** This variable stores the input that this lexer scans. */
	antlr4::CharStream * input;

	/** This queue stores the list of tokens produced by the lexer. */
	deque<unique_ptr<antlr4::CommonToken>> tokens;

	/** The lexer uses this factory to produce tokens. */
	antlr4::TokenFactory<antlr4::CommonToken> * factory = antlr4::CommonTokenFactory::DEFAULT.get ();

	/** This pair stores the token source (this lexer) and the current `input`. */
	pair<antlr4::TokenSource *, antlr4::CharStream *> source;

	/**
	 * This variable saves the current line position of the lexer inside
	 * `input`.
	 */
	size_t line = 1;

	/**
	 * This number stores the current character position of the lexer inside of
	 * `line`.
	 */
	size_t column = 1;

	/**
	 * This counter stores the number of tokens already emitted by the lexer.
	 * The lexer needs this variable, to keep track of the insertion point of
	 * `KEY` tokens in the token queue.
	 */
	size_t tokensEmitted = 0;

	/**
	 * This stack stores the indentation (in number of characters) and block
	 * type for each block node.
	 */
	stack<Level> levels{ deque<Level>{ Level{ 0 } } };

	/**
	 * This boolean specifies if the lexer has already scanned the whole input or
	 * not.
	 */
	bool done = false;

	/**
	 * This pair stores a simple key candidate token (first part) and its
	 * position in the token queue (second part).
	 *
	 * Since the lexer only supports block syntax for mappings and sequences we
	 * use a single token here. If we need support for flow collections we have
	 * to store a candidate for each flow level (block context = flow level 0).
	 */
	pair<unique_ptr<antlr4::CommonToken>, size_t> simpleKey;

	/**
	 * @brief This function returns the current position of the lexer inside the input.
	 *
	 * @return A position containing the current byte index, line number and column offset.
	 */
	Position getPosition ();

	/**
	 * @brief This function creates a new token with the specified parameters.
	 *
	 * @param type This parameter specifies the type of the token this function
	 *             should create.
	 * @param start This variable specifies the start position of the returned token
	 *              inside the character stream `input`.
	 * @param stop This number specifies the stop index of the returned token
	 *             inside the character stream `input`.
	 * @param text This string specifies the text of the returned token.
	 *
	 * @return A token with the specified parameters
	 */
	unique_ptr<antlr4::CommonToken> commonToken (size_t type, Position const & start, size_t stop, string text);

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
	bool addIndentation (size_t const column, Level::Type type);

	/**
	 * @brief This function checks if the lexer needs to scan additional tokens.
	 *
	 * @retval true If the lexer should fetch additional tokens
	 *         false Otherwise
	 */
	bool needMoreTokens () const;

	/**
	 * @brief This method adds new tokens to the token stream.
	 */
	void fetchTokens ();

	/**
	 * @brief This method consumes characters from the input stream keeping
	 *        track of line and column numbers.
	 *
	 * @param characters This parameter specifies the number of characters the
	 *                   the function should consume.
	 */
	void forward (size_t const characters);

	/**
	 * @brief This method removes uninteresting characters from the input.
	 */
	void scanToNextToken ();

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
	bool isValue (size_t const offset = 1) const;

	/**
	 * @brief This method checks if the current input starts a list element.
	 *
	 * @retval true If the input matches a list element token
	 *         false Otherwise
	 */
	bool isElement () const;

	/**
	 * @brief This method checks if the input at the specified offset starts a
	 *        line comment.
	 *
	 * @param offset This parameter specifies an offset to the current position,
	 *               where this function will look for a comment token.
	 *
	 * @retval true If the input matches a comment token
	 *         false Otherwise
	 */
	bool isComment (size_t const offset) const;

	/**
	 * @brief This method saves a token for a simple key candidate located at the
	 *        current input position.
	 */
	void addSimpleKeyCandidate ();

	/**
	 * @brief This method adds block closing tokens to the token queue, if the
	 *        indentation decreased.
	 *
	 * @param lineIndex This parameter specifies the column (indentation in number
	 *                  of spaces) for which this method should add block end
	 *                  tokens.
	 */
	void addBlockEnd (size_t const lineIndex);

	/**
	 * @brief This method adds the token for the start of the YAML stream to
	 *        `tokens`.
	 */
	void scanStart ();

	/**
	 * @brief This method adds the end markers to the token queue.
	 */
	void scanEnd ();

	/**
	 * @brief This method scans a single quoted scalar and adds it to the token
	 *        queue.
	 */
	void scanSingleQuotedScalar ();

	/**
	 * @brief This method scans a plain scalar and adds it to the token queue.
	 */
	void scanPlainScalar ();

	/**
	 * @brief This method counts the number of non-space characters that can be
	 *        part of a plain scalar at position `offset`.
	 *
	 * @param offset This parameter specifies an offset to the current input
	 *               position, where this function searches for non-space
	 *               characters.
	 *
	 * @return The number of non-space characters at the input position `offset`
	 */
	size_t countPlainNonSpace (size_t const offset) const;

	/**
	 * @brief This method counts the number of space characters that can be part
	 *        of a plain scalar at the current input position.
	 *
	 * @return The number of space characters at the current input position
	 */
	size_t countPlainSpace () const;

	/**
	 * @brief This method scans a comment and adds it to the token queue.
	 */
	void scanComment ();

	/**
	 * @brief This method scans a double quoted scalar and adds it to the token
	 *        queue.
	 */
	void scanDoubleQuotedScalar ();

	/**
	 * @brief This method scans a mapping value token and adds it to the token
	 *        queue.
	 */
	void scanValue ();

	/**
	 * @brief This method scans a list element token and adds it to the token
	 *        queue.
	 */
	void scanElement ();

public:
	/** This token type starts the YAML stream. */
	static const size_t STREAM_START = 1;
	/** This token type ends the YAML stream. */
	static const size_t STREAM_END = 2;
	/** This token type specifies that the token stores a (line) comment. */
	static const size_t COMMENT = 3;
	/** This token type specifies that the token stores a plain scalar. */
	static const size_t PLAIN_SCALAR = 4;
	/** This token type specifies that the token stores a single quoted scalar. */
	static const size_t SINGLE_QUOTED_SCALAR = 5;
	/** This token type specifies that the token stores a double quoted scalar. */
	static const size_t DOUBLE_QUOTED_SCALAR = 6;
	/** This token type indicates the start of a mapping. */
	static const size_t MAP_START = 7;
	/** This token type indicates the end of a mapping. */
	static const size_t MAP_END = 8;
	/** This token type indicates the start of a mapping key. */
	static const size_t KEY = 9;
	/** This token type indicates the start of a mapping value. */
	static const size_t VALUE = 10;
	/** This token type indicates the start of a sequence. */
	static const size_t SEQUENCE_START = 11;
	/** This token type indicates the end of a sequence. */
	static const size_t SEQUENCE_END = 12;
	/** This token type indicates a list element. */
	static const size_t ELEMENT = 13;

	/**
	 * @brief This constructor creates a new YAML lexer for the given input.
	 *
	 * @param stream This character stream stores the data this lexer scans.
	 */
	YAMLLexer (antlr4::CharStream * stream);

	/**
	 * @brief This method retrieves the current (not already emitted) token
	 *        produced by the lexer.
	 *
	 * @return A token of the token stream produced by the lexer
	 */
	unique_ptr<antlr4::Token> nextToken () override;

	/**
	 * @brief This method retrieves the current line index.
	 *
	 * @return The index of the line the lexer is currently scanning
	 */
	size_t getLine () const override;

	/**
	 * @brief This method returns the position in the current line.
	 *
	 * @return The character index in the line the lexer is scanning
	 */
	size_t getCharPositionInLine () override;

	/**
	 * @brief This method returns the source the lexer is scanning.
	 *
	 * @return The input of the lexer
	 */
	antlr4::CharStream * getInputStream () override;

	/**
	 * @brief This method retrieves the name of the source the lexer is
	 * currently scanning.
	 *
	 * @return The name of the current input source
	 */
	std::string getSourceName () override;

	/**
	 * @brief This setter changes the token factory of the lexer.
	 *
	 * @param tokenFactory This parameter specifies the factory that the scanner
	 *                     should use to create tokens.
	 */
	template <typename T1>
	void setTokenFactory (antlr4::TokenFactory<T1> * tokenFactory);

	/**
	 * @brief Retrieve the current token factory.
	 *
	 * @return The factory the scanner uses to create tokens
	 */
	antlr4::TokenFactory<antlr4::CommonToken> * getTokenFactory () override;
};
}
