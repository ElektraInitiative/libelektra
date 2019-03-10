/**
 * @file
 *
 * @brief This file contains the declaration of a basic YAML lexer.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAWN_LEXER_HPP
#define ELEKTRA_PLUGIN_YAWN_LEXER_HPP

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <deque>
#include <fstream>
#include <memory>
#include <stack>

#include "input.hpp"
#include "token.hpp"

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/** This class implements a basic YAML lexer. */
class Lexer
{
	/** This class stores information about indentation that starts a new block node. */
	class Level
	{
	public:
		/** This enumeration specifies the type of a block node. */
		enum class Type
		{
			MAP,      ///< The current indentation starts a block map
			SEQUENCE, ///< The current indentation starts a block sequence
			OTHER     ///< The current indentation starts a block scalar
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
	Input input;

	/** This queue stores the list of tokens produced by the lexer. */
	std::deque<std::unique_ptr<Token>> tokens;

	/**
	 * This variable stores tokens already emitted by the parser. We keep this
	 * list, since the syntax tree produced by YAEP only stores references to
	 * tokens. As a consequence we need to keep the referenced tokens in
	 * memory, as long as other code accesses the token attributes of the syntax
	 * tree.
	 */
	std::deque<std::unique_ptr<Token>> emitted;

	/** This variable stores the current line and column number. */
	Location location;

	/**
	 * This stack stores the indentation (in number of characters) and block
	 * type for each block node.
	 */
	std::stack<Level> levels{ std::deque<Level>{ 0 } };

	/**
	 * This pair stores a simple key candidate token (first part) and its
	 * position in the token queue (second part).
	 *
	 * Since the lexer only supports block syntax for mappings and sequences we
	 * use a single token here. If we need support for flow collections we have
	 * to store a candidate for each flow level (block context = flow level 0).
	 */
	std::pair<std::unique_ptr<Token>, size_t> simpleKey;

	/**
	 * This boolean specifies if the lexer has already scanned the whole input or
	 * not.
	 */
	bool done = false;

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
	bool isValue (size_t const offset = 1) const;

	/**
	 * @brief This method checks if the current input starts a list element.
	 *
	 * @retval true If the input matches a list element token
	 * @retval false Otherwise
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
	 * @retval false Otherwise
	 */
	bool isComment (size_t const offset) const;

	/**
	 * @brief This method consumes characters from the input stream keeping
	 *        track of line and column numbers.
	 *
	 * @param characters This parameter specifies the number of characters the
	 *                   the function should consume.
	 */
	void forward (size_t const characters);

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
	 * @brief This method saves a token for a simple key candidate located at the
	 *        current input position.
	 */
	void addSimpleKeyCandidate ();

	/**
	 * @brief This method removes uninteresting characters from the input.
	 */
	void scanToNextToken ();

	/**
	 * @brief This method checks if the lexer needs to scan additional tokens.
	 *
	 * @retval true If the lexer should fetch additional tokens
	 * @retval false Otherwise
	 */
	bool needMoreTokens () const;

	/**
	 * @brief This method adds new tokens to the token queue.
	 */
	void fetchTokens ();

	/**
	 * @brief This method adds the token for the start of the YAML stream to
	 *        the token queue.
	 */
	void scanStart ();

	/**
	 * @brief This method adds the token for the end of the YAML stream to
	 *        the token queue.
	 */
	void scanEnd ();

	/**
	 * @brief This method scans a single quoted scalar and adds it to the token
	 *        queue.
	 */
	void scanSingleQuotedScalar ();

	/**
	 * @brief This method scans a double quoted scalar and adds it to the token
	 *        queue.
	 */
	void scanDoubleQuotedScalar ();

	/**
	 * @brief This method scans a plain scalar and adds it to the token queue.
	 */
	void scanPlainScalar ();

	/**
	 * @brief This method counts the number of non space characters that can be
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
	/**
	 * @brief This constructor initializes a lexer with the given input.
	 *
	 * @param stream This stream specifies the text which this lexer analyzes.
	 */
	Lexer (std::ifstream & stream);

	/**
	 * @brief This method returns the next token produced by the lexer.
	 *
	 * If the lexer found the end of the input, then this function returns `-1`.
	 *
	 * @param attribute The parser uses this parameter to store auxiliary data for
	 *                  the returned token.
	 *
	 * @return A number specifying the type of the first token the parser has not
	 *         emitted yet
	 */
	int nextToken (void ** attribute);

	/**
	 * @brief This method returns the current input of the lexer
	 *
	 * @return A UTF-8 encoded string version of the parser input
	 */
	std::string getText ();
};

} // namespace yawn

#endif // ELEKTRA_PLUGIN_YAWN_LEXER_HPP
