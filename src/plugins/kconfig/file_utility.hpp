#ifndef ELEKTRA_FILEUTILITY_HPP
#define ELEKTRA_FILEUTILITY_HPP

#include "./kconfig_parser_exception.hpp"
#include <memory>
#include <sstream>

namespace kconfig
{
class FileUtility
{
private:
	/* This file is the file stream that we want to parse  */
	std::unique_ptr<std::istream> file;
	/* This stringBuffer is used when reading strings from file so that we don't initialize a stringstream each time */
	std::stringstream stringBuffer;
	/* This int is used to save the current line number for the purpose of better error messages */
	int currentLine;
	/* This string is used to save the filename for the purpose of better error messages */
	std::string filename;

public:
	/**
	 * @brief This constructor is used to abstract the file stream into the methods that are specific to parsing a kconfig configuration
	 * file
	 * @param filename This string contains the absolute/relative path to the file
	 */
	explicit FileUtility (std::string filenameParam, std::unique_ptr<std::istream> fileParam);

	/**
	 * \copydoc std::istream::peek()
	 */
	char peekNextChar ();

	/**
	 * @brief This method is used to check if the next character in the buffer is end of file
	 *
	 * @retval true if the next character is end of file
	 * @retval false otherwise
	 */
	bool isNextCharEOF ();

	/**
	 * @brief This method is used to check if the next character in the buffer is a new line
	 *
	 * @retval true if the next character is new line
	 * @retval false otherwise
	 */
	bool isNextCharNewline ();

	/**
	 * @brief This method is used to check if the next character in the buffer is a new line or end of file
	 *
	 * @retval true if the next character is new line or end of file
	 * @retval false otherwise
	 */
	bool isNextCharNewlineOrEOF ();

	/**
	 * @brief This method is used to check if the next character in the buffer is any of special characters used to specify a token
	 *
	 * @retval true if the next character is used to specify a token
	 * @retval false otherwise
	 */
	bool isNextCharToken ();

	/**
	 * @brief This method is used to consume a character from the buffer
	 */
	void skipChar ();

	/**
	 * @brief This method is used to consume a character from the buffer only if it is a blank character
	 */
	void skipCharsIfBlank ();

	/**
	 * @brief This method is used to skip all characters until (inclusive) the new line character
	 */
	void skipLine ();

	/**
	 * @brief This method is used to skip empty lines or lines that start with a `#` character
	 */
	void skipLineIfEmptyOrComment ();

	inline void readEscapedChar (std::ostream & str);

	/**
	 * @brief This method is used to read characters into a buffer until (exclusive) a given delimiter, new line or end of file
	 * @param str This output stream is used to read the characters into it
	 * @param delimiter This character is used to determine when to stop reading
	 */
	void readUntilChar (std::ostream & str, const char & delimiter);

	/**
	 * @brief This method is used to read characters into a buffer until (exclusive) any given delimiter, new line or end of file
	 * @param str This output stream is used to read the characters into it
	 * @param delimiterA This character is used to determine when to stop reading
	 * @param delimiterB This character is used to determine when to stop reading
	 */
	void readUntilChar (std::ostream & str, const char & delimiterA, const char & delimiterB);

	/**
	 * @brief This method is used to read characters into a string until (exclusive) a given delimiter, new line or end of file
	 * @param delimiter This character is used to determine when to stop reading
	 * @return A string containing the characters that are read
	 */
	std::string getUntilChar (const char & delimiter);

	/**
	 * @brief This method is used to read characters into a string until (exclusive) any given delimiter, new line or end of file
	 * @param delimiterA This character is used to determine when to stop reading
	 * @param delimiterB This character is used to determine when to stop reading
	 * @return A string containing the characters that are read
	 */
	std::string getUntilChar (const char & delimiterA, const char & delimiterB);

	/**
	 * @brief This method is used to get the current line number
	 * @return An int containing the current line number
	 */
	int getCurrentLineNumber () const;

	/**
	 * @brief This method is used to get the file name
	 * @return A string containing the file name
	 */
	std::string getFilename () const;
};
}

#endif // ELEKTRA_FILEUTILITY_HPP
