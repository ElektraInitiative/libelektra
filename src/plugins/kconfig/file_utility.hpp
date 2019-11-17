#ifndef ELEKTRA_FILEUTILITY_HPP
#define ELEKTRA_FILEUTILITY_HPP

#include <fstream>
#include <ostream>
#include <sstream>
#include <string>

class FileUtility
{
private:
	/* This file is the file stream that we want to parse  */
	std::ifstream file;
	/* This stringBuffer is used when reading strings from file so that we don't initialize a stringstream each time */
	std::stringstream stringBuffer;

public:
	/**
	 * @brief This constructor is used to abstract the file stream into the methods that are specific to parsing a kconfig configuration
	 * file
	 * @param filename This string contains the absolute/relative path to the file
	 */
	explicit FileUtility (const std::string & filename);

	/**
	 * \copydoc std::ifstream::peek()
	 */
	char peekNextChar ();

	/**
	 * @brief This method is used to check if the next character in the buffer is end of file
	 *
	 * @return true if the next character is end of file, false otherwise
	 */
	bool isNextCharEOF ();

	/**
	 * @brief This method is used to check if the next character in the buffer is a new line
	 *
	 * @return true if the next character is new line, false otherwise
	 */
	bool isNextCharNewline ();

	/**
	 * @brief This method is used to check if the next character in the buffer is a new line or end of file
	 *
	 * @return true if the next character is new line or end of file, false otherwise
	 */
	bool isNextCharNewlineOrEOF ();

	/**
	 * @brief This method is used to check if the next character in the buffer is any of special characters used to specify a token
	 *
	 * @return true if the next character is used to specify a token, false otherwise
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
};

#endif // ELEKTRA_FILEUTILITY_HPP
