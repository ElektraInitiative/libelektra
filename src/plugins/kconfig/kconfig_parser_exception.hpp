#ifndef ELEKTRA_KCONFIG_PARSER_EXCEPTION_HPP
#define ELEKTRA_KCONFIG_PARSER_EXCEPTION_HPP

#include <exception>
#include <string>

namespace kconfig
{
class FileUtility;
class KConfigParserException : std::exception
{
private:
	/* This string contains the error message */
	std::string message;

	/**
	 * @brief This function describes characters via a visible string. Is needed for cases such as new lines and end of file.
	 * @param c This character is the value we want do describe
	 * @return A string
	 */
	static std::string describeCharacter (char c);

	/**
	 * @brief This function generates an error message for the given values
	 * @param filename This string contains the name of the file that is being parsed
	 * @param lineNumber This int contains the current line number
	 * @param message This string contains a specific error message
	 * @return
	 */
	static std::string generateErrorMessage (std::string const & filename, int lineNumber, std::string const & message);

public:
	/**
	 * @brief This constructor is used to create an exception with an error message that contains the filename and a message
	 * @param fileUtility This FileUtility contains the file name and the line number
	 * @param errorMessage This string contains a specific error message
	 */
	KConfigParserException (FileUtility const & fileUtility, std::string const & errorMessage);
	/**
	 * @brief This function is used to create an exception when a char was expected but not found
	 * @param fileUtility This FileUtility contains the file name and the line number
	 * @param value This string is a description of the expected character
	 * @return
	 */
	static KConfigParserException expect (FileUtility & fileUtility, std::string const & value);

	/**
	 * @brief This function is used to create an exception when a char was expected but not found
	 * @param fileUtility This FileUtility contains the file name and the line number
	 * @param value This character is the expected character
	 * @return
	 */
	static KConfigParserException expect (FileUtility & fileUtility, char value);

	/**
	 * @brief This is a getter method
	 * @return the error message
	 */
	std::string getMessage () const;
};
}

#endif // ELEKTRA_KCONFIG_PARSER_EXCEPTION_HPP
