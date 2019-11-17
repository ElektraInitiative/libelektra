#ifndef ELEKTRA_KCONFIG_PARSER_EXCEPTION_HPP
#define ELEKTRA_KCONFIG_PARSER_EXCEPTION_HPP

#include <exception>
#include <string>

class FileUtility;

class KConfigParserException : std::exception
{
private:
	std::string message;
	static std::string describeCharacter (char c);
	static std::string generateErrorMessage (std::string filename, int lineNumber, std::string message);

public:
	KConfigParserException (FileUtility const & fileUtility, std::string const & errorMessage);
	static KConfigParserException expect (FileUtility & fileUtility, std::string const & value);
	static KConfigParserException expect (FileUtility & fileUtility, char value);
	std::string getMessage() const;
};

#endif // ELEKTRA_KCONFIG_PARSER_EXCEPTION_HPP
