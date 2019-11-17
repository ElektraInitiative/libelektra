#ifndef ELEKTRA_FILEUTILITY_HPP
#define ELEKTRA_FILEUTILITY_HPP


#include <fstream>
#include <ostream>
#include <sstream>
#include <string>


class FileUtility
{
private:
	std::ifstream file;
	std::stringstream string_buffer;
public:
	explicit FileUtility(const std::string &filename);

	char peekNextChar();

	bool isNextCharNewlineOrEOF();

	void skipChar();

	void skipCharsIfBlank();

	void skipLine();

	void skipLineIfNotEndOfLine();

	void skipLineIfEmptyOrComment();

	void readUntilChar(std::ostream &str, const char &delimiter);

	void readUntilChar(std::ostream &str, const char &delimiterA, const char &delimiterB);

	std::string getUntilChar(const char &delimiter);

	std::string getUntilChar(const char &delimiterA, const char &delimiterB);
};


#endif // ELEKTRA_FILEUTILITY_HPP
