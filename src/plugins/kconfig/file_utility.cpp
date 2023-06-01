#include "./file_utility.hpp"
#include "./base.hpp"
#include "./kconfig_parser_exception.hpp"

namespace kconfig
{
FileUtility::FileUtility (std::string filenameParam, std::unique_ptr<std::istream> fileParam)
: file{ std::move (fileParam) }, stringBuffer{}, currentLine{ 1 }, filename{ std::move (filenameParam) }
{
}

char FileUtility::peekNextChar ()
{
	return static_cast<char> (this->file->peek ());
}

bool FileUtility::isNextCharEOF ()
{
	return this->file->eof ();
}

bool FileUtility::isNextCharNewline ()
{
	char nextChar = peekNextChar ();
	return nextChar == character_newline || nextChar == character_carriage_return;
}

bool FileUtility::isNextCharNewlineOrEOF ()
{
	return isNextCharNewline () || isNextCharEOF ();
}

bool FileUtility::isNextCharToken ()
{
	switch (peekNextChar ())
	{
	case character_dollar_sign:
	case character_open_bracket:
	case character_newline:
	case character_carriage_return:
	case character_equals_sign:
	case character_hash_sign:
	case character_close_bracket:
	case EOF:
		return true;
	default:
		return false;
	}
}

void FileUtility::skipChar ()
{
	this->file->get ();
}

void FileUtility::skipCharsIfBlank ()
{
	while (isblank (peekNextChar ()))
	{
		skipChar ();
	}
}

void FileUtility::skipLine ()
{
	++currentLine;
	while (true)
	{
		switch (static_cast<char> (this->file->get ()))
		{
		case character_newline:
			return;
		case character_carriage_return:
			if (peekNextChar () == character_newline) skipChar ();
			return;
		case EOF:
			if (isNextCharEOF ())
			{
				return;
			}
		}
	}
}

void FileUtility::skipLineIfEmptyOrComment ()
{
	while (true)
	{
		switch (peekNextChar ())
		{
		case character_newline:
		case character_carriage_return:
		case character_hash_sign:
			skipLine ();
			break;
		default:
			return;
		}
	}
}

inline void FileUtility::readEscapedChar (std::ostream & str)
{
	switch (static_cast<char> (this->file->get ()))
	{
	case 'n':
		str << '\n';
		break;
	case 't':
		str << '\t';
		break;
	case 'r':
		str << '\r';
		break;
	case '\\':
		str << '\\';
		break;
	default:
		throw KConfigParserException::expect (*this, "valid escape character code ('n', 't', 'r' or '\\')");
	}
}

void FileUtility::readUntilChar (std::ostream & str, const char & delimiter)
{
	char c;
	while (true)
	{
		c = static_cast<char> (this->file->get ());

		if (c == EOF && isNextCharEOF ())
		{
			break;
		}
		else if (c == character_newline || c == character_carriage_return || c == delimiter)
		{
			this->file->putback (c);
			break;
		}
		else if (c == character_escape)
		{
			readEscapedChar (str);
		}
		else
		{
			str << c;
		}
	}
}

void FileUtility::readUntilChar (std::ostream & str, const char & delimiterA, const char & delimiterB)
{
	char c;
	while (true)
	{
		c = static_cast<char> (this->file->get ());
		if (c == EOF && isNextCharEOF ())
		{
			break;
		}
		else if (c == character_newline || c == character_carriage_return || c == delimiterA || c == delimiterB)
		{
			this->file->putback (c);
			break;
		}
		else if (c == character_escape)
		{
			readEscapedChar (str);
		}
		else
		{
			str << c;
		}
	}
}

std::string FileUtility::getUntilChar (const char & delimiter)
{
	// Empty the stringBuffer before re-using it
	(this->stringBuffer).str (std::string ());
	readUntilChar (this->stringBuffer, delimiter);
	return (this->stringBuffer).str ();
}

std::string FileUtility::getUntilChar (const char & delimiterA, const char & delimiterB)
{
	// Empty the stringBuffer before re-using it
	(this->stringBuffer).str (std::string ());
	readUntilChar (this->stringBuffer, delimiterA, delimiterB);
	return (this->stringBuffer).str ();
}

int FileUtility::getCurrentLineNumber () const
{
	return currentLine;
}

std::string FileUtility::getFilename () const
{
	return filename;
}
}
