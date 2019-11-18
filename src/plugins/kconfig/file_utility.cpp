#include "file_utility.hpp"
#include "base.hpp"
#include "kconfig_parser_exception.hpp"

FileUtility::FileUtility (const std::string & filenameParam)
: file{ filenameParam }, stringBuffer{}, currentLine{ 1 }, filename{ filenameParam }
{
	if (!(this->file).is_open ())
	{
		throw KConfigParserException (*this, "Could not open the file.");
	}
}

char FileUtility::peekNextChar ()
{
	return (this->file).peek ();
}

bool FileUtility::isNextCharEOF ()
{
	return peekNextChar () == EOF;
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
	(this->file).get ();
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
		switch ((this->file).get ())
		{
		case character_newline:
			return;
		case character_carriage_return:
			if (peekNextChar () == character_newline) skipChar ();
			return;
		case EOF:
			// Not sure if the following line is needed
			// (this->file).putback (EOF);
			return;
		}
	}
}

void FileUtility::skipLineIfEmptyOrComment ()
{
	while (true)
	{
		switch ((this->file).peek ())
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

void FileUtility::readUntilChar (std::ostream & str, const char & delimiter)
{
	char c;
	while (true)
	{
		c = this->file.get ();
		if (c == EOF || c == character_newline || c == character_carriage_return || c == delimiter)
		{
			this->file.putback (c);
			break;
		}
		str << c;
	}
}

void FileUtility::readUntilChar (std::ostream & str, const char & delimiterA, const char & delimiterB)
{
	char c;
	while (true)
	{
		c = this->file.get ();
		if (c == EOF || c == character_newline || c == character_carriage_return || c == delimiterA || c == delimiterB)
		{
			this->file.putback (c);
			break;
		}
		str << c;
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
