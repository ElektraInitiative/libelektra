#include "file_utility.hpp"
#include "base.hpp"

FileUtility::FileUtility (const std::string & filename) : file{ filename }, string_buffer{}
{
	if (!(this->file).is_open ())
	{
		// TODO: Handle exception correctly
		return;
	}
}

char FileUtility::peekNextChar ()
{
	return (this->file).peek ();
}

bool FileUtility::isNextCharNewlineOrEOF ()
{
	char next_char = this->peekNextChar ();
	return next_char == EOF || next_char == character_newline || next_char == character_carriage_return;
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

void FileUtility::skipLineIfNotEndOfLine ()
{
	if (!isNextCharNewlineOrEOF ())
	{
		skipLine ();
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
	(this->string_buffer).clear ();
	readUntilChar (this->string_buffer, delimiter);
	return (this->string_buffer).str ();
}

std::string FileUtility::getUntilChar (const char & delimiterA, const char & delimiterB)
{
	(this->string_buffer).clear ();
	readUntilChar (this->string_buffer, delimiterA, delimiterB);
	return (this->string_buffer).str ();
}
