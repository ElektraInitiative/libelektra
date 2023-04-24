#include "./kconfig_parser.hpp"
#include "./base.hpp"
#include "./kconfig_parser_exception.hpp"

namespace kconfig
{
KConfigParser::KConfigParser (FileUtility & fileUtilityParam, CppKeySet & keySetParam)
: fileUtility{ fileUtilityParam }, keySet{ keySetParam }
{
}


void KConfigParser::parse (CppKey const & parent)
{
	CppKey current_group{ parent.getName (), KEY_END };
	CppKey current_key{ parent.getName (), KEY_END };

	while (true)
	{
		fileUtility.skipLineIfEmptyOrComment ();

		if (fileUtility.isNextCharEOF ())
		{
			break;
		}

		if (fileUtility.peekNextChar () == character_open_bracket)
		{
			current_group = loadGroupNameFromFile (parent);
			appendIfContainsMeta (current_group);
		}
		else
		{
			current_key = loadKeyFromFile (current_group);
			appendIfNotGroup (current_key, current_group);
		}
	}
}

kdb::Key KConfigParser::loadGroupNameFromFile (CppKey const & parent)
{
	CppKey key{ parent.getName (), KEY_END };

	while (fileUtility.peekNextChar () == character_open_bracket)
	{
		// Skip open bracket
		fileUtility.skipChar ();

		// If a name starts with `$`, it should be saved as a meta value instead of a key name
		if (fileUtility.peekNextChar () == character_dollar_sign)
		{
			// Skip the dollar sign
			fileUtility.skipChar ();

			std::string meta_candidate{ fileUtility.getUntilChar (character_close_bracket) };

			if (fileUtility.peekNextChar () == character_close_bracket)
			{
				fileUtility.skipChar ();
			}
			else
			{
				throw KConfigParserException::expect (fileUtility, character_close_bracket);
			}

			// A meta value can only be placed at the last value. E.g.
			// 	[keyName1][$pseudoMeta][keyName2][$realMeta]
			//	In the previous example `realMeta` is a meta value, `pseudoMeta` is not
			if (fileUtility.isNextCharNewlineOrEOF ())
			{
				key.setMeta (KCONFIG_METADATA_KEY, meta_candidate);
			}
			else
			{
				// Add the dollar sign that we skipped earlier
				key.addBaseName (character_dollar_sign + meta_candidate);
			}
		}
		else
		{
			key.addBaseName (fileUtility.getUntilChar (character_close_bracket));

			if (fileUtility.peekNextChar () == character_close_bracket)
			{
				fileUtility.skipChar ();
			}
			else
			{
				throw KConfigParserException::expect (fileUtility, character_close_bracket);
			}
		}
	}

	if (!fileUtility.isNextCharNewlineOrEOF ())
	{
		throw KConfigParserException::expect (fileUtility, "new line or end of file");
	}

	return key;
}

CppKey KConfigParser::loadKeyFromFile (CppKey const & parent)
{
	fileUtility.skipCharsIfBlank ();
	if (fileUtility.isNextCharToken ())
	{
		throw KConfigParserException::expect (fileUtility, "key name, not a spacial character");
	}

	std::string keyName{ fileUtility.getUntilChar (character_equals_sign, character_open_bracket) };

	// If the following line introduces problems, use `parent.dup();`
	CppKey key{ parent.getName (), KEY_END };

	if (fileUtility.isNextCharNewlineOrEOF ())
	{
		// This key doesn't contain any value
		key.addBaseName (keyName);
		return key;
	}

	std::string meta;
	bool has_locale = false;
	// Load the meta values and/or locales
	while (fileUtility.peekNextChar () == character_open_bracket)
	{
		// Skip the open bracket
		fileUtility.skipChar ();

		if (fileUtility.peekNextChar () == character_dollar_sign)
		{
			// Load meta value
			// Skip dollar sign
			fileUtility.skipChar ();
			meta += fileUtility.getUntilChar (character_close_bracket);
		}
		else
		{
			// Load locale
			if (has_locale)
			{
				throw KConfigParserException{ fileUtility, "Only one locale is allowed in a single key definition." };
			}
			else
			{
				has_locale = true;
			}
			keyName += character_open_bracket + fileUtility.getUntilChar (character_close_bracket) + character_close_bracket;
		}


		if (fileUtility.peekNextChar () == character_close_bracket)
		{
			fileUtility.skipChar ();
		}
		else
		{
			throw KConfigParserException::expect (fileUtility, character_close_bracket);
		}
	}


	key.addBaseName (keyName);
	// Skip empty spaces if any
	fileUtility.skipCharsIfBlank ();
	// Load the value
	if (fileUtility.peekNextChar () == character_equals_sign)
	{
		// Skip the equals sign
		fileUtility.skipChar ();
		fileUtility.skipCharsIfBlank ();
		key.setString (fileUtility.getUntilChar (character_newline));
	}
	else if (!fileUtility.isNextCharNewlineOrEOF ())
	{
		throw KConfigParserException::expect (fileUtility, "new line or end of file");
	}

	if (!meta.empty ())
	{
		key.setMeta (KCONFIG_METADATA_KEY, meta);
	}

	return key;
}

void KConfigParser::appendIfContainsMeta (CppKey const & key)
{
	if (key.hasMeta (KCONFIG_METADATA_KEY))
	{
		keySet.append (key);
	}
}
void KConfigParser::appendIfNotGroup (CppKey const & key, CppKey const & group)
{
	if (key != group)
	{
		keySet.append (key);
	}
}
}
