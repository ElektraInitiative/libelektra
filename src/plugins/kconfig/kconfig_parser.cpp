#include "kconfig_parser.hpp"
#include "base.hpp"

using kdb::Key;
using kdb::KeySet;

KconfigParser::KconfigParser (FileUtility & fileUtilityParam, KeySet & keySetParam) : fileUtility{ fileUtilityParam }, keySet{ keySetParam }
{
}

void KconfigParser::parse (Key const & parent)
{
	Key current_group{ parent.getName (), KEY_END };
	Key current_key{ parent.getName (), KEY_END };

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

kdb::Key KconfigParser::loadGroupNameFromFile (kdb::Key const & parent)
{
	Key key{ parent.getName (), KEY_END };

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
				// TODO: Handle error properly
				//     Expected close bracket, found new line or end of file
				return parent;
			}

			// A meta value can only be placed at the last value. E.g.
			// 	[keyName1][$pseudoMeta][keyName2][$realMeta]
			//	In the previous example `realMeta` is a meta value, pseudoMeta is not
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
				// TODO: Handle error properly
				//     Expected close bracket, found new line or end of file
				return parent;
			}
		}
	}

	if (!fileUtility.isNextCharNewlineOrEOF ())
	{
		// TODO: Handle error properly
		// 	Expected new line or end of file
		return parent;
	}

	return key;
}

kdb::Key KconfigParser::loadKeyFromFile (kdb::Key const & parent)
{
	fileUtility.skipCharsIfBlank ();
	if (fileUtility.isNextCharToken ())
	{
		// TODO: Handle error properly
		// 	Expected a key name, not a special character
		return parent;
	}

	std::string keyname{ fileUtility.getUntilChar (character_equals_sign, character_open_bracket) };

	// If the following line introduces problems, use `parent.dup();`
	Key key{ parent.getName (), KEY_END };

	if (fileUtility.isNextCharNewlineOrEOF ())
	{
		// This key doesn't contain any value
		key.addBaseName (keyname);
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
				// TODO: Handle error properly
				//     Only one locale is allowed
				return parent;
			}
			else
			{
				has_locale = true;
			}
			keyname += character_open_bracket + fileUtility.getUntilChar (character_close_bracket) + character_close_bracket;
		}


		if (fileUtility.peekNextChar () == character_close_bracket)
		{
			fileUtility.skipChar ();
		}
		else
		{
			// TODO: Handle error properly
			//     Expected close bracket
			return parent;
		}
	}


	key.addBaseName (keyname);
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
		// TODO: Handle error properly
		//     Expected new line or end of file
		return parent;
	}

	if (meta != "")
	{
		key.setMeta (KCONFIG_METADATA_KEY, meta);
	}

	return key;
}

void KconfigParser::appendIfContainsMeta (Key const & key)
{
	if (key.hasMeta (KCONFIG_METADATA_KEY))
	{
		keySet.append (key);
	}
}

void KconfigParser::appendIfNotGroup (Key const & key, Key const & group)
{
	if (key != group)
	{
		keySet.append (key);
	}
}
