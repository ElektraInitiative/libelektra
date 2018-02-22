/**
 * @file
 *
 * @brief A listener reacting to matches for the grammar rules defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <iostream>

#include <kdb.hpp>

#include "YAMLBaseListener.h"

using antlr::YAMLBaseListener;
using MappingContext = antlr::YAMLParser::MappingContext;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

// -- Class --------------------------------------------------------------------------------------------------------------------------------

/**
 * @brief This class creates a key set by listening to matches of grammar rules specified via YAML.g4.
 */
class KeyListener : public YAMLBaseListener
{
	/** This variable stores a key set representing the textual input. */
	CppKeySet keys;
	/** The key `parent` stores the common path of all keys. */
	CppKey parent;

public:
	/**
	 * @brief This constructor creates a new empty key storage using the given parent key.
	 *
	 * @param parent This key specifies the parent of all keys stored in the object.
	 */
	KeyListener (CppKey parent) : keys{}, parent{ parent.dup () }
	{
	}

	/**
	 * @brief This function will be called after the parser matched a complete mapping.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	void exitMapping (MappingContext * context) override
	{
		CppKey key{ parent.getFullName (), KEY_END };
		key.addBaseName (context->key ()->getText ());
		key.setString (context->value ()->getText ());
		keys.append (key);
	}

	/**
	 * @brief This function returns the data read by the parser.
	 *
	 * @return The key set representing the data from the textual input
	 */
	CppKeySet keySet ()
	{
		return keys;
	}
};
