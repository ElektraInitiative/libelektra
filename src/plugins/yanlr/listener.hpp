/**
 * @file
 *
 * @brief A listener reacting to matches for the grammar rules defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <iostream>
#include <numeric>

#include <kdb.hpp>

#include "YAMLBaseListener.h"

using antlr::YAMLBaseListener;
using MappingContext = antlr::YAMLParser::MappingContext;
using ValueContext = antlr::YAMLParser::ValueContext;

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
	/** This stack stores a key for each level of the current key name below parent. */
	deque<CppKey> parents;

public:
	/**
	 * @brief This constructor creates a new empty key storage using the given parent key.
	 *
	 * @param parent This key specifies the parent of all keys stored in the object.
	 */
	KeyListener (CppKey parent) : keys{}
	{
		parents.push_back (parent);
	}

	/**
	 * @brief This function will be called after the parser exits a scalar value.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitValue (ValueContext * context) override
	{
		CppKey key = parents.back ();
		key.setString (context->getText ());
		keys.append (key);
	}

	/**
	 * @brief This function will be called after the parser enters a mapping.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void enterMapping (MappingContext * context) override
	{
		// Entering a mapping such as `part: …` means that we need to add `part` to the key name
		CppKey child{ parents.back ().getName (), KEY_END };
		child.addBaseName (context->key ()->getText ());
		parents.push_back (child);
	}

	/**
	 * @brief This function will be called after the parser exits a mapping.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitMapping (MappingContext * context ELEKTRA_UNUSED) override
	{
		// Returning from a mapping such as `part: …` means that we need need to remove the key for `part` from the stack.
		parents.pop_back ();
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
