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
	/** The key `parent` stores the common path of all keys. */
	CppKey parent;
	/** This stack stores each level of of the current key name. */
	deque<string> path;

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
	 * @brief This function will be called after the parser exits a scalar value.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitValue (ValueContext * context) override
	{
		CppKey key = parent.dup ();

		// Calculate the current key name from `parent` + `path`
		key = accumulate (path.begin (), path.end (), key, [](CppKey & key, string part) {
			key.addBaseName (part);
			return key;
		});
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
		// Entering a mapping such as `part: …` means that we need need to add `part` to the key name
		string part = context->key ()->getText ();
		path.push_back (part);
	}

	/**
	 * @brief This function will be called after the parser exits a mapping.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitMapping (MappingContext * context __attribute__ ((unused))) override
	{
		// Returning from a mapping such as `part: …` means that we need need to remove `part` from the key name
		path.pop_back ();
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
