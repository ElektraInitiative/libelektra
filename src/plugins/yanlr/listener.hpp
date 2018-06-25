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
#include <kdbease.h>

#include "YAMLBaseListener.h"

using antlr::YAMLBaseListener;
using ElementContext = antlr::YAMLParser::ElementContext;
using MappingContext = antlr::YAMLParser::MappingContext;
using SequenceContext = antlr::YAMLParser::SequenceContext;
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
		ELEKTRA_LOG_DEBUG ("Add new key “%s” with value “%s”", key.getName ().c_str (), key.getString ().c_str ());
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
	 * @brief This function will be called after the parser enters a sequence.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void enterSequence (SequenceContext * context ELEKTRA_UNUSED) override
	{
		parents.back ().setMeta ("array", ""); // We start with an empty array
	}

	/**
	 * @brief This function will be called after the parser exits a sequence.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitSequence (SequenceContext * context ELEKTRA_UNUSED) override
	{
		// We add the parent key of all array elements after we leave the sequence
		keys.append (parents.back ());
	}

	/**
	 * @brief This function will be called after the parser recognizes an element of a sequence.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void enterElement (ElementContext * context ELEKTRA_UNUSED) override
	{
		CppKeySet arrayEntries{ elektraArrayGet (*parents.back (), keys.getKeySet ()) };

		if (arrayEntries.size () <= 0)
		{
			CppKey first{ parents.back ().getName (), KEY_END };
			first.addBaseName ("#");
			arrayEntries.append (first);
		}

		CppKey key{ elektraArrayGetNextKey (arrayEntries.getKeySet ()) };
		parents.back ().setMeta ("array", key.getBaseName ());
		parents.push_back (key);
		ELEKTRA_LOG_DEBUG ("New array element “%s”", parents.back ().getName ().c_str ());
	}

	/**
	 * @brief This function will be called after the parser read an element of a sequence.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitElement (ElementContext * context ELEKTRA_UNUSED) override
	{
		parents.pop_back (); // Remove the key for the current array entry
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
