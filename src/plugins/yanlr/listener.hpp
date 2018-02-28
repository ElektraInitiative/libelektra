/**
 * @file
 *
 * @brief A listener reacting to matches for the grammar rules defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <iostream>

#include <kdb.hpp>

#include "YAMLBaseListener.h"

using antlr::YAMLBaseListener;
using MappingContext = antlr::YAMLParser::MappingContext;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;


class KeyListener : public YAMLBaseListener
{
	CppKeySet keys;
	CppKey parent;

public:
	KeyListener (CppKey parent) : keys{}, parent{ parent.dup () }
	{
	}

	void exitMapping (MappingContext * context) override
	{
		CppKey key{ parent.getFullName (), KEY_END };
		key.addBaseName (context->key ()->getText ());
		key.setString (context->value ()->getText ());
		keys.append (key);
	}

	CppKeySet keySet ()
	{
		return keys;
	}
};
