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
using KeyContext = antlr::YAMLParser::KeyContext;
using ValueContext = antlr::YAMLParser::ValueContext;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

using std::cout;
using std::endl;

class KeyListener : public YAMLBaseListener
{
	CppKeySet keys;
	CppKey parent;

public:
	KeyListener (CppKey parent) : keys{}, parent{ parent.dup () }
	{
	}

	void exitKey (KeyContext * context) override
	{
		cout << "Found name “" << context->getText () << "”" << endl;
	}

	void exitValue (ValueContext * context) override
	{
		cout << "Found value “" << context->getText () << "”" << endl;
	}
};
