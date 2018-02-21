/**
 * @file
 *
 * @brief A listener reacting to matches for the grammar rules defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <iostream>

#include "YAMLBaseListener.h"

using antlr::YAMLBaseListener;
using KeyContext = antlr::YAMLParser::KeyContext;
using ValueContext = antlr::YAMLParser::ValueContext;

using std::cout;
using std::endl;

class KeyListener : public YAMLBaseListener
{
public:
	void exitKey (KeyContext * context)
	{
		cout << "Found name “" << context->getText () << "”" << endl;
	}

	void exitValue (ValueContext * context)
	{
		cout << "Found value “" << context->getText () << "”" << endl;
	}
};
