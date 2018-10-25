/**
 * @file
 *
 * @brief Source for yanlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <iostream>

#include <kdb.hpp>
#include <kdberrors.h>

#include <antlr4-runtime.h>

#include "YAML.h"
#include "error_listener.hpp"
#include "listener.hpp"
#include "yaml_lexer.hpp"
#include "yanlr.hpp"

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

using antlr::YAML;

using antlr4::ANTLRInputStream;
using antlr4::CommonTokenStream;
using ParseTree = antlr4::tree::ParseTree;
using ParseTreeWalker = antlr4::tree::ParseTreeWalker;

using std::ifstream;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace
{
/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
CppKeySet getContract ()
{
	return CppKeySet{ 30,
			  keyNew ("system/elektra/modules/yanlr", KEY_VALUE, "yanlr plugin waits for your orders", KEY_END),
			  keyNew ("system/elektra/modules/yanlr/exports", KEY_END),
			  keyNew ("system/elektra/modules/yanlr/exports/get", KEY_FUNC, elektraYanlrGet, KEY_END),
#include ELEKTRA_README (yanlr)
			  keyNew ("system/elektra/modules/yanlr/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

/**
 * @brief This function parses the content of a YAML file and saves the result in the given key set.
 *
 * @param file This file contains the YAML content this function should parse.
 * @param keys The function adds the key set representing `file` in this key set, if the parsing process finished successfully.
 * @param parent The function uses this parameter to emit error information.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE If parsing was successful and `keys` was not updated
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS If parsing was successful and `keys` was updated
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR If there was an error parsing `file`
 */
int parseYAML (ifstream & file, CppKeySet & keys, CppKey & parent)
{
	ANTLRInputStream input{ file };
	YAMLLexer lexer{ &input };
	CommonTokenStream tokens{ &lexer };
	YAML parser{ &tokens };
	ParseTreeWalker walker{};
	KeyListener listener{ parent };

	ErrorListener errorListener{};
	parser.removeErrorListeners ();
	parser.addErrorListener (&errorListener);

	ParseTree * tree = parser.yaml ();
	if (parser.getNumberOfSyntaxErrors () > 0)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_PARSE, parent.getKey (), errorListener.message ());
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	walker.walk (&listener, tree);

	auto readKeys = listener.keySet ();
	keys.append (readKeys);
	return readKeys.size () <= 0 ? ELEKTRA_PLUGIN_STATUS_NO_UPDATE : ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

} // end namespace

extern "C" {
// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraYanlrGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	CppKey parent{ parentKey };
	CppKeySet keys{ returned };

	if (parent.getName () == "system/elektra/modules/yanlr")
	{
		keys.append (getContract ());
		keys.release ();
		parent.release ();

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	ifstream file (parent.getString ());
	if (!file.is_open ())
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_COULD_NOT_OPEN, parent.getKey (), "Unable to open file “%s”",
				    parent.getString ().c_str ());
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int status = parseYAML (file, keys, parent);

	keys.release ();
	parent.release ();

	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yanlr)
{
	return elektraPluginExport ("yanlr", ELEKTRA_PLUGIN_GET, &elektraYanlrGet, ELEKTRA_PLUGIN_END);
}

} // end extern "C"
