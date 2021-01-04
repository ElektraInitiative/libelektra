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

#include <antlr4-runtime/antlr4-runtime.h>

#include "YAML.h"
#include "error_listener.hpp"
#include "listener.hpp"
#include "yaml_lexer.hpp"
#include "yanlr.hpp"

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

using yanlr::ErrorListener;
using yanlr::KeyListener;
using yanlr::YAML;
using yanlr::YAMLLexer;

using antlr4::ANTLRInputStream;
using antlr4::CommonTokenStream;
using antlr4::DiagnosticErrorListener;
using ParserATNSimulator = antlr4::atn::ParserATNSimulator;
using PredictionMode = antlr4::atn::PredictionMode;
using ParseTree = antlr4::tree::ParseTree;
using ParseTreeWalker = antlr4::tree::ParseTreeWalker;

using ckdb::keyNew;
using std::ifstream;

// -- Functions
// ----------------------------------------------------------------------------------------------------------------------------

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
			  keyNew ("system:/elektra/modules/yanlr", KEY_VALUE, "yanlr plugin waits for your orders", KEY_END),
			  keyNew ("system:/elektra/modules/yanlr/exports", KEY_END),
			  keyNew ("system:/elektra/modules/yanlr/exports/get", KEY_FUNC, elektraYanlrGet, KEY_END),
#include ELEKTRA_README
			  keyNew ("system:/elektra/modules/yanlr/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
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

	ErrorListener errorListener{ parent.getString () };
	parser.removeErrorListeners ();
	parser.addErrorListener (&errorListener);
#if DEBUG
	DiagnosticErrorListener diagErrorListener;
	parser.addErrorListener (&diagErrorListener);
	parser.getInterpreter<ParserATNSimulator> ()->setPredictionMode (PredictionMode::LL_EXACT_AMBIG_DETECTION);
#endif

	ParseTree * tree = parser.yaml ();
	if (parser.getNumberOfSyntaxErrors () > 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parent.getKey (), errorListener.message ());
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

	if (parent.getName () == "system:/elektra/modules/yanlr")
	{
		keys.append (getContract ());
		keys.release ();
		parent.release ();

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	ifstream file (parent.getString ());
	if (!file.is_open ())
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parent.getKey (), "Unable to open file '%s'", parent.getString ().c_str ());
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int status = parseYAML (file, keys, parent);

	keys.release ();
	parent.release ();

	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("yanlr", ELEKTRA_PLUGIN_GET, &elektraYanlrGet, ELEKTRA_PLUGIN_END);
}

} // end extern "C"
