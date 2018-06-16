/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.hpp"

#include <kdbmodule.h>
#include <kdbprivate.h>

#include <tests.hpp>

using std::string;

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

using ckdb::elektraModulesClose;
using ckdb::elektraModulesInit;
using ckdb::elektraPluginClose;
using ckdb::elektraPluginOpen;
using ckdb::keyNew;
using ckdb::ksDel;

using ckdb::Plugin;

string const encodedString = "a\\wvalue\\nwith\\e\\s\\r\\wand\\w\\b\\witself";
string const decodedString = "a value\nwith=;# and \\ itself";

TEST (type, config)
{
	CppKeySet modules{ 0, KS_END };
	elektraModulesInit (modules.getKeySet (), NULL);

	CppKeySet config{ 20,
			  keyNew ("user/chars", KEY_END),
			  keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
			  keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END), // space -> w
			  keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END), // # -> r
			  keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
			  keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
			  keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
			  KS_END };
	CppKey parent{ "system/elektra/modules/type", KEY_END };
	Plugin * plugin = elektraPluginOpen ("ccode", modules.getKeySet (), config.getKeySet (), *parent);
	exit_if_fail (plugin != NULL, "Could not open ccode plugin");

	CppKeySet keys{ 20, keyNew ("user/tests/ccode/encoded", KEY_VALUE, encodedString.c_str (), KEY_END), KS_END };
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Call of `kdbGet` was not successful");

	CppKey decoded = keys.lookup ("user/tests/ccode/encoded");
	succeed_if_same (decoded.getString (), decodedString, "String not correctly decoded");

	elektraPluginClose (plugin, 0);
	ksDel (modules.release ());
	config.release ();
	elektraModulesClose (modules.getKeySet (), 0);
}
