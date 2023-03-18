/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.hpp"

#include <internal/kdbprivate.h>
#include <internal/pluginload/module.h>

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

CppKeySet defaultConfig ()
{
	CppKeySet config{ 20,
			  keyNew ("user:/chars", KEY_END),
			  keyNew ("user:/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
			  keyNew ("user:/chars/20", KEY_VALUE, "77", KEY_END), // space -> w
			  keyNew ("user:/chars/23", KEY_VALUE, "72", KEY_END), // # -> r
			  keyNew ("user:/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
			  keyNew ("user:/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
			  keyNew ("user:/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
			  KS_END };
	return config;
}

CppKeySet percentConfig ()
{
	CppKeySet config{ 20,
			  keyNew ("user:/chars", KEY_END),
			  keyNew ("user:/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
			  keyNew ("user:/chars/20", KEY_VALUE, "77", KEY_END), // space -> w
			  keyNew ("user:/chars/23", KEY_VALUE, "72", KEY_END), // # -> r
			  keyNew ("user:/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
			  keyNew ("user:/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
			  keyNew ("user:/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
			  keyNew ("user:/escape", KEY_VALUE, "25", KEY_END),   // use % as escape character
			  KS_END };
	return config;
}

void testEnocdingDecoding (Plugin * const plugin, CppKey const & parent, string const decodedString, string const encodedString = "")
{
	ckdb::KeySet * rawKeys = ksNew (20, keyNew ("user:/tests/ccode/key", KEY_VALUE, decodedString.c_str (), KEY_END), KS_END);
	succeed_if_same (plugin->kdbSet (plugin, rawKeys, *parent), //! OCLint (empty if, too few branches switch)
			 ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbset` was not successful");

	CppKeySet keys (rawKeys);

	if (!encodedString.empty ())
	{
		CppKey encoded = keys.lookup ("user:/tests/ccode/key");
		succeed_if_same (encoded.getString (), encodedString, //! OCLint (empty if, too few branches switch)
				 "String not correctly encoded");
	}

	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), //! OCLint (empty if, too few branches switch)
			 ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` was not successful");
	CppKey decoded = keys.lookup ("user:/tests/ccode/key");
	succeed_if_same (decoded.getString (), decodedString, //! OCLint (empty if, too few branches switch)
			 "String not correctly decoded");
}

void testRoundTrip (string const decodedString, string const encodedString = "", CppKeySet config = defaultConfig ())
{
	CppKeySet modules{ 0, KS_END };
	elektraModulesInit (modules.getKeySet (), NULL);

	CppKey parent{ "system:/elektra/modules/type", KEY_END };
	Plugin * plugin = elektraPluginOpen ("ccode", modules.getKeySet (), config.getKeySet (), *parent);
	exit_if_fail (plugin != NULL, "Could not open ccode plugin"); //! OCLint (empty if, too few branches switch)

	testEnocdingDecoding (plugin, parent, decodedString, encodedString);

	elektraPluginClose (plugin, 0);
	ksDel (modules.release ());
	config.release ();
	elektraModulesClose (modules.getKeySet (), 0);
}

TEST (type, roundtrip) //! OCLint (avoid private static members)
{
	testRoundTrip ("a value\nwith=;# and \\ itself", "a\\wvalue\\nwith\\e\\s\\r\\wand\\w\\b\\witself");
	testRoundTrip ("hello world");
	testRoundTrip ("hello world!\nnew line");
	testRoundTrip ("\0");
	testRoundTrip ("\n");
	testRoundTrip ("\\");
	testRoundTrip (" ");
	testRoundTrip ("=");
	testRoundTrip (";");
	testRoundTrip ("#");
	testRoundTrip (" =;#");
	testRoundTrip ("\n\\");
	testRoundTrip ("");
	testRoundTrip ("a value\nwith=;# and \\ itself", "a%wvalue%nwith%e%s%r%wand%w%b%witself", percentConfig ());
}
