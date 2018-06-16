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

CppKeySet defaultConfig ()
{
	CppKeySet config{ 20,
			  keyNew ("user/chars", KEY_END),
			  keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
			  keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END), // space -> w
			  keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END), // # -> r
			  keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
			  keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
			  keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
			  KS_END };
	return config;
}

CppKeySet percentConfig ()
{
	CppKeySet config{ 20,
			  keyNew ("user/chars", KEY_END),
			  keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
			  keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END), // space -> w
			  keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END), // # -> r
			  keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
			  keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
			  keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
			  keyNew ("user/escape", KEY_VALUE, "25", KEY_END),   // use % as escape character
			  KS_END };
	return config;
}

void testRoundTrip (string const decodedString, string const encodedString = "", CppKeySet config = defaultConfig ())
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[empty if statement]"), annotate ("oclint:suppress[high cyclomatic complexity]"),
			annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	CppKeySet modules{ 0, KS_END };
	elektraModulesInit (modules.getKeySet (), NULL);

	CppKey parent{ "system/elektra/modules/type", KEY_END };
	Plugin * plugin = elektraPluginOpen ("ccode", modules.getKeySet (), config.getKeySet (), *parent);
	exit_if_fail (plugin != NULL, "Could not open ccode plugin");

	CppKeySet keys{ 20, keyNew ("user/tests/ccode/key", KEY_VALUE, decodedString.c_str (), KEY_END), KS_END };
	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Call of `kdbset` was not successful");

	if (!encodedString.empty ())
	{
		CppKey encoded = keys.lookup ("user/tests/ccode/key");
		succeed_if_same (encoded.getString (), encodedString, "String not correctly encoded");
	}

	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Call of `kdbGet` was not successful");
	CppKey decoded = keys.lookup ("user/tests/ccode/key");
	succeed_if_same (decoded.getString (), decodedString, "String not correctly decoded");

	elektraPluginClose (plugin, 0);
	ksDel (modules.release ());
	config.release ();
	elektraModulesClose (modules.getKeySet (), 0);
}

TEST (type, roundtrip)
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
