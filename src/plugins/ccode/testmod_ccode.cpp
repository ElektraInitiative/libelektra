/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.hpp"

#include <kdbprivate.h> // For plugin handle (struct _Plugin)

#include <tests.hpp>

CCodeData * get_data (void)
{
	CCodeData * mapping = static_cast<CCodeData *> (calloc (1, sizeof (CCodeData)));
	mapping->escape = '\\'_uc;

	mapping->encode[' '_uc] = 'w'_uc;
	mapping->encode['\n'_uc] = 'n'_uc;
	mapping->encode['='_uc] = 'e'_uc;
	mapping->encode[';'_uc] = 's'_uc;
	mapping->encode['#'_uc] = 'r'_uc;
	mapping->encode['\\'_uc] = 'b'_uc;

	mapping->decode['w'_uc] = ' '_uc;
	mapping->decode['n'_uc] = '\n'_uc;
	mapping->decode['e'_uc] = '='_uc;
	mapping->decode['s'_uc] = ';'_uc;
	mapping->decode['r'_uc] = '#'_uc;
	mapping->decode['b'_uc] = '\\'_uc;
	return mapping;
}

const char encoded_string[] = "a\\wvalue\\nwith\\e\\s\\r\\wand\\w\\b\\witself";
const char other_encoded_string[] = "a%wvalue%nwith%e%s%r%wand%w%b%witself";
const char decoded_string[] = "a value\nwith=;# and \\ itself";

TEST (type, encode)
{
	printf ("test encode\n");

	CCodeData * mapping = get_data ();

	unsigned char buffer[1000];
	mapping->buffer = buffer;
	ckdb::Key * test = keyNew ("user/test", KEY_VALUE, decoded_string, KEY_END);
	encodeKey (test, mapping);
	succeed_if (memcmp (keyValue (test), encoded_string, sizeof (encoded_string) - 1) == 0, "string not correctly encoded");

	elektraFree (mapping);
	keyDel (test);
}

TEST (type, decode)
{
	printf ("test decode\n");

	CCodeData * mapping = get_data ();

	unsigned char buffer[1000];
	mapping->buffer = buffer;
	ckdb::Key * test = keyNew ("user/test", KEY_SIZE, sizeof (encoded_string) - 1, KEY_VALUE, encoded_string, KEY_END);
	decodeKey (test, mapping);
	succeed_if (memcmp (keyValue (test), decoded_string, sizeof (decoded_string) - 1) == 0, "string not correctly encoded");

	elektraFree (mapping);
	keyDel (test);
}

void check_reversibility (const char * msg)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[deep nested block]"), annotate ("oclint:suppress[high cyclomatic complexity]"),
			annotate ("oclint:suppress[high ncss method]")))
#endif

{
	CCodeData * mapping = get_data ();
	unsigned char buffer[1000];
	mapping->buffer = buffer;
	ckdb::Key * decode = keyNew ("user/test", KEY_VALUE, msg, KEY_END);

	ckdb::Key * encode = keyDup (decode);
	encodeKey (encode, mapping);

	decodeKey (encode, mapping);
	compare_key (encode, decode); //! OCLint (Constant conditional operator)

	elektraFree (mapping);
	keyDel (decode);
	keyDel (encode);
}

TEST (type, test_reversibility)
{
	printf ("test reversibility\n");

	check_reversibility ("hello world");
	check_reversibility ("hello world!\nnew line");
	check_reversibility ("\0");
	check_reversibility ("\n");
	check_reversibility ("\\");
	check_reversibility (" ");
	check_reversibility ("=");
	check_reversibility (";");
	check_reversibility ("#");
	check_reversibility (" =;#");
	check_reversibility ("\n\\");
}

TEST (type, decodeescape)
{
	printf ("test decode escape\n");

	CCodeData * mapping = get_data ();
	mapping->encode['\\'_uc] = '\\'_uc;
	mapping->decode['\\'_uc] = '\\'_uc;

	unsigned char buffer[1000];
	mapping->buffer = buffer;
	ckdb::Key * test = keyNew ("user/test", KEY_SIZE, 2, KEY_VALUE, "\\\\", KEY_END);
	decodeKey (test, mapping);
	succeed_if (memcmp (keyValue (test), "\\", 2) == 0, "string not correctly encoded");

	elektraFree (mapping);
	keyDel (test);
}

TEST (type, config)
{
	printf ("test with config\n");

	ckdb::KeySet * config =
		ksNew (20, keyNew ("user/chars", KEY_END), keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
		       keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END),				       // space -> w
		       keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END),				       // # -> r
		       keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END),				       // \\ (backslash) -> b
		       keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END),				       // = -> e
		       keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END),				       // ; -> s
		       KS_END);

	ckdb::KeySet * returned = ksNew (20, keyNew ("user/something", KEY_VALUE, decoded_string, KEY_END), KS_END);

	Plugin * plugin = static_cast<Plugin *> (calloc (1, sizeof (Plugin)));
	plugin->config = config;

	elektraCcodeOpen (plugin, 0);

	elektraCcodeSet (plugin, returned, 0);

	ckdb::Key * test = ksLookupByName (returned, "user/something", 0);
	succeed_if (memcmp (keyValue (test), encoded_string, sizeof (encoded_string) - 1) == 0, "string not correctly encoded");

	elektraCcodeClose (plugin, 0);

	ksDel (returned);
	ksDel (plugin->config);
	elektraFree (plugin);
}

TEST (type, otherescape)
{
	printf ("test with config with other escape\n");

	ckdb::KeySet * config =
		ksNew (20, keyNew ("user/chars", KEY_END), keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
		       keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END),				       // space -> w
		       keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END),				       // # -> r
		       keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END),				       // \\ (backslash) -> b
		       keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END),				       // = -> e
		       keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END),				       // ; -> s
		       keyNew ("user/escape", KEY_VALUE, "25", KEY_END),				       // use % as escape character
		       KS_END);

	ckdb::KeySet * returned = ksNew (20, keyNew ("user/something", KEY_VALUE, decoded_string, KEY_END), KS_END);

	Plugin * plugin = static_cast<Plugin *> (calloc (1, sizeof (Plugin)));
	plugin->config = config;

	elektraCcodeOpen (plugin, 0);

	elektraCcodeSet (plugin, returned, 0);

	ckdb::Key * test = ksLookupByName (returned, "user/something", 0);
	succeed_if (memcmp (keyValue (test), other_encoded_string, sizeof (other_encoded_string) - 1) == 0, "string not correctly encoded");

	elektraCcodeClose (plugin, 0);

	ksDel (returned);
	ksDel (plugin->config);
	elektraFree (plugin);
}
