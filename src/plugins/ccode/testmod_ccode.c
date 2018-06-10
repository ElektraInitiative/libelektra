/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.hpp"

#include <tests_plugin.h>

CCodeData * get_data (void)
{
	CCodeData * mapping = calloc (1, sizeof (CCodeData));
	mapping->escape = '\\';

	mapping->encode[' '] = 'w';
	mapping->encode['\n'] = 'n';
	mapping->encode['='] = 'e';
	mapping->encode[';'] = 's';
	mapping->encode['#'] = 'r';
	mapping->encode['\\'] = 'b';

	mapping->decode['w'] = ' ';
	mapping->decode['n'] = '\n';
	mapping->decode['e'] = '=';
	mapping->decode['s'] = ';';
	mapping->decode['r'] = '#';
	mapping->decode['b'] = '\\';
	return mapping;
}

const char encoded_string[] = "a\\wvalue\\nwith\\e\\s\\r\\wand\\w\\b\\witself";
const char other_encoded_string[] = "a%wvalue%nwith%e%s%r%wand%w%b%witself";
const char decoded_string[] = "a value\nwith=;# and \\ itself";

void test_encode (void)
{
	printf ("test encode\n");

	CCodeData * mapping = get_data ();

	unsigned char buffer[1000];
	mapping->buffer = buffer;
	Key * test = keyNew ("user/test", KEY_VALUE, decoded_string, KEY_END);
	elektraCcodeEncode (test, mapping);
	succeed_if (memcmp (keyValue (test), encoded_string, sizeof (encoded_string) - 1) == 0, "string not correctly encoded");

	elektraFree (mapping);
	keyDel (test);
}

void test_decode (void)
{
	printf ("test decode\n");

	CCodeData * mapping = get_data ();

	unsigned char buffer[1000];
	mapping->buffer = buffer;
	Key * test = keyNew ("user/test", KEY_SIZE, sizeof (encoded_string) - 1, KEY_VALUE, encoded_string, KEY_END);
	elektraCcodeDecode (test, mapping);
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
	Key * decode = keyNew ("user/test", KEY_VALUE, msg, KEY_END);

	Key * encode = keyDup (decode);
	elektraCcodeEncode (encode, mapping);

	elektraCcodeDecode (encode, mapping);
	compare_key (encode, decode); //! OCLint (Constant conditional operator)

	elektraFree (mapping);
	keyDel (decode);
	keyDel (encode);
}

void test_reversibility (void)
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

void test_decodeescape (void)
{
	printf ("test decode escape\n");

	CCodeData * mapping = get_data ();
	mapping->encode['\\'] = '\\';
	mapping->decode['\\'] = '\\';

	unsigned char buffer[1000];
	mapping->buffer = buffer;
	Key * test = keyNew ("user/test", KEY_SIZE, 2, KEY_VALUE, "\\\\", KEY_END);
	elektraCcodeDecode (test, mapping);
	succeed_if (memcmp (keyValue (test), "\\", 2) == 0, "string not correctly encoded");

	elektraFree (mapping);
	keyDel (test);
}

void test_config (void)
{
	printf ("test with config\n");

	KeySet * config = ksNew (20, keyNew ("user/chars", KEY_END), keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
				 keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END),					 // space -> w
				 keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END),					 // # -> r
				 keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
				 keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
				 keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
				 KS_END);

	KeySet * returned = ksNew (20, keyNew ("user/something", KEY_VALUE, decoded_string, KEY_END), KS_END);

	Plugin * plugin = calloc (1, sizeof (Plugin));
	plugin->config = config;

	elektraCcodeOpen (plugin, 0);

	elektraCcodeSet (plugin, returned, 0);

	Key * test = ksLookupByName (returned, "user/something", 0);
	succeed_if (memcmp (keyValue (test), encoded_string, sizeof (encoded_string) - 1) == 0, "string not correctly encoded");

	elektraCcodeClose (plugin, 0);

	ksDel (returned);
	ksDel (plugin->config);
	elektraFree (plugin);
}

void test_otherescape (void)
{
	printf ("test with config with other escape\n");

	KeySet * config = ksNew (20, keyNew ("user/chars", KEY_END), keyNew ("user/chars/0A", KEY_VALUE, "6E", KEY_END), // new line -> n
				 keyNew ("user/chars/20", KEY_VALUE, "77", KEY_END),					 // space -> w
				 keyNew ("user/chars/23", KEY_VALUE, "72", KEY_END),					 // # -> r
				 keyNew ("user/chars/5C", KEY_VALUE, "62", KEY_END), // \\ (backslash) -> b
				 keyNew ("user/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
				 keyNew ("user/chars/3B", KEY_VALUE, "73", KEY_END), // ; -> s
				 keyNew ("user/escape", KEY_VALUE, "25", KEY_END),   // use % as escape character
				 KS_END);

	KeySet * returned = ksNew (20, keyNew ("user/something", KEY_VALUE, decoded_string, KEY_END), KS_END);

	Plugin * plugin = calloc (1, sizeof (Plugin));
	plugin->config = config;

	elektraCcodeOpen (plugin, 0);

	elektraCcodeSet (plugin, returned, 0);

	Key * test = ksLookupByName (returned, "user/something", 0);
	succeed_if (memcmp (keyValue (test), other_encoded_string, sizeof (other_encoded_string) - 1) == 0, "string not correctly encoded");

	elektraCcodeClose (plugin, 0);

	ksDel (returned);
	ksDel (plugin->config);
	elektraFree (plugin);
}


int main (int argc, char ** argv)
{
	printf ("CCODE   TESTS\n");
	printf ("=============\n\n");

	init (argc, argv);

	test_encode ();
	test_decode ();
	test_reversibility ();
	test_decodeescape ();
	test_config ();
	test_otherescape ();

	print_result ("testmod_ccode");

	return nbError;
}
