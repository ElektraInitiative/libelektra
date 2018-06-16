/**
 * @file
 *
 * @brief Implementation of data type checker
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CODER_HPP
#define ELEKTRA_CODER_HPP

#include <vector>

#include <kdblogger.h>

#include <keyset.hpp>

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

using std::string;
using std::vector;

namespace elektra
{

class Coder
{
	vector<unsigned char> encode;
	vector<unsigned char> decode;

	unsigned char escapeCharacter;

	void setDefaultConfig ();
	void readConfig (CppKeySet const & config, CppKey const & root);

	string encodeString (string const & text);
	string decodeString (string const & text);

	void encodeValue (CppKey & key);
	void decodeValue (CppKey & key);

	CppKey encodeName (CppKey const & key);
	CppKey decodeName (CppKey const & key);

public:
	explicit Coder (CppKeySet config);
	CppKeySet encodeKeySet (CppKeySet const & keys);
	CppKeySet decodeKeySet (CppKeySet const & keys);
};

} // end namespace elektra

#endif
