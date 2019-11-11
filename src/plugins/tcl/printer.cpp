/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "printer.hpp"

#include <key.hpp>
#include <keyset.hpp>

#include <iostream>
#include <string>

using namespace std;

namespace elektra
{

using kdb::Key;
using kdb::KeySet;

Printer::Printer (KeySet & ks, Key & parentKey) : nr_keys (0), nr_meta (0), current (ks), parent (parentKey)
{
}

void Printer::add_key (std::vector<char> const & c)
{
	std::string s (c.begin (), c.end ());

	keyname = s;

	Key k (parent.getName (), KEY_END);
	k.addName (keyname);

	current.append (k);

	// cout << "current key is: " << current.current().getName() << endl;

	++nr_keys;
	// std::cout << "[" << nr_keys << "] add key " << s << std::endl;
}

void Printer::add_val (std::vector<char> const & c)
{
	std::string s (c.begin (), c.end ());
	// std::cout << "[" << nr_keys << "] add val " << s << " to keyname " << keyname << std::endl;

	// cout << "current key is: " << current.current().getName() << endl;

	current.current ().setString (s);
}

void Printer::add_metakey (std::vector<char> const & c)
{
	std::string s (c.begin (), c.end ());
	++nr_meta;
	metaname = s;

	// std::cout << "[" << nr_meta << "] add metakey " << s << std::endl;
}

void Printer::add_metaval (std::vector<char> const & c)
{
	std::string s (c.begin (), c.end ());
	// std::cout << "[" << nr_meta << "] add metaval " << s << " to metaname " << metaname << std::endl;

	current.current ().setMeta<string> (metaname, s);
}

} // end namespace elektra
