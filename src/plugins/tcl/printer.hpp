/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef PRINTER_HPP
#define PRINTER_HPP

#include <keyset.hpp>

#include <string>
#include <vector>

namespace elektra
{

struct Printer
{
	int nr_keys;
	int nr_meta;

	kdb::KeySet & current;
	kdb::Key & parent;

	std::string keyname;
	std::string metaname;

	Printer (kdb::KeySet & ks, kdb::Key & parent);

	void add_key (std::vector<char> const & c);
	void add_val (std::vector<char> const & c);

	void add_metakey (std::vector<char> const & c);
	void add_metaval (std::vector<char> const & c);
};


} // end namespace elektra

#endif
