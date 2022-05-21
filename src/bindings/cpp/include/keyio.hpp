/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KEY_IO_HPP
#define ELEKTRA_KEY_IO_HPP

#include <key.hpp>
#include <keyset.hpp>

#include <iostream>

namespace kdb
{

/**
 * @brief Stream the name of a key
 *
 * Use setf(std::ios_base::showbase) on the stream if you want to
 * also output all metakeys (warning, cannot be parsed back!)
 *
 * If you also want to stream the value, use the plugin framework.
 *
 * @param os the stream to write to
 * @param k the key which name should be streamed
 *
 * @return the stream
 */
inline std::ostream & operator<< (std::ostream & os, kdb::Key const & k)
{
	os << k.getName ();
	if (os.flags () & std::ios_base::showbase)
	{
		ckdb::KeySet * metaKeys = ckdb::keyMeta (k.getKey ());
		for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
		{
			const Key & meta = ckdb::ksAtCursor (metaKeys, it);
			os << " " << meta.getName ();
		}
	}

	return os;
}

/**
 * @brief Reads a line with a keys name
 *
 * @param is the stream to read from
 * @param k the key whose name will be set
 *
 * Use unsetf(std::ios_base::skipws) on the stream if the keyname is
 * terminated with an null character and not a newline.
 *
 * @return the stream
 */
inline std::istream & operator>> (std::istream & is, kdb::Key & k)
{
	std::string name;
	char delim = '\0';
	if (is.flags () & std::ios_base::skipws)
	{
		delim = '\n';
	}
	getline (is, name, delim);
	if (is.flags () & std::ios_base::showbase)
	{
		std::stringstream iis (name);
		iis >> name;
		std::string n;
		while ((iis >> n))
		{
			k.setMeta (n, "");
		}
	}
	k.setName (name);

	return is;
}
} // namespace kdb

#endif
