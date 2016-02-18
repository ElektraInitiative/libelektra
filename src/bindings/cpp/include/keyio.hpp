/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_KEY_IO_HPP
#define ELEKTRA_KEY_IO_HPP

#include <key.hpp>

#include <iostream>

namespace kdb
{

/**
 * @brief Stream the name of a key
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
	k.setName (name);

	return is;
}
}

#endif
