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
inline std::ostream & operator << (std::ostream & os, kdb::Key const & k)
{
	os << k.getName();

	return os;
}

/**
 * @brief Reads a line with a keys name
 *
 * @param is the stream to read from
 * @param k the key whose name will be set
 *
 * @return the stream
 */
inline std::istream & operator >> (std::istream & is, kdb::Key & k)
{
	std::string name;
	getline(is, name);
	k.setName(name);

	return is;
}

}

#endif
