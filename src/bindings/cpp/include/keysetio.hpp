/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_KEYSET_IO_HPP
#define ELEKTRA_KEYSET_IO_HPP

#include <iostream>

#include <keyio.hpp>

#include <keyset.hpp>

namespace kdb
{

/**
 * @brief Outputs line per line the keynames
 *
 * To output values you should use the plugin framework.
 *
 * @param os the stream to write to
 * @param cks the keyset which should be streamed
 *
 * Use unsetf(std::ios_base::skipws) or use noskipws iomanip on the stream
 * if you want a null terminated sequence of key names.
 *
 * Use setf(std::ios_base::unitbuf) on the stream if you want to
 * flush the buffer after each key.
 *
 * @return the stream
 */
inline std::ostream & operator<< (std::ostream & os, kdb::KeySet const & cks)
{
	kdb::KeySet & ks = const_cast<kdb::KeySet &> (cks);
	cursor_t c = ks.getCursor ();
	ks.rewind ();
	kdb::Key k;
	while ((k = ks.next ()))
	{
		os << k;
		if (os.flags () & std::ios_base::skipws)
		{
			os << '\n';
		}
		else
		{
			os << '\0';
		}

		if (os.flags () & std::ios_base::unitbuf)
		{
			os << std::flush;
		}
	}
	ks.setCursor (c);

	return os;
}

/**
 * @brief Reads line per line key names and appends those keys to ks.
 *
 * To input values you need to use the plugin framework.
 *
 * @param is the stream to read from
 * @param ks the keyset to append to
 *
 * @return the stream
 */
inline std::istream & operator>> (std::istream & is, kdb::KeySet & ks)
{
	cursor_t c = ks.getCursor ();
	while (!is.eof ())
	{
		kdb::Key k;
		is >> k;
		ks.append (k);
	}
	ks.setCursor (c); // jump back to previous cursor

	return is;
}
}

#endif
