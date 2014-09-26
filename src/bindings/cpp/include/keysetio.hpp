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
 * @return the stream
 */
inline std::ostream & operator << (std::ostream & os, kdb::KeySet const & cks)
{
	kdb::KeySet & ks = const_cast<kdb::KeySet &>(cks);
	cursor_t c = ks.getCursor();
	ks.rewind();
	kdb::Key k;
	while ((k=ks.next()))
	{
		os << k << std::endl;
	}
	ks.setCursor(c);

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
inline std::istream & operator >> (std::istream & is, kdb::KeySet & ks)
{
	cursor_t c = ks.getCursor();
	while (!is.eof())
	{
		kdb::Key k;
		is >> k;
		ks.append(k);
	}
	ks.setCursor(c); // jump back to previous cursor

	return is;
}

}

#endif
