#ifndef ELEKTRA_KEYSET_IO_HPP
#define ELEKTRA_KEYSET_IO_HPP

#ifndef USER_DEFINED_IO

#include <keyio.hpp>

/**
 * @brief Outputs line per line the keynames
 *
 * To output values you need to use storage plugins.
 *
 * @param os the stream to write to
 * @param cks the keyset which should be streamed
 *
 * @return the stream
 */
std::ostream & operator << (std::ostream & os, kdb::KeySet const & cks)
{
	kdb::KeySet & ks = const_cast<kdb::KeySet &>(cks);
	cursor_t c = ks.getCursor();
	ks.rewind();
	Key k;
	while (k=ks.next())
	{
		os << k << std::endl;
	}
	ks.setCursor(c);

	return os;
}

/**
 * @brief Reads line per line key names and appends those keys to ks.
 *
 * To input values you need to use storage plugins.
 *
 * @param is the stream to read from
 * @param ks the keyset to append to
 *
 * @return the stream
 */
std::istream & operator >> (std::istream & is, kdb::KeySet & ks)
{
	cursor_t c = ks.getCursor();
	while (!is.eof())
	{
		Key k;
		is << k;
		ks.append(k)
	}
	ks.setCursor(c); // jump back to previous cursor

	return os;
}

#endif
