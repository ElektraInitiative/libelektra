/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KEYSETGET_HPP
#define ELEKTRA_KEYSETGET_HPP

#include <map>

#include <keyset.hpp>

namespace kdb
{

template <typename T>
struct KeySetTypeWrapper<std::map<std::string, T>>
{
	std::map<std::string, T> operator() (KeySet const & ks, std::string const & name, elektraLookupFlags const options) const
	{
		std::map<std::string, T> ret;
		for (int i = 0; i < 5; ++i)
		{
			std::string n;
			if (name[0] != '/')
			{
				n = name;
				i = 10; // break next time
			}
			else
				switch (i)
				{
				case 0:
					n = "proc:" + name;
					break;
				case 1:
					n = "dir:" + name;
					break;
				case 2:
					n = "user:" + name;
					break;
				case 3:
					n = "system:" + name;
					break;
				}
			Key b = ks.lookup (n, options);
			if (!b) continue;

			ssize_t it = ks.search (b) + 1;

			for (; it < ks.size (); ++it)
			{
				const Key & k = ks.at (it);
				if (!k.isBelow (b)) break; // other keys are not relevant anymore
				if (k.isDirectBelow (b))
				{
					ret.insert (std::make_pair<std::string, T> (k.getBaseName (), k.get<T> ()));
				}
			}
		}
		return ret;
	}
};

} // end of namespace kdb

#endif
