/**
 * @file
 *
 * @brief header-only singleton class template
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_TEMPLATE_SINGLETON_HPP
#define ELEKTRA_REST_TEMPLATE_SINGLETON_HPP

template <typename T>
class singleton
{
public:
	static T & instance ()
	{
		static T instance;
		return instance;
	}

protected:
	singleton ()
	{
	}
	singleton (T &&)
	{
	}
	singleton (const T &)
	{
	}
};

#endif
