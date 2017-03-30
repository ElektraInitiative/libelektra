/**
 * @file
 *
 * @brief Utility functions used in the xerces plugin
 *
 * Provides unique pointers that act as a bridge between XMLCh and std::string
 * Provides a general XercesPluginException
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XERCES_UTIL_H
#define ELEKTRA_PLUGIN_XERCES_UTIL_H

#include <memory>
#include <xercesc/util/XMLString.hpp>

namespace xerces
{

template <typename T>
struct XercesDeleter
{
	void operator() (T * ptr) const
	{
		ptr->release ();
	}
};

template <>
struct XercesDeleter<XMLCh>
{
	void operator() (XMLCh * ptr) const
	{
		XERCES_CPP_NAMESPACE::XMLString::release (&ptr);
	}
};

template <>
struct XercesDeleter<char>
{
	void operator() (char * ptr) const
	{
		XERCES_CPP_NAMESPACE::XMLString::release (&ptr);
	}
};

template <typename T>
using XercesPtr = std::unique_ptr<T, XercesDeleter<T>>;

inline XercesPtr<XMLCh> toXMLCh (std::string const & str)
{
	return XercesPtr<XMLCh> (XERCES_CPP_NAMESPACE::XMLString::transcode (str.c_str ()));
}

inline XercesPtr<char> toCStr (XMLCh const * xmlCh)
{
	return XercesPtr<char> (XERCES_CPP_NAMESPACE::XMLString::transcode (xmlCh));
}

inline std::string toStr (XMLCh const * xmlCh)
{
	return std::string (toCStr (xmlCh).get ());
}

#define asXMLCh(str) toXMLCh (str).get ()
#define asCStr(str) toCStr (str).get ()

class XercesPluginException : public std::exception
{
public:
	explicit XercesPluginException (std::string const & m) : msg (m)
	{
	}
	~XercesPluginException () throw ()
	{
	}
	const char * what () const throw ()
	{
		return msg.c_str ();
	}

private:
	std::string msg;
};
}

#define ELEKTRA_XERCES_ORIGINAL_ROOT_NAME "xerces/rootname"

#endif
