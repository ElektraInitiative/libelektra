/**
 * @file
 *
 * @brief Utility functions used in the xerces plugin
 *
 * Provides unique pointers that act as a bridge between XMLCh and std::string
 * Provides a general XercesPluginException
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XERCES_UTIL_H
#define ELEKTRA_PLUGIN_XERCES_UTIL_H

#include <memory>
#include <xercesc/util/TransService.hpp>
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
	// XMLByte required by TranscodeFromStr is an unsigned char * but basically they can be used equivalently
	return XercesPtr<XMLCh> (
		XERCES_CPP_NAMESPACE::TranscodeFromStr (reinterpret_cast<const XMLByte *> (str.c_str ()), str.size (), "UTF-8").adopt ());
}

inline XercesPtr<char> toCStr (XMLCh const * xmlCh)
{
	// XMLByte returned by TranscodeToStr is an unsigned char * but basically they can be used equivalently
	return XercesPtr<char> (reinterpret_cast<char *> (XERCES_CPP_NAMESPACE::TranscodeToStr (xmlCh, "UTF-8").adopt ()));
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
} // namespace xerces

#define ELEKTRA_XERCES_ORIGINAL_ROOT_NAME "xerces/rootname"

#endif
