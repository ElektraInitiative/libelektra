/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KEY_EXCEPT_HPP
#define ELEKTRA_KEY_EXCEPT_HPP

#ifndef USER_DEFINED_EXCEPTIONS

#include <stdexcept>
#include <string>

namespace kdb
{

class Exception : public std::exception
{
public:
	virtual const char * what () const throw ()
	{
		return "Exception thrown by Elektra";
	}
};

class KeyException : public Exception
{
public:
	virtual const char * what () const throw ()
	{
		return "Exception thrown by a Key, typically "
		       "because you called a method on a null key. "
		       "Make sure to check this with !key first";
	}
};

class KeyNotFoundException : public Exception
{
public:
	explicit KeyNotFoundException (std::string message) : m_str (message)
	{
	}

	virtual const char * what () const throw ()
	{
		return m_str.c_str ();
	}

private:
	std::string m_str;
};

class KeyTypeMismatch : public KeyException
{
public:
	virtual const char * what () const throw ()
	{
		return "Binary/String key mismatch, use proper "
		       "getString()/getBinary() or use getValue() to get both.";
	}
};

class KeyTypeConversion : public KeyException
{
public:
	virtual const char * what () const throw ()
	{
		return "Could not convert data to requested type. "
		       "Use get(Meta)<std::string> respectively get(Meta)<const Key> for more generic access "
		       "or specialize the template methods with your type.";
	}
};


class KeyInvalidName : public KeyException
{
private:
	std::string msg;

public:
	KeyInvalidName (const std::string & name, const std::string & more)
	{
		msg = "Invalid Keyname: keyname needs to start with /, meta:/, default:/, spec:/, proc:/, dir:/, user:/ or system:/ "
		      "or maybe you tried to change a key that is already in a KeySet. Name was: '" +
		      name + "' " + more;
	}

	virtual const char * what () const throw ()
	{
		return msg.c_str ();
	}
};
} // namespace kdb

#endif

#endif
