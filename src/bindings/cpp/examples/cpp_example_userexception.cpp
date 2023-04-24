/**
 * @file
 *
 * @brief This example explains how to define user defined exception.
 *
 * That means that the user can implement their own exceptions,
 * e.g. potentially derived from their own base class with its
 * own what() information.
 * The user could even decide to use another mechanism
 * instead of what().
 *
 * The only requirements are: They must be named exactly like
 * the original classes and KDBException constructor must take a key
 * as argument (which has all information like error and warnings).
 *
 * It does not matter from which class the exceptions are derived
 * or which members they have (if they are binary compatible) as
 * long as they are used everywhere.
 *
 * Never use non-binary compatible user exceptions if you do not
 * use them everywhere where you include any of elektras header
 * files!
 *
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/macros/old_utils.h>
#include <stdexcept>

class UserException : public std::exception
{
public:
	virtual const char * what () const throw () override
	{
		return "User Exception";
	}
};

namespace kdb
{

class Exception : public UserException
{
public:
	virtual const char * what () const throw () override
	{
		return "User Exception: Exception thrown by Elektra";
	}
};

class KeyException : public Exception
{
public:
	virtual const char * what () const throw () override
	{
		return "User Exception: Exception thrown by a Key";
	}
};

class KeyNotFoundException : public Exception
{
public:
	explicit KeyNotFoundException (std::string const &)
	{
	}
	virtual const char * what () const throw ()
	{
		return "User Exception: Key not found";
	}

private:
};

class KeyTypeMismatch : public KeyException
{
public:
	virtual const char * what () const throw () override
	{
		return "User Exception: Binary or String key mismatch";
	}
};

class KeyInvalidName : public KeyException
{
public:
	KeyInvalidName (const std::string & name ELEKTRA_UNUSED, const std::string & more ELEKTRA_UNUSED)
	{
	}

	virtual const char * what () const throw () override
	{
		return "User Exception: Invalid Keyname";
	}
};

class KeyTypeConversion : public KeyException
{
public:
	virtual const char * what () const throw () override
	{
		return "User Exception: Exception thrown by get/set";
	}
};
} // namespace kdb

#define USER_DEFINED_EXCEPTIONS
#include <key.hpp>

namespace kdb
{

class KDBException : public Exception
{
public:
	explicit KDBException (Key key) : m_key (key)
	{
	}

	virtual ~KDBException () throw ()
	{
	}

	virtual const char * what () const throw () override
	{
		return "User Exception: KDB";
	}

protected:
	Key m_key;
};

class ContractException : public KDBException
{
public:
	explicit ContractException (Key key) : KDBException (key)
	{
	}

	~ContractException () noexcept override = default;

	const char * what () const noexcept override
	{
		if (!m_key)
		{
			return "Malformed contract";
		}
		return KDBException::what ();
	}
};

} // namespace kdb


#include <kdb.hpp>

int main ()
{
	kdb::Key k ("abc", KEY_END);
	kdb::KDB kdb;
	kdb::KeySet ks;
	kdb.get (ks, k);
}
