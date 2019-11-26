/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_EXCEPT_HPP
#define ELEKTRA_KDB_EXCEPT_HPP

/*
 * @brief See examples/cpp_example_userexception.cpp for how to use
 * USER_DEFINED_EXCEPTIONS.
 */
#ifndef USER_DEFINED_EXCEPTIONS

#include <keyexcept.hpp>

#include <sstream>
#include <string>

#include <kdbio.hpp>

namespace kdb
{

class KDBException : public Exception
{
public:
	explicit KDBException (Key key) : m_key (key), m_str ()
	{
	}

	virtual ~KDBException () throw ()
	{
	}

	virtual const char * what () const throw ()
	{
		return whatWithArguments (true, true);
	}

	virtual const char * whatWithArguments (bool printVerbose, bool printDebug) const throw ()
	{
		if (!m_key)
		{
			return "Generic KDBException";
		}
		else if (m_str.empty ())
		{
			// note that the code will be re-evaluated
			// if it prints nothing, but an expensive
			// function not printing anything seems
			// to be unlikely.
			//
			// note that printError/printWarning will be
			// used either from namespace kdb or global
			// namespace.
			std::stringstream ss;
			printWarnings (ss, m_key, printVerbose, printDebug);
			printError (ss, m_key, printVerbose, printDebug);
			m_str = ss.str ();
		}
		return m_str.c_str ();
	}

protected:
	Key m_key;

private:
	mutable std::string m_str;
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

class KDBMountException : public KDBException
{
	std::string msg;

public:
	explicit KDBMountException (std::string const & e) : KDBException (Key ()), msg (e)
	{
	}

	virtual const char * what () const noexcept override
	{
		return msg.c_str ();
	}
}; // namespace kdb
}

#endif

#endif
