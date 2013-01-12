#ifndef ELEKTRA_KDB_EXCEPT_HPP
#define ELEKTRA_KDB_EXCEPT_HPP

#ifndef USER_DEFINED_EXCEPTIONS

#include <keyexcept.hpp>

#include <string>
#include <sstream>

#include <kdbio.hpp>

namespace kdb
{

class KDBException : public Exception
{
public:
	KDBException (Key key) :
		m_key(key),
		m_str()
	{}

	virtual ~KDBException() throw()
	{}

	virtual const char* what() const throw()
	{
		std::stringstream ss;
		if (m_str.empty())
		{
			// note that the code will be re-evaluated
			// if it prints nothing, but an expensive
			// function not printing anything seems
			// to be unlikely.
			kdb::printError(ss, m_key);
			kdb::printWarnings(ss, m_key);
			m_str = ss.str();
		}
		return m_str.c_str();
	}
private:
	Key m_key;
	mutable std::string m_str;
};

}

#endif

#endif
