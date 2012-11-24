#ifndef ELEKTRA_KDB_EXCEPT_HPP
#define ELEKTRA_KDB_EXCEPT_HPP

#ifndef USER_DEFINED_EXCEPTIONS

#include <keyexcept.hpp>

#include <kdbio.hpp>

namespace kdb {

class KDBException : public Exception
{
public:
	KDBException (Key key) :
		m_key (key)
	{}

	virtual ~KDBException() throw()
	{}

	virtual const char* what() const throw()
	{
		/*
		std::ostringstream m_str;
		if (m_str.str().empty())
		{
			printError(m_str, m_key);
			printWarnings(m_str, m_key);
		}
		return m_str.str().c_str();
		*/
		return "KDBException";
	}
private:
	Key m_key;
};

}

#endif

#endif
