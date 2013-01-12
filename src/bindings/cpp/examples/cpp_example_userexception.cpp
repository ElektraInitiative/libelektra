#include <stdexcept>

class UserException : public std::exception
{
public:
	virtual const char* what() const throw()
	{
		return "User Exception";
	}
};

namespace kdb
{

class Exception : public UserException
{
public:
	virtual const char* what() const throw()
	{
		return "User Exception: Exception thrown by Elektra";
	}
};

class KeyException : public Exception
{
public:
	virtual const char* what() const throw()
	{
		return "User Exception: Exception thrown by a Key";
	}
};

class KeyTypeMismatch: public KeyException
{
public:
	virtual const char* what() const throw()
	{
		return "User Exception: Binary or String key mismatch";
	}
};

class KeyInvalidName : public KeyException
{
public:
	virtual const char* what() const throw()
	{
		return "User Exception: Invalid Keyname";
	}
};

class KeyMetaException : public KeyException
{
public:
	virtual const char* what() const throw()
	{
		return "User Exception: Exception thrown by Key Meta Data related Operations";
	}
};

class KeyNoSuchMeta : public KeyMetaException
{
public:
	virtual const char* what() const throw()
	{
		return "User Exception: No such meta data";
	}
};

class KeyBadMeta : public KeyMetaException
{
public:
	virtual const char* what() const throw()
	{
		return "User Exception: Could not convert bad meta data";
	}
};

}

#define USER_DEFINED_EXCEPTIONS
#include <key.hpp>

namespace kdb
{

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
		return "User Exception: KDB";
	}
private:
	Key m_key;
};

}


#include <kdb.hpp>

int main()
{
	kdb::Key k ("abc", KEY_END);
	kdb::KDB kdb;
	kdb::KeySet ks;
	kdb.get(ks, k);
}
