#ifndef ELEKTRA_KEY_EXCEPT_HPP
#define ELEKTRA_KEY_EXCEPT_HPP

#ifndef USER_DEFINED_EXCEPTIONS

namespace kdb {

class Exception : public std::exception
{
public:
	virtual const char* what() const throw()
	{
		return "Exception thrown by Elektra";
	}
};

class KeyException : public Exception
{
public:
	virtual const char* what() const throw()
	{
		return "Exception thrown by a Key, typically because you called a function on a null key";
	}
};

class KeyTypeMismatch: public KeyException
{
public:
	virtual const char* what() const throw()
	{
		return "Binary or String key mismatch";
	}
};

class KeyInvalidName : public KeyException
{
public:
	virtual const char* what() const throw()
	{
		return "Invalid Keyname";
	}
};

class KeyMetaException : public KeyException
{
public:
	virtual const char* what() const throw()
	{
		return "Exception thrown by Key Meta Data related Operations";
	}
};

class KeyNoSuchMeta : public KeyMetaException
{
public:
	virtual const char* what() const throw()
	{
		return "No such meta data";
	}
};

class KeyBadMeta : public KeyMetaException
{
public:
	virtual const char* what() const throw()
	{
		return "Could not convert bad meta data";
	}
};

}

#endif

#endif
