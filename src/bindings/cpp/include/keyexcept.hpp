#ifndef ELEKTRA_KEY_EXCEPT_HPP
#define ELEKTRA_KEY_EXCEPT_HPP

#ifndef USER_DEFINED_EXCEPTIONS

namespace kdb
{

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
		return  "Exception thrown by a Key, typically "
			"because you called a method on a null key. "
			"Make sure to check this with !key first";
	}
};

class KeyTypeMismatch: public KeyException
{
public:
	virtual const char* what() const throw()
	{
		return  "Binary/String key mismatch, use proper "
			"getString()/getBinary() or use getValue() to get both.";
	}
};

class KeyInvalidName : public KeyException
{
public:
	virtual const char* what() const throw()
	{
		return "Invalid Keyname: keyname needs to start with user/ or system/";
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

class KeyBadMeta : public KeyMetaException
{
public:
	virtual const char* what() const throw()
	{
		return  "Could not convert meta data to requested type."
			"use getMeta<const Key> or getMeta<std::string>.";
	}
};

}

#endif

#endif
