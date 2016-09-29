#ifndef ELEKTRA_REST_EXCEPTIONS_HEADER_GUARD
#define ELEKTRA_REST_EXCEPTIONS_HEADER_GUARD

#include <exception>

namespace kdbrest
{

namespace exception
{

class ElektraRestException : public std::exception
{
public:
	inline ElektraRestException ()
	{
	}
	inline ElektraRestException (const std::string & msg)
	{
		errorMsg = msg;
	}
	virtual const char * what () const throw ()
	{
		return errorMsg.c_str ();
	}

protected:
	std::string errorMsg;
};

class EntryNotFoundException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "An entry with the given name does not exist in the database";
	}
};

class EntryAlreadyExistsException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "An entry with the given name does already exist in the database";
	}
};

class UserNotFoundException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "An user with the given name does not exist in the database";
	}
};

class UserAlreadyExistsException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "An user with the given name does already exist in the database";
	}
};

class SubkeyNotFoundException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The requested sub key was not found";
	}
};

class NoCurrentUserException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "Currently there is no user authenticated";
	}
};

class FileformatPluginException : public ElektraRestException
{
public:
	inline FileformatPluginException (const std::string & msg) : ElektraRestException (msg)
	{
	}
};

class UnsupportedConfigurationFormatException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The configuration could not be converted because the "
		       "format is unknown or no plugin available/enabled.";
	}
};

class ParseConfigurationException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The configuration could not be parsed within the given format.";
	}
};

class EntryValidationException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The request data did not contain any valid entry.";
	}
};

class InvalidPostDataFormatException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The request did not contain post data in the required format.";
	}
};

class InsufficientPermissionsException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The authenticated user has insufficient permissions to perform the action.";
	}
};

} // namespace exception

} // namespace kdbrest

#endif
