/**
 * @file
 *
 * @brief exceptions used in the REST backend
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_EXCEPTIONS_HPP
#define ELEKTRA_REST_EXCEPTIONS_HPP

#include <exception>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

/**
 * @brief namespace for exceptions
 */
namespace exception
{

/**
 * @brief base exception to be used within the rest service.
 */
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

/**
 * @brief exception to be used if a snippet entry was not found.
 */
class EntryNotFoundException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "An entry with the given name does not exist in the database";
	}
};

/**
 * @brief exception to be used if a snippet entry with a certain key does already exist.
 */
class EntryAlreadyExistsException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "An entry with the given name does already exist in the database";
	}
};

/**
 * @brief exception to be used if a user entry was not found.
 */
class UserNotFoundException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "An user with the given name does not exist in the database";
	}
};

/**
 * @brief exception to be used if a user entry with a certain username does already exist.
 */
class UserAlreadyExistsException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "An user with the given name does already exist in the database";
	}
};

/**
 * @brief exception to be used if a requested subkey was not found, i.e. email of user.
 */
class SubkeyNotFoundException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The requested sub key was not found";
	}
};

/**
 * @brief exception to be used if no authenticated user was found (i.e. no valid session token).
 */
class NoCurrentUserException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "Currently there is no user authenticated";
	}
};

/**
 * @brief exception to be used for plugins.
 * 
 * @param msg custom exception error message
 */
class FileformatPluginException : public ElektraRestException
{
public:
	inline FileformatPluginException (const std::string & msg) : ElektraRestException (msg)
	{
	}
};

/**
 * @brief exception to be used if an unsupported configuration format was used within an import/export.
 */
class UnsupportedConfigurationFormatException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The configuration could not be converted because the "
		       "format is unknown or no plugin available/enabled.";
	}
};

/**
 * @brief exception to be used if a snippet cannot be consumed or represented by a plugin.
 */
class ParseConfigurationException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The configuration could not be parsed within the given format.";
	}
};

/**
 * @brief exception to be used if the validation of a snippet entry failed.
 */
class EntryValidationException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The request data did not contain any valid entry.";
	}
};

/**
 * @brief exception to be used if the submitted post data does not match the requirement.
 */
class InvalidPostDataFormatException : public ElektraRestException
{
public:
	virtual const char * what () const throw ()
	{
		return "The request did not contain post data in the required format.";
	}
};

/**
 * @brief exception to be used if the currently authenticated user has insufficient permissions.
 */
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
