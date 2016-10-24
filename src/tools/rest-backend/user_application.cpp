#include <algorithm>
#include <ctime>
#include <iostream>
#include <regex>
#include <stdlib.h>

#include <boost/algorithm/string.hpp>

#include "authentication_application.hpp"
#include "cryptlite/sha256.h"
#include "root_application.hpp"
#include "service.hpp"
#include "user_application.hpp"

namespace kdbrest
{

/**
 * @brief the constructor of the user endpoint application.
 * @param srv a service container
 */
UserApp::UserApp (cppcms::service & srv) : cppcms::application (srv)
{
	dispatcher ().assign ("/?([a-zA-Z0-9\\-\\.]{3,20})?/{0,1}", &UserApp::handle, this, 1);
	mapper ().assign ("handle", "/{1}");
}

/**
 * @brief the main handle function for this endpoint. it checks permissions
 *		  and mapps the endpoint resources to their corresponding handlers.
 * @param username a username that may be provided as resource parameter
 */
void UserApp::handle (std::string username)
{
	using namespace cryptlite;

	RootApp::setCORSHeaders (response (), "GET,POST,PUT,DELETE,OPTIONS");

	// retrieve user details
	if (request ().request_method () == "GET")
	{
		// check if details of the currently authenticated user should be returned
		if (request ().get (PARAM_CURRENT) == "true")
		{
			try
			{
				model::User user = AuthenticationApp::getCurrentUser (request ());
				this->handleGetUnique (response (), user.getUsername ());
				return;
			}
			catch (exception::NoCurrentUserException & e)
			{
				RootApp::setBadRequest (response (), "You need to be authenticated to perform this action.",
							"NO_CURRENT_USER");
				return;
			}
			catch (exception::UserNotFoundException & e)
			{
				RootApp::setNotFound (response (), "The requested user does not exist.", "USER_NOT_FOUND");
				return;
			}
		}
		// or if we should return details for another user
		else
		{
			// first check permissions
			try
			{
				model::User user = AuthenticationApp::getCurrentUser (request ());
				if (user.getRank () < 50)
				{
					throw exception::InsufficientPermissionsException ();
				}

				// check if we should only return the user list
				if (username.empty ())
				{
					this->handleGet (request (), response ());
					return;
				}
				// or if we should return a single user
				else
				{
					this->handleGetUnique (response (), username);
					return;
				}
			}
			catch (exception::NoCurrentUserException & e)
			{
				RootApp::setUnauthorized (response (), "You need to be authenticated to perform this action.",
							  "NEED_AUTHENTICATION");
				return;
			}
			catch (exception::UserNotFoundException & e)
			{
				RootApp::setUnauthorized (response (), "You need to be authenticated to perform this action.",
							  "NEED_AUTHENTICATION");
				return;
			}
			catch (exception::InsufficientPermissionsException & e)
			{
				RootApp::setUnauthorized (response (), "You have insufficient permissions to perform this action.",
							  "USER_INSUFFICIENT_PERMISSIONS");
				return;
			}
		}
	}

	// register new user
	else if (request ().request_method () == "POST")
	{
		this->handleInsert (request (), response ());
	}

	// edit existing user
	else if (request ().request_method () == "PUT")
	{
		try
		{
			// retrieve currently authenticated user
			model::User user = AuthenticationApp::getCurrentUser (request ());

			// in case no target specified, update current user
			if (username.empty ())
			{
				username = user.getUsername ();
			}

			// check permissions
			if (user.getUsername () != username && user.getRank () < 100)
			{
				throw exception::InsufficientPermissionsException ();
			}

			// execute the update request
			this->handleUpdate (request (), response (), username, user.getRank () >= 100);
		}
		catch (exception::NoCurrentUserException & e)
		{
			RootApp::setUnauthorized (response (), "You need to be authenticated to perform this action.",
						  "NEED_AUTHENTICATION");
			return;
		}
		catch (exception::UserNotFoundException & e)
		{
			RootApp::setUnauthorized (response (), "You need to be authenticated to perform this action.",
						  "NEED_AUTHENTICATION");
			return;
		}
		catch (exception::InsufficientPermissionsException & e)
		{
			RootApp::setUnauthorized (response (), "You have insufficient permissions to perform this action.",
						  "USER_INSUFFICIENT_PERMISSIONS");
			return;
		}
	}

	// delete user
	else if (request ().request_method () == "DELETE")
	{
		try
		{
			// retrieve currently authenticated user
			model::User user = AuthenticationApp::getCurrentUser (request ());

			// in case no target specified, delete current user
			if (username.empty ())
			{
				username = user.getUsername ();
			}

			// check permissions
			if (user.getUsername () != username && user.getRank () < 100)
			{
				throw exception::InsufficientPermissionsException ();
			}

			// execute the delete request
			this->handleDelete (response (), username);
		}
		catch (exception::NoCurrentUserException & e)
		{
			RootApp::setUnauthorized (response (), "You need to be authenticated to perform this action.",
						  "NEED_AUTHENTICATION");
			return;
		}
		catch (exception::UserNotFoundException & e)
		{
			RootApp::setUnauthorized (response (), "You need to be authenticated to perform this action.",
						  "NEED_AUTHENTICATION");
			return;
		}
		catch (exception::InsufficientPermissionsException & e)
		{
			RootApp::setUnauthorized (response (), "You have insufficient permissions to perform this action.",
						  "USER_INSUFFICIENT_PERMISSIONS");
			return;
		}
	}

	// ask for allowed operations
	else if (request ().request_method () == "OPTIONS")
	{
		RootApp::setOk (response ());
		return;
	}

	// fallback for non-supported methods
	else
	{
		RootApp::setMethodNotAllowed (response ()); // send HTTP 405
		return;
	}
}

/**
 * @brief handles the retirval of a specific user entry
 * @param resp a response
 * @param username the username of the user whose information shall be retrieved
 */
void UserApp::handleGetUnique (cppcms::http::response & resp, std::string username)
{
	try
	{
		model::User user = service::StorageEngine::instance ().getUser (username);

		cppcms::json::value data;
		data["username"] = user.getUsername ();
		data["email"] = user.getEmail ();
		data["rank"] = user.getRank ();
		data["created_at"] = user.getCreatedAt ();

		RootApp::setOk (resp, data);
	}
	catch (exception::UserNotFoundException & e)
	{
		RootApp::setNotFound (resp, "The requested user does not exist.", "USER_NOT_FOUND");
		return;
	}
}

/**
 * @brief handles the retrieval of a list of user entries
 * @param req a request
 * @param resp a response
 */
void UserApp::handleGet (cppcms::http::request & req, cppcms::http::response & resp)
{
	std::vector<kdbrest::model::User> users = service::StorageEngine::instance ().getAllUsers ();

	this->processFiltering (req, users);
	this->processSorting (req, users);

	this->produceOutput (req, resp, users);
}

/**
 * @brief handles the creation of a new user entry
 * @param req a request
 * @param resp a response
 */
void UserApp::handleInsert (cppcms::http::request & req, cppcms::http::response & resp)
{
	using namespace cryptlite;

	// check if request data is of type application/json
	if (req.content_type_parsed ().media_type () != "application/json")
	{
		RootApp::setNotAcceptable (resp, "You have supplied an unsupported Content-Type.", "REQUEST_UNSUPPORTED_CONTENT_TYPE");
		return;
	}

	// try to parse request body data
	cppcms::json::value requestData;
	try
	{
		requestData = RootApp::parsePostDataAsJson (req);
	}
	catch (kdbrest::exception::InvalidPostDataFormatException & e)
	{
		RootApp::setBadRequest (resp, "The submitted data is not of type application/json.", "REQUEST_MALFORMED_DATA");
		return;
	}

	// required request fields
	std::string username;
	std::string password;
	std::string email;

	// find username
	try
	{
		username = requestData.get<std::string> ("username");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply an username.", "USER_CREATE_MISSING_USERNAME");
		return;
	}

	// find password
	try
	{
		password = requestData.get<std::string> ("password");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a password.", "USER_CREATE_MISSING_PASSWORD");
		return;
	}

	// find email
	try
	{
		email = requestData.get<std::string> ("email");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply an email.", "USER_CREATE_MISSING_EMAIL");
		return;
	}

	// valiate inputs
	std::regex regex_username (REGEX_USERNAME);
	std::regex regex_password (REGEX_PASSWORD);
	if (!std::regex_match (username, regex_username))
	{
		RootApp::setBadRequest (resp,
					"The given username is not valid. The username must be "
					"between 3 and 20 signs long and may contain only "
					"letters a-z (lower- and upper-case), the numbers 0-9 "
					"and dots (.) as well as dashes (-).",
					"USER_CREATE_INVALID_USERNAME");
		return; // quit early
	}
	if (!std::regex_match (password, regex_password))
	{
		RootApp::setBadRequest (resp,
					"The password must be at least 8 signs long and contain at "
					"least one lower-case letter, one upper-case letter and one "
					"digit.",
					"USER_CREATE_INVALID_PASSWORD");
		return; // quit early
	}
	if (!this->isValidEmail (email))
	{
		RootApp::setBadRequest (resp, "The given email is not a valid email.", "USER_CREATE_INVALID_EMAIL");
		return; // quit early
	}
	if (service::StorageEngine::instance ().userExists (username))
	{
		RootApp::setUnprocessableEntity (resp,
						 "An user with the given username does already exist. "
						 "Please choose another username.",
						 "USER_CREATE_USER_DOES_ALREADY_EXIST");
		return; // quit early
	}

	// create user model
	model::User u (username);
	u.setPasswordHash (sha256::hash_hex (password));
	u.setEmail (email);
	u.setRank (10);
	u.setCreatedAt (static_cast<long> (time (0)));

	// store user
	try
	{
		if (service::StorageEngine::instance ().createUser (u))
		{
			RootApp::setOk (resp, "The new user has been stored successfully!", "USER_CREATED_SUCCESSFULLY");
			return;
		}
		else
		{
			RootApp::setInternalServerError (resp,
							 "Something went wrong while creating your account. "
							 "Please try again.",
							 "USER_CREATE_UNKNOWN_ERROR");
			return;
		}
	}
	catch (exception::UserAlreadyExistsException & e)
	{
		RootApp::setUnprocessableEntity (resp,
						 "An user with the given username does already exist. "
						 "Please choose another username.",
						 "USER_CREATE_USER_DOES_ALREADY_EXIST");
		return;
	}
}

/**
 * @brief handles the update of a user entry.
 * @param req a request
 * @param resp a response
 * @param username the username of the user who shall be updated
 * @param canSetRank whether the currently authenticated user can update the rank or not
 */
void UserApp::handleUpdate (cppcms::http::request & req, cppcms::http::response & resp, std::string username, bool canSetRank)
{
	using namespace cryptlite;

	// check if the users exists
	if (!service::StorageEngine::instance ().userExists (username))
	{
		RootApp::setNotFound (resp, "A user with the given username does not exist.", "USER_NOT_FOUND");
		return;
	}

	// check if request data is of type application/json
	if (req.content_type_parsed ().media_type () != "application/json")
	{
		RootApp::setNotAcceptable (resp, "You have supplied an unsupported Content-Type.", "REQUEST_UNSUPPORTED_CONTENT_TYPE");
		return;
	}

	// try to parse request body data
	cppcms::json::value requestData;
	try
	{
		requestData = RootApp::parsePostDataAsJson (req);
	}
	catch (kdbrest::exception::InvalidPostDataFormatException & e)
	{
		RootApp::setBadRequest (resp, "The submitted data is not of type application/json.", "REQUEST_MALFORMED_DATA");
		return;
	}

	// possible request fields
	std::string password = requestData.get<std::string> ("password", "");
	std::string email = requestData.get<std::string> ("email", "");
	int rank = requestData.get<int> ("rank", -1);

	// valiate inputs
	std::regex regex_password (REGEX_PASSWORD);
	if (!password.empty () && !std::regex_match (password, regex_password))
	{
		RootApp::setBadRequest (resp,
					"The password must be at least 8 signs long and contain at "
					"least one lower-case letter, one upper-case letter and one "
					"digit.",
					"USER_UPDATE_INVALID_PASSWORD");
		return; // quit early
	}
	if (!email.empty () && !this->isValidEmail (email))
	{
		RootApp::setBadRequest (resp, "The given email is not a valid email.", "USER_UPDATE_INVALID_EMAIL");
		return; // quit early
	}
	if (rank != -1 && rank != 10 && rank != 50 && rank != 100)
	{
		RootApp::setBadRequest (resp, "The given rank is not available.", "USER_UPDATE_INVALID_RANK");
	}

	// get the user and update
	try
	{
		model::User user = service::StorageEngine::instance ().getUser (username);
		if (!email.empty ())
		{
			user.setEmail (email);
		}
		if (!password.empty ())
		{
			user.setPasswordHash (sha256::hash_hex (password));
		}
		if (canSetRank && rank != -1)
		{
			user.setRank (rank);
		}
		if (service::StorageEngine::instance ().updateUser (user))
		{
			RootApp::setOk (resp, "The user has been updated successfully!", "USER_UPDATED_SUCCESSFULLY");
			return;
		}
		else
		{
			RootApp::setInternalServerError (resp, "Something went wrong while updating the user.",
							 "USER_UPDATE_UNKNOWN_ERROR");
			return;
		}
	}
	catch (exception::UserNotFoundException & e)
	{
		RootApp::setNotFound (resp, "A user with the given username does not exist.", "USER_NOT_FOUND");
		return;
	}
}

/**
 * @brief handles a delete request for a user resource.
 * @param resp a response
 * @param username the username of the user who shall be deleted
 */
void UserApp::handleDelete (cppcms::http::response & resp, std::string username)
{
	try
	{
		model::User user = service::StorageEngine::instance ().getUser (username);

		service::StorageEngine::instance ().deleteUser (user);

		RootApp::setOk (resp, "The user has been deleted successfully.", "USER_DELETED_SUCCESSFULLY");
	}
	catch (exception::UserNotFoundException & e)
	{
		RootApp::setNotFound (resp, "The requested user does not exist.", "USER_NOT_FOUND");
		return;
	}
}

/**
 * @brief extracts the max number of rows to print from a request.
 * @param req a request
 * @return the max number of rows to print or the default value if not set
 */
inline int UserApp::getMaxrows (cppcms::http::request & req)
{
	int maxrows = ELEKTRA_REST_OUTPUT_MAX_ENTRIES;

	std::string s_maxrows = req.get (PARAM_ROWS);
	if (!s_maxrows.empty ())
	{
		try
		{
			maxrows = std::stoi (s_maxrows);
			if (maxrows > ELEKTRA_REST_OUTPUT_MAX_ENTRIES)
			{
				maxrows = ELEKTRA_REST_OUTPUT_MAX_ENTRIES;
			}
		}
		catch (std::invalid_argument & e)
		{
		}
	}

	return maxrows;
}

/**
 * @brief extracts the offset parameter from a request.
 * @param req a request
 * @return the offset extracted from the request parameter, if not set 0
 */
inline int UserApp::getOffset (cppcms::http::request & req)
{
	int offset = 0;

	std::string s_offset = req.get (PARAM_OFFSET);
	if (!s_offset.empty ())
	{
		try
		{
			offset = std::stoi (s_offset);
		}
		catch (std::invalid_argument & e)
		{
		}
	}

	return offset;
}

/**
 * @brief filters a vector of user entries based on parameters of a request.
 * @param req a request
 * @param users the user vector to filter
 */
inline void UserApp::processFiltering (cppcms::http::request & req, std::vector<model::User> & users)
{
	// retrieve parameter values
	std::string filter = req.get (PARAM_FILTER);
	std::string filterby = req.get (PARAM_FILTERBY);

	// only proceed if a filter is set
	if (!filter.empty ())
	{
		// if the filter is unknown, take default
		if (filterby != "all" && filterby != "username" && filterby != "email")
		{
			filterby = std::string (ELEKTRA_REST_OUTPUT_FILTERBY_USER_DEFAULT);
		}

		service::SearchEngine::instance ().findUsersByFilter (users, filter, filterby);
	}
}

/**
 * @brief sorts a vector of user entries based on parameters of a request.
 * @param req a request
 * @param users the user vector to sort
 */
inline void UserApp::processSorting (cppcms::http::request & req, std::vector<model::User> & users)
{
	// retrieve parameter values
	std::string sort = req.get (PARAM_SORT);
	std::string sortby = req.get (PARAM_SORTBY);

	// validate the sort direction input or set default
	if (!boost::iequals (sort, "asc") && !boost::iequals (sort, "desc"))
	{
		sort = std::string (ELEKTRA_REST_OUTPUT_SORT_USER_DEFAULT);
	}

	// validate the sortby input or set default
	std::vector<std::string> sortOptions = { "username", "email", "created_at", "rank" };
	if (std::find (sortOptions.begin (), sortOptions.end (), sortby) == sortOptions.end ())
	{
		sortby = std::string (ELEKTRA_REST_OUTPUT_SORTBY_USER_DEFAULT);
	}

	// do the sorting
	if (boost::iequals (sort, "asc"))
	{
		if (boost::iequals (sortby, "email"))
		{
			std::sort (users.begin (), users.end (), model::User::less_than_email);
		}
		else if (boost::iequals (sortby, "created_at"))
		{
			std::sort (users.begin (), users.end (), model::User::less_than_created_at);
		}
		else if (boost::iequals (sortby, "rank"))
		{
			std::sort (users.begin (), users.end (), model::User::less_than_rank);
		}
		else // last option "username"
		{
			std::sort (users.begin (), users.end (), model::User::less_than_username);
		}
	}
	else
	{
		if (boost::iequals (sortby, "email"))
		{
			std::sort (users.begin (), users.end (), model::User::greater_than_email);
		}
		else if (boost::iequals (sortby, "created_at"))
		{
			std::sort (users.begin (), users.end (), model::User::greater_than_created_at);
		}
		else if (boost::iequals (sortby, "rank"))
		{
			std::sort (users.begin (), users.end (), model::User::greater_than_rank);
		}
		else // last option "username"
		{
			std::sort (users.begin (), users.end (), model::User::greater_than_username);
		}
	}
}

/**
 * @brief creates the output for a list of users with all helping attributes
 *	      like number of entries, offset, etc.
 * @note not all users in the list may actually be printed to the output.
 *       which users are used in the output depends on the offset and maxrows.
 * @param req a request
 * @param resp a response
 * @param users a list of users, of which some may be used for the output
 */
inline void UserApp::produceOutput (cppcms::http::request & req, cppcms::http::response & resp, std::vector<model::User> & users)
{
	int offset = this->getOffset (req);
	int maxrows = this->getMaxrows (req);

	cppcms::json::value data;
	int elements = static_cast<int> (users.size ()) - offset;
	elements = (elements > 0) ? elements : 0;
	elements = (elements < maxrows) ? elements : maxrows;
	int remaining = (static_cast<int> (users.size ()) - offset) - elements;
	data["offset"] = offset;
	data["elements"] = elements;
	data["remaining"] = (remaining > 0) ? remaining : 0;

	if (offset < static_cast<int> (users.size ()))
	{
		int index = 0;
		int offset_it = 0;
		for (auto & elem : users)
		{

			// break for maxrows
			if (index >= maxrows)
			{
				break;
			}

			// skip to offset
			if (offset_it < offset)
			{
				offset_it++;
				continue;
			}

			data["users"][index]["username"] = elem.getUsername ();
			data["users"][index]["email"] = elem.getEmail ();
			data["users"][index]["rank"] = elem.getRank ();
			data["users"][index]["created_at"] = elem.getCreatedAt ();
			index++;
		}
	}

	RootApp::setOk (resp, data, "application/json");
}

/**
 * @brief does basic checks on a string to validate whether it is an
 *		  email or not. in order to do so, it first checks if the string
 *		  is empty, then looks for a @ followed by a . for the TLD
 * @param email a string containing the email to validate
 * @return true in case the email is considered valid, false otherwise
 */
bool UserApp::isValidEmail (std::string & email)
{
	if (email.empty ()) return false;
	size_t at_index = email.find_first_of ('@', 1); // there has to be one sign before @
	return at_index != std::string::npos && email.find_first_of ('.', at_index) != std::string::npos; // . has to be after @
}

} // namespace kdbrest
