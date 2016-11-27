/**
 * @file
 *
 * @brief cppcms controller implementation managing authentication resources
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <ctime>
#include <iostream>
#include <memory>
#include <regex>
#include <stdlib.h>
#include <string.h>

#include <boost/algorithm/string/predicate.hpp>
#include <jwt/jwt.h>
#include <openssl/sha.h>

#include <authentication_application.hpp>
#include <config.hpp>
#include <crypto.hpp>
#include <root_application.hpp>
#include <service.hpp>
#include <user_application.hpp>

namespace kdbrest
{

/**
 * @brief the constructor for the authentication endopint application
 *
 * @param srv a service container
 */
AuthenticationApp::AuthenticationApp (cppcms::service & srv) : cppcms::application (srv)
{

	dispatcher ().assign ("", &AuthenticationApp::authenticate, this);
	mapper ().assign ("");
}

/**
 * @brief handler for the authentication resource
 *
 * the function will attempt an authentication with submitted credentials
 * found in the request() object. in case the attempt succeeds, a session
 * token will be returned in the response(), otherwise an appropriate
 * error message stating the issue.
 */
void AuthenticationApp::authenticate ()
{
	RootApp::setCORSHeaders (response (), "POST,OPTIONS");

	if (request ().request_method () == "POST")
	{
		// check if request data is of type application/json
		if (request ().content_type_parsed ().media_type () != MIME_APPLICATION_JSON)
		{
			RootApp::setNotAcceptable (response (), "You have supplied an unsupported Content-Type.",
						   "REQUEST_UNSUPPORTED_CONTENT_TYPE");
			return;
		}

		// try to parse request body data
		cppcms::json::value requestData;
		try
		{
			requestData = RootApp::parsePostDataAsJson (request ());
		}
		catch (kdbrest::exception::InvalidPostDataFormatException & e)
		{
			RootApp::setBadRequest (response (), "The submitted data is not of type application/json.",
						"REQUEST_MALFORMED_DATA");
			return;
		}

		// required request fields
		std::string username;
		std::string password;

		// find username
		try
		{
			username = requestData.get<std::string> ("username");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (response (), "You have to supply an username.", "AUTH_MISSING_USERNAME");
			return;
		}

		// find password
		try
		{
			password = requestData.get<std::string> ("password");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (response (), "You have to supply a password.", "AUTH_MISSING_PASSWORD");
			return;
		}

		// regex checks
		std::regex regex_username (REGEX_USERNAME);

		// before doing a lookup, validate that username input is safe
		if (!std::regex_match (username, regex_username))
		{
			RootApp::setUnauthorized (response (), "An account for the given username does not exist.",
						  "AUTH_UNKNOWN_USERNAME");
			return;
		}

		// attempt the actual authentication
		// first load the user
		model::User user (std::string (""));
		try
		{
			user = service::StorageEngine::instance ().getUser (username);
		}
		catch (exception::UserNotFoundException & e)
		{
			RootApp::setUnauthorized (response (), "An account for the given username does not exist.",
						  "AUTH_UNKNOWN_USERNAME");
			return;
		}
		// then compare the password
		std::string passwordEncrypted;
		if (!crypto::sha256_encrypt (password, passwordEncrypted))
		{
			RootApp::setInternalServerError (response (), "Could not hash password. Please try again.", "AUTH_UNKNOWN_ERROR");
			return;
		}
		if (user.getPasswordHash ().compare (passwordEncrypted) != 0)
		{
			RootApp::setUnauthorized (response (), "The entered password is wrong.", "AUTH_INVALID_PASSWORD");
			return;
		}

		/* authentication was successful, so lets build the token */
		std::string token;
		try
		{
			token = this->buildJWT (response (), user);
		}
		catch (kdbrest::exception::JwtCreationException & e)
		{
			// error is already set to response
			return;
		}

		// write response
		cppcms::json::value data;
		data["token"] = token;
		RootApp::setOk (response (), data, MIME_APPLICATION_JSON);
	}
	else if (request ().request_method () == "OPTIONS")
	{
		RootApp::setOk (response ());
		return;
	}
	else
	{
		RootApp::setMethodNotAllowed (response ()); // send HTTP 405
		return;
	}
}

/**
 * @brief validates the authorization header of a request()
 *
 * helper method that allows to validate whether a request contains a valid
 * session token or not. it checks for the authorization header, format and
 * validity of the token.
 * additionally the method can do checks on the authenticated users rank and
 * username (i.e. check for permissions or if is owner of something).
 *
 * @note if a rank and a username are given, it is sufficient that one of both matches the
 *       requirement in order for the validation to succeed.
 *
 * @param request a request
 * @param response a response
 * @param rank optionally the rank of a user can be checked
 * @param orUser optionally the name of a user can be checked
 * @return true if an authentication is present and the authenticated user
 *		   matches all requirements, false otherwise
 */
bool AuthenticationApp::validateAuthentication (cppcms::http::request & request, cppcms::http::response & response, const int rank,
						const std::string orUser)
{
	// authentication validation
	std::string headerAuthorization = request.http_authorization ();
	std::string token;
	if (headerAuthorization.empty ())
	{
		// find token in get_parameters
		token = request.get (PARAM_TOKEN);
		if (token.empty ())
		{
			RootApp::setUnauthorized (response, "Session token missing.", "NEED_AUTHENTICATION"); // send HTTP 401
			return false;									      // not successful
		}
	}
	else
	{
		if (!boost::starts_with (headerAuthorization, AUTH_HEADER_PREFIX))
		{
			RootApp::setUnauthorized (response, "Authentication header has wrong format.", "NEED_AUTHENTICATION");
			return false; // not successful
		}
		token = headerAuthorization.substr (AUTH_HEADER_PREFIX.size ());
	}

	/* request seems fine, so lets parse the token */
	jwt_t * jwt;

	// decode it
	if (jwt_decode (&jwt, token.c_str (), reinterpret_cast<const unsigned char *> (Config::jwt_encryption_key.c_str ()),
			Config::jwt_encryption_key.size ()) != 0)
	{
		RootApp::setUnauthorized (response, "Session token is invalid", "NEED_AUTHENTICATION"); // send HTTP 401
		return false;
	}

	std::unique_ptr<jwt_t, void (*) (jwt_t *)> jwt_ptr (jwt, jwt_free);

	// check issuer and other grants
	if (std::string (jwt_get_grant (jwt_ptr.get (), "issuer")) != std::string (ELEKTRA_REST_AUTHENTICATION_JWT_ISSUER) ||
	    jwt_get_grant_int (jwt_ptr.get (), "expires") < std::time (NULL))
	{
		RootApp::setUnauthorized (response, "Session token is invalid", "NEED_AUTHENTICATION"); // send HTTP 401
		return false;
	}

	model::User currentUser = AuthenticationApp::getCurrentUser (request);
	if (currentUser.getRank () < rank && (orUser.empty () || orUser != currentUser.getUsername ()))
	{
		RootApp::setUnauthorized (response, "This action requires higher permissions.", "USER_INSUFFICIENT_PERMISSIONS");
		return false; // not successful
	}

	return true; // authentication successful
}

/**
 * @brief helper method to retrieve a user model of the currently authenticated user
 *
 * the function parses the authorization header of a request,
 * validates its format and ensures a proper session token is set.
 * it then takes the token and finds the user it belongs to.
 *
 * @param request a request
 * @return model of the currently authenticated user, taken from the storage
 * @throw kdbrest::exception::NoCurrentUserException
 * @throw kdbrest::exception::UserNotFoundException
 */
model::User AuthenticationApp::getCurrentUser (cppcms::http::request & request)
{
	// authentication validation
	std::string headerAuthorization = request.http_authorization ();
	std::string token;
	if (headerAuthorization.empty ())
	{
		// find token in get_parameters
		token = request.get (PARAM_TOKEN);
		if (token.empty ())
		{
			throw exception::NoCurrentUserException ();
		}
	}
	else
	{
		if (!boost::starts_with (headerAuthorization, AUTH_HEADER_PREFIX))
		{
			throw exception::NoCurrentUserException ();
		}
		token = headerAuthorization.substr (AUTH_HEADER_PREFIX.size ());
	}

	/* request seems fine, lets build the jwt */
	jwt_t * jwt;

	if (jwt_decode (&jwt, token.c_str (), reinterpret_cast<const unsigned char *> (Config::jwt_encryption_key.c_str ()),
			Config::jwt_encryption_key.size ()) != 0)
	{
		throw exception::NoCurrentUserException ();
	}

	std::unique_ptr<jwt_t, void (*) (jwt_t *)> jwt_ptr (jwt, jwt_free);

	std::string username = std::string (jwt_get_grant (jwt_ptr.get (), "username"));
	if (username.empty ())
	{
		throw exception::NoCurrentUserException ();
	}

	try
	{
		model::User user = service::StorageEngine::instance ().getUser (username);
		return user;
	}
	catch (exception::UserNotFoundException & e)
	{
		throw exception::UserNotFoundException ();
	}
}

/**
 * @brief can be used to build a jwt for a user
 *
 * @param user a user model
 * @return a jwt token as string
 */
std::string AuthenticationApp::buildJWT (cppcms::http::response & resp, const model::User & user) const
{
	jwt_t * jwt;

	// reserve token memory
	if (jwt_new (&jwt) != 0 || !jwt)
	{
		RootApp::setInternalServerError (resp, "Something went wrong while creating the session token.", "AUTH_CREATE_TOKEN_ERROR");
		throw exception::JwtCreationException ();
	}

	// create smart pointer that ensures freeing the jwt
	std::unique_ptr<jwt_t, void (*) (jwt_t *)> jwt_ptr (jwt, jwt_free);

	// specify jwt algorithm and encryption key
	if (jwt_set_alg (jwt_ptr.get (), JWT_ALG_HS256, reinterpret_cast<const unsigned char *> (Config::jwt_encryption_key.c_str ()),
			 Config::jwt_encryption_key.size ()) != 0)
	{
		RootApp::setInternalServerError (resp, "Something went wrong while creating the session token.", "AUTH_CREATE_TOKEN_ERROR");
		throw exception::JwtCreationException ();
	}

	// add claims
	if (jwt_add_grant (jwt_ptr.get (), "issuer", ELEKTRA_REST_AUTHENTICATION_JWT_ISSUER) != 0 ||
	    jwt_add_grant (jwt_ptr.get (), "username", user.getUsername ().c_str ()) != 0 ||
	    jwt_add_grant_int (jwt_ptr.get (), "rank", user.getRank ()) != 0 ||
	    jwt_add_grant_int (jwt_ptr.get (), "expires", std::time (NULL) + Config::jwt_expiration_time) != 0)
	{
		RootApp::setInternalServerError (resp, "Something went wrong while creating the session token.", "AUTH_CREATE_TOKEN_ERROR");
		throw exception::JwtCreationException ();
	}

	// generate and return token
	return std::string (jwt_encode_str (jwt_ptr.get ()));
}

} // namespace kdbrest
