#include <ctime>
#include <iostream>
#include <regex>
#include <stdlib.h>

#include "cryptlite/sha256.h"

#include "authentication_application.hpp"
#include "jwt.hpp"
#include "root_application.hpp"
#include "service.hpp"
#include "user_application.hpp"

namespace kdbrest
{

AuthenticationApp::AuthenticationApp (cppcms::service & srv) : cppcms::application (srv)
{

	dispatcher ().assign ("", &AuthenticationApp::authenticate, this);
	mapper ().assign ("");
}

void AuthenticationApp::authenticate ()
{
	using namespace cryptlite;

	if (request ().request_method () == "POST")
	{
		// check if request data is of type application/json
		if (request ().content_type_parsed ().media_type () != "application/json")
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
		model::User u (std::string (""));
		try
		{
			u = service::StorageEngine::instance ().getUser (username);
		}
		catch (exception::UserNotFoundException & e)
		{
			RootApp::setUnauthorized (response (), "An account for the given username does not exist.",
						  "AUTH_UNKNOWN_USERNAME");
			return;
		}
		// then compare the password
		if (u.getPasswordHash ().compare (sha256::hash_hex (password)) != 0)
		{
			RootApp::setUnauthorized (response (), "The entered password is wrong.", "AUTH_INVALID_PASSWORD");
			return;
		}

		// authentication was successful, so lets generate the token
		jwt::JWTBuilder jwtBuilder;
		std::string token;
		try
		{

			token = jwtBuilder.setAlgorithm (jwt::JWTAlgorithm::HS256)
					.setAuthType (jwt::AuthType::JWT)
					.setIssuer (ELEKTRA_REST_AUTHENTICATION_JWT_ISSUER)
					.setExpirationDate (std::time (NULL) + ELEKTRA_REST_AUTHENTICATION_JWT_EXPIRATION_TIME)
					.setClaim (INDEX_USERNAME, username)
					.setClaim (INDEX_RANK, std::to_string (u.getRank ()))
					.generateToken (ELEKTRA_REST_AUTHENTICATION_ENCRYPT_KEY);
		}
		catch (const jwt::exception::JWTBuildException & e)
		{
			RootApp::setInternalServerError (response (), "Something went wrong while creating the session token.",
							 "AUTH_CREATE_TOKEN_ERROR");
			return; // quit early due to error
		}

		// write response
		cppcms::json::value data;
		data["token"] = token;
		RootApp::setOk (response (), data);
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

bool AuthenticationApp::validateAuthentication (cppcms::http::request & request, cppcms::http::response & response, int rank,
						std::string orUser)
{
#ifdef ELEKTRA_REST_NO_AUTH
	return true;
#endif

	// authentication validation
	std::string headerAuthorization = request.http_authorization ();
	std::string token;
	if (headerAuthorization.empty ())
	{
		// find token in get_parameters
		token = request.get (INDEX_TOKEN);
		if (token.empty ())
		{
			RootApp::setUnauthorized (response, "Session token missing.", "NEED_AUTHENTICATION"); // send HTTP 401
			return false;									      // not successful
		}
	}
	else
	{
		if (!(headerAuthorization.compare (0, AUTH_HEADER_PREFIX.size (), AUTH_HEADER_PREFIX)) == 0)
		{
			RootApp::setUnauthorized (response, "Authentication header has wrong format.", "NEED_AUTHENTICATION");
			return false; // not successful
		}
		token = headerAuthorization.substr (AUTH_HEADER_PREFIX.size ());
	}

	jwt::JWTValidator jwtValidator;
	jwtValidator.setToken (token).setIssuer (ELEKTRA_REST_AUTHENTICATION_JWT_ISSUER);
	if (token.size () == 0 || !jwtValidator.validate (ELEKTRA_REST_AUTHENTICATION_ENCRYPT_KEY))
	{
		RootApp::setUnauthorized (response, "Session token is invalid", "NEED_AUTHENTICATION"); // send HTTP 401
		return false;										// not successful
	}

	model::User currentUser = AuthenticationApp::getCurrentUser (request);
	if (currentUser.getRank () < rank && (orUser.empty () || orUser != currentUser.getUsername ()))
	{
		RootApp::setUnauthorized (response, "This action requires higher permissions.", "USER_INSUFFICIENT_PERMISSIONS");
		return false; // not successful
	}

	return true; // authentication successful
}

model::User AuthenticationApp::getCurrentUser (cppcms::http::request & request)
{
	// authentication validation
	std::string headerAuthorization = request.http_authorization ();
	std::string token;
	if (headerAuthorization.empty ())
	{
		// find token in get_parameters
		token = request.get (INDEX_TOKEN);
		if (token.empty ())
		{
			throw exception::NoCurrentUserException ();
		}
	}
	else
	{
		if (!(headerAuthorization.compare (0, AUTH_HEADER_PREFIX.size (), AUTH_HEADER_PREFIX)) == 0)
		{
			throw exception::NoCurrentUserException ();
		}
		token = headerAuthorization.substr (AUTH_HEADER_PREFIX.size ());
	}

	jwt::JWTValidator jwtValidator;
	jwtValidator.setToken (token).setIssuer (ELEKTRA_REST_AUTHENTICATION_JWT_ISSUER);

	std::string username;
	jwt::jwt jwt = jwtValidator.getParsedJwt ();
	for (auto & elem : jwt.s_payload.claims)
	{
		if (elem.first.compare (INDEX_USERNAME) == 0)
		{
			username = elem.second;
		}
	}

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

} // namespace kdbrest
