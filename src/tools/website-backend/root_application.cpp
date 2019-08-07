/**
 * @file
 *
 * @brief cppcms controller managing global resources
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <sstream>

#include <boost/algorithm/string/replace.hpp>
#include <cppcms/view.h>

#include <authentication_application.hpp>
#include <config.hpp>
#include <conversion_application.hpp>
#include <database_application.hpp>
#include <exceptions.hpp>
#include <elektra/kdb.h>
#include <kdblogger.h>
#include <root_application.hpp>
#include <service.hpp>
#include <user_application.hpp>

namespace kdbrest
{

/**
 * @brief constructor for main service class
 *
 * @param srv a service container
 */
RootApp::RootApp (cppcms::service & srv) : cppcms::application (srv)
{
	attach (new AuthenticationApp (srv), "auth", "/auth{1}", // mapping
		"/auth(/(.*))?", 1				 // dispatching
	);

	attach (new UserApp (srv), "user", "/user{1}", // mapping
		"/user(/(.*))?", 1		       // dispatching
	);

	attach (new DatabaseApp (srv), "database", "/database{1}", // mapping
		"/database(/(.*))?", 1				   // dispatching
	);

	attach (new ConversionApp (srv), "conversion", "/conversion{1}", // mapping
		"/conversion(/(.*))?", 1				 // dispatching
	);

	dispatcher ().assign ("/?", &RootApp::welcome, this);
	mapper ().assign ("");

	dispatcher ().assign ("/version", &RootApp::version, this);
	mapper ().assign ("version", "/version");

	mapper ().root ("");
}

/**
 * @brief handler for the root of the whole rest service
 *
 * responds with a redirect to a configurable link, should be
 * the API description.
 */
void RootApp::welcome ()
{
	RootApp::setCORSHeaders (response (), "GET,OPTIONS");

	if (request ().request_method () == "GET")
	{
		const std::string raw = request ().get (PARAM_RAW);
		if (!raw.empty () && raw == "true")
		{
			RootApp::setSeeOther (response (), Config::instance ().getConfig ().get<std::string> ("api.description.raw"));
			return;
		}

		RootApp::setSeeOther (response (), Config::instance ().getConfig ().get<std::string> ("api.description.html"));
		return;
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
 * @brief handler for the version resource of the endpoint
 *
 * responds with versions for the API and the used Elektra version.
 */
void RootApp::version ()
{
	RootApp::setCORSHeaders (response (), "GET,OPTIONS");

	if (request ().request_method () == "GET")
	{
		bool error = false;
		cppcms::json::value data;

		// api version
		data["api"] = ELEKTRA_REST_API_VERSION;

		// elektra version
		try
		{
			kdb::KDB kdb;
			kdb::KeySet ks;
			kdb.get (ks, "system/elektra/version/constants");

			auto ksLookup = [&ks, &error](const std::string key, cppcms::json::value & out, const bool isInt) {
				kdb::Key k = ks.lookup (key);
				if (!k)
				{
					error = true;
				}
				else
				{
					if (isInt)
					{
						out = k.get<int> ();
					}
					else
					{
						out = k.getString ();
					}
				}
			};

			ksLookup ("system/elektra/version/constants/KDB_VERSION", data["elektra"]["version"], false);
			ksLookup ("system/elektra/version/constants/KDB_VERSION_MAJOR", data["elektra"]["major"], true);
			ksLookup ("system/elektra/version/constants/KDB_VERSION_MINOR", data["elektra"]["minor"], true);
			ksLookup ("system/elektra/version/constants/KDB_VERSION_MICRO", data["elektra"]["micro"], true);
		}
		catch (kdb::KDBException const & e)
		{
			error = true;
		}

		if (error)
		{
			// in case we could not retrieve the run-time version,
			// use compile-time version
			elektraLog (ELEKTRA_LOG_LEVEL_ERROR, __FUNCTION__, __FILE__, __LINE__,
				    "Could not find run-time version of Elektra installation, using compile-time version as fallback.");
			data["elektra"]["version"] = KDB_VERSION;
			data["elektra"]["major"] = KDB_VERSION_MAJOR;
			data["elektra"]["minor"] = KDB_VERSION_MINOR;
			data["elektra"]["micro"] = KDB_VERSION_MICRO;
		}

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


/* STATIC */

/**
 * @brief helper method that allows to set custom data within the response.
 *
 * @param response a response
 * @param data a plain string containing the response body data
 * @param content_type the content type to be used for the response
 */
void RootApp::setOkRaw (cppcms::http::response & response, const std::string data, const std::string content_type)
{
	// send actual response
	if (!content_type.empty ())
	{
		response.set_content_header (content_type);
	}
	response.status (cppcms::http::response::ok);
	response.out () << data;
}

/**
 * @brief helper method that allows to set json data within the response.
 *
 * @param response a response
 * @param data json data to be used as body in response
 * @param content_type the content type to be used for the response
 */
void RootApp::setOk (cppcms::http::response & response, cppcms::json::value & data, const std::string content_type)
{
	// send actual response
	if (!content_type.empty ())
	{
		response.set_content_header (content_type);
	}
	response.content_type (MIME_APPLICATION_JSON + ";" + CHARSET_UTF_8);
	response.status (cppcms::http::response::ok);
	response.out () << data;
}

/**
 * @brief helper method that allows to set a message and a localization string within the response.
 *
 * @param response a response
 * @param message a message, i.e. success message because of http 200
 * @param loca a localization string, may not contain spaces (e.g. USER_CREATED_SUCCESSFULLY)
 */
void RootApp::setOk (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::ok, message, loca);
}

/**
 * @brief helper method that allows to set a method not allowed response
 *
 * @param response a response
 * @param message a message, i.e. method not allowed message because of http 405
 * @param loca a localization string, may not contain spaces (e.g. METHOD_NOT_ALLOWED)
 */
void RootApp::setMethodNotAllowed (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::method_not_allowed, message, loca);
}

/**
 * @brief helper method that allows to set a not acceptable response
 *
 * @param response a response
 * @param message a message, i.e. not acceptable message because of http 406
 * @param loca a localization string, may not contain spaces (e.g. UNSUPPORTED_CONTENT_TYPE)
 */
void RootApp::setNotAcceptable (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::not_acceptable, message, loca);
}

/**
 * @brief helper method that allows to set a unprocessable entity response
 *
 * @param response a response
 * @param message a message, i.e. unprocessable entity message because of http 422
 * @param loca a localization string, may not contain spaces (e.g. USER_ALREADY_EXISTS)
 */
void RootApp::setUnprocessableEntity (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, 422, message, loca);
}

/**
 * @brief helper method that allows to set an unauthorized response
 *
 * @param response a response
 * @param message a message, i.e. unauthorized message because of http 401
 * @param loca a localization string, may not contain spaces (e.g. NEED_AUTHENTICATION)
 */
void RootApp::setUnauthorized (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::unauthorized, message, loca);
}

/**
 * @brief helper method that allows to set a bad request response
 *
 * @param response a response
 * @param message a message, i.e. bad request message because of http 400
 * @param loca a localization string, may not contain spaces (e.g. USER_MISSING_USERNAME)
 */
void RootApp::setBadRequest (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::bad_request, message, loca);
}

/**
 * @brief helper method that allows to set a not found response
 *
 * @param response a response
 * @param message a message, i.e. not found message because of http 404
 * @param loca a localization string, may not contain spaces (e.g. USER_DOES_NOT_EXIST)
 */
void RootApp::setNotFound (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::not_found, message, loca);
}

/**
 * @brief helper method that allows to set an internal server error response
 *
 * @param response a response
 * @param message a message, i.e. internal server error message because of http 500
 * @param loca a localization string, may not contain spaces (e.g. AUTH_CREATE_TOKEN_ERROR)
 */
void RootApp::setInternalServerError (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::internal_server_error, message, loca);
}

/**
 * @brief helper method that allows to send a redirect response containing a location header
 *
 * @param response a response
 * @param location the location the browser is sent to, should be an URL
 */
void RootApp::setSeeOther (cppcms::http::response & response, const std::string location)
{
	response.status (cppcms::http::response::see_other);
	response.set_header ("Location", location);
}

/**
 * @brief helper method that attempts to parse post data as json
 *
 * @param request a request
 * @return a json value containing the parsed post data
 */
cppcms::json::value RootApp::parsePostDataAsJson (cppcms::http::request & request)
{
	std::pair<void *, size_t> post_data = request.raw_post_data ();
	std::istringstream ss (std::string (reinterpret_cast<char const *> (post_data.first), post_data.second));
	cppcms::json::value data;
	if (!data.load (ss, true))
	{
		throw kdbrest::exception::InvalidPostDataFormatException ();
	}
	return data;
}


/* STATIC PRIVATE */

/**
 * @brief helper method that sets the http status of a response, as well as an error message and loca string
 *
 * @param response a response
 * @param status the http status to set
 * @param message a message to use in response body
 * @param loca a localization string to use in response body
 */
void RootApp::setHttpStatus (cppcms::http::response & response, const int status, const std::string & message, const std::string & loca)
{
	// send actual response
	response.content_type (MIME_APPLICATION_JSON + ";" + CHARSET_UTF_8);
	response.status (status);
	cppcms::json::value data;
	data["status"] = cppcms::http::response::status_to_string (status);
	if (!message.empty ())
	{
		data["message"] = message;
	}
	if (!loca.empty ())
	{
		data["i18n"] = loca;
	}
	response.out () << data;
}

/**
 * @brief helper method that sets CORS headers for a response, as well as allowed methods
 *
 * @param response a response
 * @param allowedMethods a comma separated list of allowed methods, e.g. "GET,POST,PUT,OPTIONS"
 */
void RootApp::setCORSHeaders (cppcms::http::response & response, const std::string allowedMethods)
{
	response.set_header ("Allow", allowedMethods);
	response.set_header ("Access-Control-Allow-Methods", allowedMethods);
	response.set_header ("Access-Control-Allow-Origin", ELEKTRA_REST_HEADER_RESPONSE_CORS_ORIGIN);
	response.set_header ("Access-Control-Allow-Headers", ELEKTRA_REST_HEADER_RESPONSE_CORS_HEADERS);
}

} // namespace kdbrest
