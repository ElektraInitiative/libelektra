#include <boost/algorithm/string/replace.hpp>
#include <cppcms/view.h>
#include <sstream>

#include "authentication_application.hpp"
#include "conversion_application.hpp"
#include "database_application.hpp"
#include "exceptions.hpp"
#include "jwt.hpp"
#include "kdb.h"
#include "root_application.hpp"
#include "service.hpp"
#include "user_application.hpp"


namespace kdbrest
{

/**
     * @brief Constructor for main service class
     *
     * @param srv Takes a CppCms service
     */
RootApp::RootApp (cppcms::service & srv) : cppcms::application (srv)
{

	attach (new AuthenticationApp (srv), "auth", "/auth{1}", // mapping
		"/auth(/(.*))?", 1				 // dispatching
		);

	attach (new UserApp (srv), "user", "/user{1}", // mapping
		"/user(/(.*))?", 1			 // dispatching
		);

	attach (new DatabaseApp (srv), "database", "/database{1}", // mapping
		"/database(/(.*))?", 1				   // dispatching
		);

	attach (new ConversionApp (srv), "conversion", "/conversion{1}", // mapping
		"/conversion(/(.*))?", 1				 // dispatching
		);

	dispatcher ().assign ("", &RootApp::welcome, this);
	mapper ().assign ("");

	dispatcher ().assign ("/version", &RootApp::version, this);
	mapper ().assign ("version", "/version");

	mapper ().root ("");
}


void RootApp::welcome ()
{
	if (request ().request_method () == "GET")
	{
		cppcms::base_content c;
		render ("rest_interface", c);
	}
	else
	{
		RootApp::setMethodNotAllowed (response ()); // send HTTP 405
		return;
	}
}

void RootApp::version ()
{
	if (request ().request_method () == "GET")
	{
		cppcms::json::value data;

		data["elektra"]["version"] = KDB_VERSION;
		data["elektra"]["major"] = KDB_VERSION_MAJOR;
		data["elektra"]["minor"] = KDB_VERSION_MINOR;
		data["elektra"]["micro"] = KDB_VERSION_MICRO;
		data["api"] = std::to_string (ELEKTRA_REST_API_VERSION);

		RootApp::setOk (response (), data, "application/json");
	}
	else
	{
		RootApp::setMethodNotAllowed (response ()); // send HTTP 405
		return;
	}
}


/* STATIC */

void RootApp::setOkRaw (cppcms::http::response & response, std::string & data, const std::string content_type)
{
	// send actual response
	if (!content_type.empty ())
	{
		response.set_content_header (content_type);
	}
	response.status (cppcms::http::response::ok);
	response.out () << data;
}

void RootApp::setOk (cppcms::http::response & response, cppcms::json::value & data, const std::string content_type)
{
	// send actual response
	if (!content_type.empty ())
	{
		response.set_content_header (content_type);
	}
	response.status (cppcms::http::response::ok);
	response.out () << data;
}

void RootApp::setOk (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::ok, message, loca);
}

void RootApp::setMethodNotAllowed (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::method_not_allowed, message, loca);
}

void RootApp::setNotAcceptable (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::not_acceptable, message, loca);
}

void RootApp::setUnprocessableEntity (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, 422, message, loca);
}

void RootApp::setUnauthorized (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::unauthorized, message, loca);
}

void RootApp::setBadRequest (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::bad_request, message, loca);
}

void RootApp::setNotFound (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::not_found, message, loca);
}

void RootApp::setInternalServerError (cppcms::http::response & response, const std::string message, const std::string loca)
{
	RootApp::setHttpStatus (response, cppcms::http::response::internal_server_error, message, loca);
}


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

void RootApp::setHttpStatus (cppcms::http::response & response, const int status, const std::string & message, const std::string & loca)
{
	// send actual response
	response.content_type ("application/json");
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

void RootApp::setCORSHeaders (cppcms::http::response & response, const std::string allowedMethods)
{
	response.set_header ("Allow", allowedMethods);
	response.set_header ("Access-Control-Allow-Methods", allowedMethods);
	response.set_header ("Access-Control-Allow-Origin", ELEKTRA_REST_HEADER_RESPONSE_CORS_ORIGIN);
	response.set_header ("Access-Control-Allow-Headers", ELEKTRA_REST_HEADER_RESPONSE_CORS_HEADERS);
}

} // namespace kdbrest
