/**
 * @file
 *
 * @brief header for cppcms controller implementation managing global resources
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_ROOTAPP_HPP
#define ELEKTRA_REST_ROOTAPP_HPP

#include <chrono>
#include <iostream>
#include <random>
#include <stdlib.h>
#include <string>
#include <vector>

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

// STATIC CONSTANTS

// parameter used for offsets of entry lists
static const std::string PARAM_OFFSET = "offset";
// parameter used for max number of rows for output of entry lists
static const std::string PARAM_ROWS = "rows";
// parameter used for the sorting of entry lists (asc, desc)
static const std::string PARAM_SORT = "sort";
// parameter used for the field to sort after of entry lists
static const std::string PARAM_SORTBY = "sortby";
// parameter used for the filtering of entry lists
static const std::string PARAM_FILTER = "filter";
// parameter used for the field of filtering of entry lists
static const std::string PARAM_FILTERBY = "filterby";

// mime type for application/json
static const std::string MIME_APPLICATION_JSON = "application/json";
// mime type for text/plain
static const std::string MIME_TEXT_PLAIN = "text/plain";

// parameter value used to sort ascending
static const std::string PARAM_VAL_SORT_ASC = "asc";
// parameter value used to sort descending
static const std::string PARAM_VAL_SORT_DESC = "desc";

/**
 * @brief main controller for the kdb rest service
 */
class RootApp : public cppcms::application
{
public:
	RootApp (cppcms::service & srv);

	virtual void welcome ();
	virtual void version ();

	static void setOkRaw (cppcms::http::response & response, const std::string data,
			      const std::string content_type = std::string (MIME_TEXT_PLAIN));
	static void setOk (cppcms::http::response & response, cppcms::json::value & data,
			   const std::string content_type = std::string (MIME_APPLICATION_JSON));
	static void setOk (cppcms::http::response & response, const std::string message = std::string (),
			   const std::string loca = std::string ());
	static void setMethodNotAllowed (cppcms::http::response & response, const std::string message = std::string (),
					 const std::string loca = std::string ());
	static void setNotAcceptable (cppcms::http::response & response, const std::string message = std::string (),
				      const std::string loca = std::string ());
	static void setUnprocessableEntity (cppcms::http::response & response, const std::string message = std::string (),
					    const std::string loca = std::string ());
	static void setUnauthorized (cppcms::http::response & response, const std::string message, const std::string loca);
	static void setBadRequest (cppcms::http::response & response, const std::string message, const std::string loca);
	static void setNotFound (cppcms::http::response & response, const std::string message, const std::string loca);
	static void setInternalServerError (cppcms::http::response & response, const std::string message = std::string (),
					    const std::string loca = std::string ());
	static void setSeeOther (cppcms::http::response & response, const std::string location);

	static void setCORSHeaders (cppcms::http::response & response, const std::string allowedMethods);

	static cppcms::json::value parsePostDataAsJson (cppcms::http::request & request);

private:
	static void setHttpStatus (cppcms::http::response & response, const int status, const std::string & message,
				   const std::string & loca);
};

} // namespace kdbrest

#endif
