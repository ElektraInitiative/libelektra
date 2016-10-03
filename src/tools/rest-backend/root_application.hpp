#ifndef ELEKTRA_REST_ROOTAPP_HEADER_GUARD
#define ELEKTRA_REST_ROOTAPP_HEADER_GUARD

#include <chrono>
#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <iostream>
#include <random>
#include <stdlib.h>
#include <string>
#include <vector>

/**
 * @brief This is the main namespace for all classes belonging
 * to the kdb rest service
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

/**
     * @brief Main class for the kdb rest service, serves as
     * coordinator and manager
     */
class RootApp : public cppcms::application
{
public:
	RootApp (cppcms::service & srv);

	virtual void welcome ();
	virtual void version ();

	static void setOkRaw (cppcms::http::response & response, std::string & data,
			      const std::string content_type = std::string ("text/plain"));
	static void setOk (cppcms::http::response & response, cppcms::json::value & data,
			   const std::string content_type = std::string ("application/json"));
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

	static void setCORSHeaders (cppcms::http::response & response, const std::string allowedMethods);

	static cppcms::json::value parsePostDataAsJson (cppcms::http::request & request);

private:
	static void setHttpStatus (cppcms::http::response & response, const int status, const std::string & message,
				   const std::string & loca);
};

} // namespace kdbrest

#endif
