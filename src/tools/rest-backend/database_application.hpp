#ifndef ELEKTRA_REST_DATABASEAPP_HEADER_GUARD
#define ELEKTRA_REST_DATABASEAPP_HEADER_GUARD

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <string>

#include "model_entry.hpp"


/**
 * @brief This is the main namespace for all classes belonging
 * to the kdb rest service
 */
namespace kdbrest
{

// STATIC CONSTANTS
static const std::string PARAM_RAW = "raw";

static const std::string INDEX_ORGANIZATION = "organization";
static const std::string INDEX_APPLICATION = "application";
static const std::string INDEX_SCOPE = "scope";
static const std::string INDEX_SLUG = "slug";
static const std::string INDEX_TITLE = "title";
static const std::string INDEX_DESCRIPTION = "description";
static const std::string INDEX_TAGS = "tags";
static const std::string INDEX_CONFIGURATION = "configuration";
static const std::string INDEX_CONFIGURATION_VALUE = "value";
static const std::string INDEX_CONFIGURATION_FORMAT = "format";

static const std::string REGEX_ENTRY_PARTS = "([a-zA-Z0-9\\-\\.]+)";
static const std::string REGEX_ENTRY_TITLE = "[^\\n\\r]{3,}";
static const std::string REGEX_ENTRY_DESCRIPTION = "[\\s\\S]{3,}";
static const std::string REGEX_ENTRY_TAGS = "([a-z0-9\\-\\.]{3,20})";

/**
     * @brief Database Application, serves as endpoint for operations
     * that query or alter the entry database.
     */
class DatabaseApp : public cppcms::application
{

public:
	DatabaseApp (cppcms::service & srv);

	virtual void getAllEntries ();
	virtual void getEntriesByPrefix (std::string keyPart);
	virtual void getUniqueEntry (std::string key);

private:
	inline void handleGet (cppcms::http::request & request, cppcms::http::response & response, std::string keyPart = std::string ());
	inline void handleGetUnique (cppcms::http::request & request, cppcms::http::response & response, std::string key = std::string ());
	inline void handleInsert (cppcms::http::request & request, cppcms::http::response & response);
	inline void handleUpdate (cppcms::http::request & request, cppcms::http::response & response, std::string & key);
	inline void handleDelete (cppcms::http::request & request, cppcms::http::response & response, std::string & key);

	inline model::Entry buildAndValidateEntry (cppcms::http::request & request, cppcms::http::response & response,
						   std::string keyName = std::string ());

	inline void produceOutput (cppcms::http::request & request, cppcms::http::response & response, std::vector<model::Entry> & entries);
	inline void processFiltering (cppcms::http::request & request, std::vector<model::Entry> & entries);
	inline void processSorting (cppcms::http::request & request, std::vector<model::Entry> & entries);
	inline int getMaxrows (cppcms::http::request & request);
	inline int getOffset (cppcms::http::request & request);
};

} // namespace kdbrest

#endif
