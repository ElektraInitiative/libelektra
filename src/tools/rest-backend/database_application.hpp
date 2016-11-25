#ifndef ELEKTRA_REST_DATABASEAPP_HEADER_GUARD
#define ELEKTRA_REST_DATABASEAPP_HEADER_GUARD

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <map>
#include <string>

#include <model_entry.hpp>

/**
 * @brief This is the main namespace for all classes belonging
 * to the kdb rest service
 */
namespace kdbrest
{

// STATIC CONSTANTS

// parameter to retrieve a configuration snippet in a raw format
static const std::string PARAM_RAW = "raw";

// index for the organization in requests
static const std::string INDEX_ORGANIZATION = "organization";
// index for the application in requests
static const std::string INDEX_APPLICATION = "application";
// index for the scope in requests
static const std::string INDEX_SCOPE = "scope";
// index for the slug in requests
static const std::string INDEX_SLUG = "slug";
// index for the title in requests
static const std::string INDEX_TITLE = "title";
// index for the description in requests
static const std::string INDEX_DESCRIPTION = "description";
// index for the tags array in requests
static const std::string INDEX_TAGS = "tags";
// index for the configuration object in requests
static const std::string INDEX_CONFIGURATION = "configuration";
// index for the configuration value in requests
static const std::string INDEX_CONFIGURATION_VALUE = "value";
// index for the configuration format in requests
static const std::string INDEX_CONFIGURATION_FORMAT = "format";

// regex that is used for the parts of entries (i.e. organization, application, ...)
static const std::string REGEX_ENTRY_PARTS = "([a-zA-Z0-9\\-\\.]+)";
// regex that is used for the title of entries
static const std::string REGEX_ENTRY_TITLE = "[^\\n\\r]{3,}";
// regex that is used for the description of entries
static const std::string REGEX_ENTRY_DESCRIPTION = "[\\s\\S]{3,}";
// regex that is used for the tags of entries
static const std::string REGEX_ENTRY_TAGS = "([a-z0-9\\-\\.]{3,20})";

// map from sort type to function
static std::map<std::string, bool (*) (model::Entry &, model::Entry &)> SORT_ENTRY_MAP = {
	{ "key", &model::Entry::less_than_key },
	{ "organization", &model::Entry::less_than_organization },
	{ "application", &model::Entry::less_than_application },
	{ "scope", &model::Entry::less_than_scope },
	{ "slug", &model::Entry::less_than_slug },
	{ "title", &model::Entry::less_than_title },
	{ "author", &model::Entry::less_than_author },
	{ "created_at", &model::Entry::less_than_created_at }
};

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
