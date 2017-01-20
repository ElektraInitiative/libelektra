/**
 * @file
 *
 * @brief header for cppcms controller managing snippet entry resources
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_DATABASEAPP_HPP
#define ELEKTRA_REST_DATABASEAPP_HPP

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <map>
#include <string>

#include <config.hpp>
#include <model_entry.hpp>

/**
 * @brief main namespace for the REST service
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

// insert & update entry request input data
struct entry_input_data
{
	std::string organization;
	std::string application;
	std::string scope;
	std::string slug;
	std::string title;
	std::string description;
	cppcms::json::array tags;
	std::string conf_format;
	std::string conf_value;
};

/**
 * @brief serves endpoint for snippet database
 */
class DatabaseApp : public cppcms::application
{

public:
	DatabaseApp (cppcms::service & srv);

	virtual void getAllEntries ();
	virtual void getEntriesByPrefix (std::string keyPart);
	virtual void getUniqueEntry (std::string key);

private:
	void handleGet (cppcms::http::request & request, cppcms::http::response & response,
			const std::string keyPart = std::string ()) const;
	void handleGetUnique (cppcms::http::request & request, cppcms::http::response & response,
			      const std::string key = std::string ()) const;
	void handleInsert (cppcms::http::request & request, cppcms::http::response & response) const;
	void handleUpdate (cppcms::http::request & request, cppcms::http::response & response, const std::string & key) const;
	void handleDelete (cppcms::http::request & request, cppcms::http::response & response, const std::string & key) const;

	void retrieveEntryInputData (cppcms::http::response & response, const cppcms::json::value & requestData,
				     entry_input_data & input_data, const bool withKeyParts = false) const;
	void validateEntryInputData (cppcms::http::response & response, const entry_input_data & input_data,
				     const bool withKeyParts = false) const;
	model::Entry buildAndValidateEntry (cppcms::http::request & request, cppcms::http::response & response,
					    const std::string keyName = std::string ()) const;

	inline void processFiltering (cppcms::http::request & request, std::vector<model::Entry> & entries) const;
	inline void processSorting (cppcms::http::request & request, std::vector<model::Entry> & entries) const;
	inline int getMaxrows (cppcms::http::request & request) const;
	inline int getOffset (cppcms::http::request & request) const;

	void generateAndSendEntryList (cppcms::http::request & request, cppcms::http::response & response,
				       const std::vector<model::Entry> & entries) const;

	void copyEntryData (model::Entry & from, model::Entry & to) const;
};

} // namespace kdbrest

#endif
