#include <algorithm>
#include <iostream>
#include <regex>
#include <stdexcept>
#include <stdlib.h>
#include <vector>

#include <boost/algorithm/string.hpp>
#include <boost/tokenizer.hpp>

#include "authentication_application.hpp"
#include "database_application.hpp"
#include "jwt.hpp"
#include "model_configformat.hpp"
#include "model_entry.hpp"
#include "model_importedconfig.hpp"
#include "model_pluginformat.hpp"
#include "model_user.hpp"
#include "root_application.hpp"
#include "service.hpp"

namespace kdbrest
{

/**
 * @brief the constructor of the database endpoint application.
 * @param srv a service container
 */
DatabaseApp::DatabaseApp (cppcms::service & srv) : cppcms::application (srv)
{

	dispatcher ().assign ("/(([a-zA-Z0-9\\-\\.]+)(/([a-zA-Z0-9\\-\\.]+)){3})/{0,1}", &DatabaseApp::getUniqueEntry, this, 1);
	mapper ().assign ("entry", "/{1}");

	dispatcher ().assign ("/(([a-zA-Z0-9\\-\\.]+)(/([a-zA-Z0-9\\-\\.]+)){0,2})/{0,1}", &DatabaseApp::getEntriesByPrefix, this, 1);
	mapper ().assign ("search", "/{1}");

	dispatcher ().assign ("(/?)", &DatabaseApp::getAllEntries, this);
	mapper ().assign ("");
}

/**
 * @brief handler for the root resource of the endpoint which serves a list of
 *		  snippet entries.
 */
void DatabaseApp::getAllEntries ()
{
	RootApp::setCORSHeaders (response (), "GET,POST,OPTIONS");

	if (request ().request_method () == "GET")
	{
		this->handleGet (request (), response ());
		return;
	}
	else if (request ().request_method () == "POST")
	{
		this->handleInsert (request (), response ());
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
 * @brief handler for dynamic resources that limit search space of snippet entries.
 * @param keyPart a part of a possible entry key to reduce search space
 */
void DatabaseApp::getEntriesByPrefix (std::string keyPart)
{
	RootApp::setCORSHeaders (response (), "GET,OPTIONS");

	if (request ().request_method () == "GET")
	{
		this->handleGet (request (), response (), keyPart);
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
 * @brief handler for the unique entry resource which serves methods for single entries.
 * @param key the unique snippet key of an entry resource
 */
void DatabaseApp::getUniqueEntry (std::string key)
{
	RootApp::setCORSHeaders (response (), "GET,PUT,DELETE,OPTIONS");

	if (request ().request_method () == "GET")
	{
		this->handleGetUnique (request (), response (), key);
		return;
	}
	else if (request ().request_method () == "PUT")
	{
		this->handleUpdate (request (), response (), key);
		return;
	}
	else if (request ().request_method () == "DELETE")
	{
		this->handleDelete (request (), response (), key);
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
 * @brief handles the retrieval for a list of snippet entries
 * @param req a request
 * @param resp a response
 * @param keyPart a part of a possible entry key to reduce search space
 */
inline void DatabaseApp::handleGet (cppcms::http::request & req, cppcms::http::response & resp, std::string keyPart)
{
	// first get the complete entry list
	std::vector<kdbrest::model::Entry> entries = service::StorageEngine::instance ().getAllEntries ();

	// if we are searching a sub-tree, filter first
	if (!keyPart.empty ())
	{
		service::SearchEngine::instance ().filterConfigurationsByName (entries, keyPart);
	}

	// then execute search based on parameters
	this->processFiltering (req, entries);
	// and sort the list
	this->processSorting (req, entries);

	// finally create the output
	this->produceOutput (req, resp, entries);
}

/**
 * @brief handles a request to retrieve details for a snippet entry
 * @param req a request
 * @param resp a response
 * @param key the unique snippet key of the entry which shall be retrieved
 */
inline void DatabaseApp::handleGetUnique (cppcms::http::request & req, cppcms::http::response & resp, std::string key)
{
	try
	{
		model::Entry entry = service::StorageEngine::instance ().getEntry (key);

		const std::string raw = req.get (PARAM_RAW);
		if (!raw.empty ())
		{
			try
			{
				model::ConfigFormat configFormat = service::ConvertEngine::instance ().exportTo (raw, entry);
				RootApp::setOkRaw (resp, configFormat.getConfig (), "text/plain");
				return; // quit here
			}
			catch (kdbrest::exception::UnsupportedConfigurationFormatException & e)
			{
				RootApp::setBadRequest (resp, "Configuration format not supported.", "ENTRY_UNSUPPORTED_FORMAT");
				return; // quit early
			}
			catch (kdbrest::exception::ParseConfigurationException & e)
			{
				RootApp::setBadRequest (resp, "The snippet cannot be represented using the submitted format.",
							"ENTRY_UNABLE_TO_EXPORT_SNIPPET");
			}
		}

		cppcms::json::value data;

		data["data"]["key"]["full"] = entry.getPublicName ();
		data["data"]["key"]["organization"] = entry.getOrganization ();
		data["data"]["key"]["application"] = entry.getApplication ();
		data["data"]["key"]["scope"] = entry.getScope ();
		data["data"]["key"]["slug"] = entry.getSlug ();

		int i = 0;
		for (auto & elem : entry.getTags ())
		{
			data["data"]["meta"]["tags"][i] = elem;
			i++;
		}
		data["data"]["meta"]["title"] = entry.getTitle ();
		data["data"]["meta"]["description"] = entry.getDescription ();
		data["data"]["meta"]["author"] = entry.getAuthor ();
		data["data"]["meta"]["created_at"] = entry.getCreatedAt ();

		std::vector<model::ConfigFormat> formats = service::ConvertEngine::instance ().exportToAll (entry);
		int j = 0;
		for (auto & elem : formats)
		{
			data["data"]["value"][j]["format"] = elem.getPluginformat ().getFileformat ();
			data["data"]["value"][j]["plugin"] = elem.getPluginformat ().getPluginname ();
			data["data"]["value"][j]["value"] = elem.getConfig ();
			j++;
		}

		RootApp::setOk (resp, data, "application/json");
	}
	catch (exception::EntryNotFoundException & e)
	{
		RootApp::setNotFound (resp, "The requested entry does not exist.", "ENTRY_DOES_NOT_EXIST");
		return;
	}
}

/**
 * @brief handles a creation request for a new snippet entry.
 * @param req a request
 * @param resp a response
 */
inline void DatabaseApp::handleInsert (cppcms::http::request & req, cppcms::http::response & resp)
{
	// first check if the currently authenticated user may create new database entries
	if (!AuthenticationApp::validateAuthentication (req, resp, ELEKTRA_REST_PERMISSIONS_CREATE_ENTRY))
	{
		return; // quit early, error message set already
	}

	model::User currentUser ("");
	try
	{
		currentUser = AuthenticationApp::getCurrentUser (req);
	}
	catch (exception::ElektraRestException & e)
	{
		RootApp::setUnauthorized (resp, "You need to be authenticated to perform this action.", "NEED_AUTHENTICATION");
		return; // quit early
	}

	model::Entry entry ("");
	try
	{
		entry = this->buildAndValidateEntry (req, resp);
	}
	catch (exception::EntryValidationException & e)
	{
		// error already written to output
		return; // so quit early
	}

	if (service::StorageEngine::instance ().entryExists (entry.getPublicName ()))
	{
		RootApp::setBadRequest (resp, "An entry with the given details does already exist.", "ENTRY_DOES_ALREADY_EXIST");
		return; // quit early
	}

	try
	{
		service::StorageEngine::instance ().createEntry (entry);
	}
	catch (exception::EntryAlreadyExistsException & e)
	{
		RootApp::setBadRequest (resp, "An entry with the given details does already exist.", "ENTRY_DOES_ALREADY_EXIST");
		return; // quit early
	}

	RootApp::setOk (resp, "The entry has been created successfully.", "ENTRY_CREATED_SUCCESSFULLY");
}

/**
 * @brief handles an update request for a snippet entry.
 * @param req a request
 * @param resp a response
 * @param key the unique snippet key of the entry which shall be updated
 */
inline void DatabaseApp::handleUpdate (cppcms::http::request & req, cppcms::http::response & resp, std::string & key)
{
	// first lets find the entry
	model::Entry oldEntry ("");
	try
	{
		oldEntry = service::StorageEngine::instance ().getEntry (key);
	}
	catch (exception::EntryNotFoundException & e)
	{
		RootApp::setNotFound (resp, "The entry to update does not exist.", "ENTRY_DOES_NOT_EXIST");
		return; // quit early
	}

	// now check if the currently authenticated user may even update it
	if (!AuthenticationApp::validateAuthentication (req, resp, ELEKTRA_REST_PERMISSIONS_UPDATE_ENTRY, oldEntry.getAuthor ()))
	{
		return; // quit early, error response already set
	}

	model::Entry newEntry ("");
	try
	{
		newEntry = this->buildAndValidateEntry (req, resp, key);
	}
	catch (exception::EntryValidationException & e)
	{
		// error already written to output
		return; // so quit early
	}

	// can't change base key, so scope, app, etc. remain
	// only change description, title, config
	oldEntry.setDescription (newEntry.getDescription ()); // set new description
	oldEntry.setTitle (newEntry.getTitle ());	     // set new title
	oldEntry.setTags (newEntry.getTags ());		      // set new tags
	oldEntry.getSubkeys ().clear ();		      // clear old config
	oldEntry.addSubkeys (newEntry.getSubkeys ());	 // replace config

	try
	{
		service::StorageEngine::instance ().updateEntry (oldEntry);
	}
	catch (exception::EntryNotFoundException & e)
	{
		RootApp::setBadRequest (resp, "An entry with the given details does not exist.", "ENTRY_DOES_NOT_EXIST");
		return; // quit early
	}

	RootApp::setOk (resp, "The entry has been updated successfully.", "ENTRY_UPDATED_SUCCESSFULLY");
}

/**
 * @brief handles a delete request of a snippet entry.
 * @param req a request
 * @param resp a response
 * @param key the unique snippet key of the entry which shall be deleted
 */
inline void DatabaseApp::handleDelete (cppcms::http::request & req, cppcms::http::response & resp, std::string & key)
{
	// first find the entry to delete
	model::Entry entry ("");
	try
	{
		entry = service::StorageEngine::instance ().getEntry (key);
	}
	catch (exception::EntryNotFoundException & e)
	{
		RootApp::setNotFound (resp, "The entry to delete does not exist.", "ENTRY_DOES_NOT_EXIST");
		return; // quit early
	}

	// now check if the currently authenticated user may even update it
	if (!AuthenticationApp::validateAuthentication (req, resp, ELEKTRA_REST_PERMISSIONS_DELETE_ENTRY, entry.getAuthor ()))
	{
		return; // quit early, error response already set
	}

	try
	{
		service::StorageEngine::instance ().deleteEntry (entry);
	}
	catch (exception::EntryNotFoundException & e)
	{
		RootApp::setBadRequest (resp, "An entry with the given details does not exist.", "ENTRY_DOES_NOT_EXIST");
		return; // quit early
	}

	RootApp::setOk (resp, "The entry has been deleted successfully.", "ENTRY_DELETED_SUCCESSFULLY");
}

/**
 * @brief a helper function that builds and validates a snippet entry from a request.
 * @note the method sets response messages itself in case an error occurs. if that happens,
 *		 an exception is thrown so that the calling method can quit too.
 * @param req a request
 * @param resp a response
 * @param keyName in case an entry is built for an update, the keyname has to be provided
 * @return the built and validated entry
 */
inline model::Entry DatabaseApp::buildAndValidateEntry (cppcms::http::request & req, cppcms::http::response & resp, std::string keyName)
{
	// check if request data is of type application/json
	if (req.content_type_parsed ().media_type () != "application/json")
	{
		RootApp::setNotAcceptable (resp, "You have supplied an unsupported Content-Type.", "REQUEST_UNSUPPORTED_CONTENT_TYPE");
		throw exception::EntryValidationException (); // quit early
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
		throw exception::EntryValidationException (); // quit early
	}

	// check some special fields before retrieving them
	if (requestData.type ("configuration") != cppcms::json::is_object ||
	    (requestData.type ("tags") != cppcms::json::is_undefined && requestData.type ("tags") != cppcms::json::is_array))
	{
		RootApp::setBadRequest (resp, "The submitted data could not be parsed.", "REQUEST_MALFORMED_DATA");
		throw exception::EntryValidationException (); // quit early
	}

	// required request fields
	std::string organization;
	std::string app;
	std::string scope;
	std::string slug;
	std::string title;
	std::string description;
	cppcms::json::array tags;
	std::string conf_format;
	std::string conf_value;

	// regex for formats
	std::regex regex_parts (REGEX_ENTRY_PARTS);
	std::regex regex_title (REGEX_ENTRY_TITLE);
	std::regex regex_description (REGEX_ENTRY_DESCRIPTION);
	std::regex regex_tags (REGEX_ENTRY_TAGS);

	// retrieve submitted data
	if (keyName.empty ())
	{
		try
		{
			organization = requestData.get<std::string> ("organization");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (resp, "You have to supply an organization.", "ENTRY_MISSING_ORGANIZATION");
			throw exception::EntryValidationException (); // quit early
		}

		try
		{
			app = requestData.get<std::string> ("application");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (resp, "You have to supply an application.", "ENTRY_MISSING_APPLICATION");
			throw exception::EntryValidationException (); // quit early
		}

		try
		{
			scope = requestData.get<std::string> ("scope");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (resp, "You have to supply a scope.", "ENTRY_MISSING_SCOPE");
			throw exception::EntryValidationException (); // quit early
		}

		try
		{
			slug = requestData.get<std::string> ("slug");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (resp, "You have to supply a slug.", "ENTRY_MISSING_SLUG");
			throw exception::EntryValidationException (); // quit early
		}
	}

	try
	{
		title = requestData.get<std::string> ("title");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a title.", "ENTRY_MISSING_TITLE");
		throw exception::EntryValidationException (); // quit early
	}

	try
	{
		description = requestData.get<std::string> ("description");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a description.", "ENTRY_MISSING_DESCRIPTION");
		throw exception::EntryValidationException (); // quit early
	}

	try
	{
		conf_format = requestData.get<std::string> ("configuration.format");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a configuration format.", "ENTRY_MISSING_CONFIGURATION_FORMAT");
		throw exception::EntryValidationException (); // quit early
	}

	try
	{
		conf_value = requestData.get<std::string> ("configuration.value");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a configuration snippet.", "ENTRY_MISSING_CONFIGURATION_VALUE");
		throw exception::EntryValidationException (); // quit early
	}

	if (requestData.type ("tags") != cppcms::json::is_undefined)
	{
		tags = requestData["tags"].array ();
	}

	// validate inputs
	if (keyName.empty ())
	{
		if (!std::regex_match (organization, regex_parts))
		{
			RootApp::setBadRequest (resp, "The submitted organization has an invalid format.", "ENTRY_INVALID_ORGANIZATION");
			throw exception::EntryValidationException (); // quit early
		}
		if (!std::regex_match (app, regex_parts))
		{
			RootApp::setBadRequest (resp, "The submitted application has an invalid format.", "ENTRY_INVALID_APPLICATION");
			throw exception::EntryValidationException (); // quit early
		}
		if (!std::regex_match (scope, regex_parts))
		{
			RootApp::setBadRequest (resp, "The submitted scope has an invalid format.", "ENTRY_INVALID_SCOPE");
			throw exception::EntryValidationException (); // quit early
		}
		if (!std::regex_match (slug, regex_parts))
		{
			RootApp::setBadRequest (resp, "The submitted slug has an invalid format.", "ENTRY_INVALID_SLUG");
			throw exception::EntryValidationException (); // quit early
		}
	}

	if (!std::regex_match (title, regex_title))
	{
		RootApp::setBadRequest (resp,
					"The submitted title has an invalid format. It has to be at least 3 signs long and may not contain "
					"line breakings.",
					"ENTRY_INVALID_TITLE");
		throw exception::EntryValidationException (); // quit early
	}
	if (!std::regex_match (description, regex_description))
	{
		RootApp::setBadRequest (resp, "The submitted description has an invalid format. It has to be at least 3 signs long.",
					"ENTRY_INVALID_DESCRIPTION");
		throw exception::EntryValidationException (); // quit early
	}
	for (auto & tag : tags)
	{
		if (tag.type () != cppcms::json::is_string || !std::regex_match (tag.str (), regex_tags))
		{
			RootApp::setBadRequest (resp,
						"You have supplied a malformed tag. Tags may only consist of lower case "
						"letters, numbers, dots (.) and dashes (-).",
						"ENTRY_INVALID_TAG");
			throw exception::EntryValidationException (); // quit early
		}
	}

	std::vector<std::string> taglist;
	for (auto & tag : tags)
	{
		taglist.push_back (tag.str ());
	}

	model::User currentUser ("");
	try
	{
		currentUser = AuthenticationApp::getCurrentUser (req);
	}
	catch (exception::ElektraRestException & e)
	{
		RootApp::setUnauthorized (resp, "You need to be authenticated to perform this action.", "NEED_AUTHENTICATION");
		throw exception::EntryValidationException (); // quit early
	}

	std::string entryKey = organization + "/" + app + "/" + scope + "/" + slug;
	if (!keyName.empty ())
	{
		entryKey = keyName;
	}
	model::Entry entry (entryKey);
	entry.setTitle (title);
	entry.setDescription (description);
	entry.setCreatedAt (static_cast<long> (time (0)));
	entry.setAuthor (currentUser.getUsername ());
	entry.setTags (taglist);

	try
	{
		model::ImportedConfig cfg = service::ConvertEngine::instance ().import (conf_value, conf_format, entry);
		entry.addSubkeys (cfg.getKeySet ());
	}
	catch (exception::UnsupportedConfigurationFormatException & e)
	{
		RootApp::setBadRequest (resp, "The given configuration has an invalid or unsupported format.",
					"ENTRY_INVALID_CONFIGURATION_FORMAT");
		throw exception::EntryValidationException (); // quit early
	}
	catch (exception::ParseConfigurationException & e)
	{
		RootApp::setBadRequest (resp, "The given configuration could not be parsed within the given format.",
					"ENTRY_INVALID_CONFIGURATION_VALUE");
		throw exception::EntryValidationException ();
	}

	return entry;
}


/**
 * @brief extracts the max number of rows to print from a request.
 * @param req a request
 * @return the max number of rows to print or the default value if not set
 */
inline int DatabaseApp::getMaxrows (cppcms::http::request & req)
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
inline int DatabaseApp::getOffset (cppcms::http::request & req)
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
 * @brief filters a vector of snippet entries based on parameters of a request.
 * @param req a request
 * @param entries the entry vector to filter
 */
inline void DatabaseApp::processFiltering (cppcms::http::request & req, std::vector<model::Entry> & entries)
{
	// retrieve parameter values
	std::string filter = req.get (PARAM_FILTER);
	std::string filterby = req.get (PARAM_FILTERBY);

	// only proceed if a filter is set
	if (!filter.empty ())
	{
		// if the filter is unknown, take default
		if (filterby != "all" && filterby != "key" && filterby != "title" && filterby != "description" && filterby != "author" &&
		    filterby != "tags")
		{
			filterby = std::string (ELEKTRA_REST_OUTPUT_FILTERBY_ENTRY_DEFAULT);
		}

		service::SearchEngine::instance ().findConfigurationsByFilter (entries, filter, filterby);
	}
}

/**
 * @brief sorts a vector of snippet entries based on parameters of a request.
 * @param req a request
 * @param entries the entry vector to sort
 */
inline void DatabaseApp::processSorting (cppcms::http::request & req, std::vector<model::Entry> & entries)
{
	// retrieve parameter values
	std::string sort = req.get (PARAM_SORT);
	std::string sortby = req.get (PARAM_SORTBY);

	// validate the sort direction input or set default
	if (!boost::iequals (sort, "asc") && !boost::iequals (sort, "desc"))
	{
		sort = std::string (ELEKTRA_REST_OUTPUT_SORT_ENTRY_DEFAULT);
	}

	// validate the sortby input or set default
	std::vector<std::string> sortOptions = { "key", "title", "created_at", "author", "organization", "application", "scope", "slug" };
	if (std::find (sortOptions.begin (), sortOptions.end (), sortby) == sortOptions.end ())
	{
		sortby = std::string (ELEKTRA_REST_OUTPUT_SORTBY_ENTRY_DEFAULT);
	}

	// do the sorting
	if (boost::iequals (sort, "asc"))
	{
		if (boost::iequals (sortby, "title"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::less_than_title);
		}
		else if (boost::iequals (sortby, "created_at"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::less_than_created_at);
		}
		else if (boost::iequals (sortby, "author"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::less_than_author);
		}
		else if (boost::iequals (sortby, "organization"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::less_than_organization);
		}
		else if (boost::iequals (sortby, "application"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::less_than_application);
		}
		else if (boost::iequals (sortby, "scope"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::less_than_scope);
		}
		else if (boost::iequals (sortby, "slug"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::less_than_slug);
		}
		else // last option "key"
		{
			std::sort (entries.begin (), entries.end (), model::Entry::less_than_key);
		}
	}
	else
	{
		if (boost::iequals (sortby, "title"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::greater_than_title);
		}
		else if (boost::iequals (sortby, "created_at"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::greater_than_created_at);
		}
		else if (boost::iequals (sortby, "author"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::greater_than_author);
		}
		else if (boost::iequals (sortby, "organization"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::greater_than_organization);
		}
		else if (boost::iequals (sortby, "application"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::greater_than_application);
		}
		else if (boost::iequals (sortby, "scope"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::greater_than_scope);
		}
		else if (boost::iequals (sortby, "slug"))
		{
			std::sort (entries.begin (), entries.end (), model::Entry::greater_than_slug);
		}
		else // last option "key"
		{
			std::sort (entries.begin (), entries.end (), model::Entry::greater_than_key);
		}
	}
}

/**
 * @brief creates the output for a list of snippets with all helping attributes
 *	      like number of entries, offset, etc.
 * @note not all snippets in the list may actually be printed to the output.
 *       which snippets are used in the output depends on the offset and maxrows.
 * @param req a request
 * @param resp a response
 * @param entries a list of snippets, of which some may be used for the output
 */
inline void DatabaseApp::produceOutput (cppcms::http::request & req, cppcms::http::response & resp, std::vector<model::Entry> & entries)
{
	int offset = this->getOffset (req);
	int maxrows = this->getMaxrows (req);

	cppcms::json::value data;
	int elements = static_cast<int> (entries.size ()) - offset;
	elements = (elements > 0) ? elements : 0;
	elements = (elements < maxrows) ? elements : maxrows;
	int remaining = (static_cast<int> (entries.size ()) - offset) - elements;
	data["offset"] = offset;
	data["elements"] = elements;
	data["remaining"] = (remaining > 0) ? remaining : 0;

	if (offset < static_cast<int> (entries.size ()))
	{
		int index = 0;
		int offset_it = 0;
		for (auto & elem : entries)
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

			data["entries"][index]["key"] = elem.getPublicName ();
			data["entries"][index]["title"] = elem.getTitle ();
			data["entries"][index]["description"] = elem.getDescription ();
			data["entries"][index]["created_at"] = elem.getCreatedAt ();
			data["entries"][index]["author"] = elem.getAuthor ();
			index++;
		}
	}

	RootApp::setOk (resp, data, "application/json");
}

} // namespace kdbrest
