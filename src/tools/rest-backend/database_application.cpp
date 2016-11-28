/**
 * @file
 *
 * @brief cppcms controller implementation managing snippet entry resources
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <algorithm>
#include <iostream>
#include <regex>
#include <stdexcept>
#include <stdlib.h>
#include <vector>

#include <boost/algorithm/string.hpp>
#include <boost/tokenizer.hpp>

#include <authentication_application.hpp>
#include <database_application.hpp>
#include <model_configformat.hpp>
#include <model_entry.hpp>
#include <model_importedconfig.hpp>
#include <model_pluginformat.hpp>
#include <model_user.hpp>
#include <root_application.hpp>
#include <service.hpp>

namespace kdbrest
{

/**
 * @brief the constructor of the database endpoint application
 * 
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
 * @brief root resource handler
 * 
 * the function delegates work to different helper methods
 * based on the HTTP method in the current request.
 * 
 * for a GET request, it serves a list of entries.
 * for a POST request, it attempts an insert.
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
 * @brief search resource handler
 * 
 * the function delegates work to different helper methods
 * based on the HTTP method in the current request.
 * 
 * for a GET request, it serves a list of entries, while
 * taking an additional GET parameter into account to limit
 * the resulting entry list.
 * 
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
 * @brief unique entry resource handler
 * 
 * the function delegates work to different helper methods
 * based on the HTTP method in the current request.
 * it operates on a well-specified unique resource.
 * 
 * for a GET request, it serves detailed information of an entry.
 * for a PUT request, it attempts an update on an entry.
 * for a DELETE request, it attempts the deletion of an entry.
 * 
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
 * @brief handles the retrieval of a list of snippet entries
 * 
 * @param req a request
 * @param resp a response
 * @param keyPart a part of a possible entry key to reduce search space
 */
void DatabaseApp::handleGet (cppcms::http::request & req, cppcms::http::response & resp, const std::string keyPart) const
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
	this->generateAndSendEntryList (req, resp, entries);
}

/**
 * @brief handles a request to retrieve details for a single snippet entry
 * 
 * @param req a request
 * @param resp a response
 * @param key the unique snippet key of the entry which shall be retrieved
 */
void DatabaseApp::handleGetUnique (cppcms::http::request & req, cppcms::http::response & resp, const std::string key) const
{
	try
	{
		model::Entry entry = service::StorageEngine::instance ().getEntry (key);
		this->addViewToEntry (entry);

		const std::string raw = req.get (PARAM_RAW);
		if (!raw.empty ())
		{
			try
			{
				model::ConfigFormat configFormat = service::ConvertEngine::instance ().exportTo (raw, entry);
				RootApp::setOkRaw (resp, configFormat.getConfig (), MIME_TEXT_PLAIN);
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

		// create output data & add meta information
		cppcms::json::value data;
		data["key"]["full"] = entry.getPublicName ();
		data["key"]["organization"] = entry.getOrganization ();
		data["key"]["application"] = entry.getApplication ();
		data["key"]["scope"] = entry.getScope ();
		data["key"]["slug"] = entry.getSlug ();
		int i = 0;
		for (auto elem : entry.getTags ())
		{
			data["meta"]["tags"][i++] = elem;
		}
		data["meta"]["title"] = entry.getTitle ();
		data["meta"]["description"] = entry.getDescription ();
		data["meta"]["author"] = entry.getAuthor ();
		data["meta"]["created_at"] = entry.getCreatedAt ();

		// get the configuration in all supported formats
		std::vector<model::ConfigFormat> formats = service::ConvertEngine::instance ().exportToAll (entry);

		// first output the config of the plugin that was used during import
		int j = 0;
		auto it = formats.begin ();
		while (it != formats.end ())
		{
			if (it->getPluginformat ().getPluginname () == entry.getUploadPlugin ())
			{
				data["value"][j]["format"] = it->getPluginformat ().getFileformat ();
				data["value"][j]["plugin"] = it->getPluginformat ().getPluginname ();
				data["value"][j]["value"] = it->getConfig ();
				data["value"][j]["validated"] = it->isValidated ();
				j++;
				formats.erase (it);
				break;
			}
			it++;
		}
		// then output the other formats
		for (auto & elem : formats)
		{
			data["value"][j]["format"] = elem.getPluginformat ().getFileformat ();
			data["value"][j]["plugin"] = elem.getPluginformat ().getPluginname ();
			data["value"][j]["value"] = elem.getConfig ();
			data["value"][j]["validated"] = elem.isValidated ();
			j++;
		}

		RootApp::setOk (resp, data, MIME_APPLICATION_JSON);
	}
	catch (exception::EntryNotFoundException & e)
	{
		RootApp::setNotFound (resp, "The requested entry does not exist.", "ENTRY_DOES_NOT_EXIST");
		return;
	}
}

/**
 * @brief handles a creation request for a new snippet entry
 * 
 * @param req a request
 * @param resp a response
 */
void DatabaseApp::handleInsert (cppcms::http::request & req, cppcms::http::response & resp) const
{
	// first check if the currently authenticated user may create new database entries
	if (!AuthenticationApp::validateAuthentication (req, resp, Config::instance ().getConfig ().get<int> ("permissions.entry.create")))
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
 * @brief handles an update request for a snippet entry
 *
 * @param req a request
 * @param resp a response
 * @param key the unique snippet key of the entry which shall be updated
 */
void DatabaseApp::handleUpdate (cppcms::http::request & req, cppcms::http::response & resp, const std::string & key) const
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
	if (!AuthenticationApp::validateAuthentication (req, resp, Config::instance ().getConfig ().get<int> ("permissions.entry.edit"),
							oldEntry.getAuthor ()))
	{
		return; // quit early, error response already set
	}

	// build a new entry from the input
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

	// update the entry data with the new input
	this->copyEntryData (newEntry, oldEntry);

	// store updated entry
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
 * @brief handles a delete request of a snippet entry
 *
 * @param req a request
 * @param resp a response
 * @param key the unique snippet key of the entry which shall be deleted
 */
void DatabaseApp::handleDelete (cppcms::http::request & req, cppcms::http::response & resp, const std::string & key) const
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
	if (!AuthenticationApp::validateAuthentication (req, resp, Config::instance ().getConfig ().get<int> ("permissions.entry.delete"),
							entry.getAuthor ()))
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
 * @brief attempts to parse input data from a request data pack
 * 
 * @param response a response for error output
 * @param requestData a json value containing all request post data
 * @param input_data the target struct to place input data in
 * @param withKeyParts if key part fields should be fetches as well
 */
void DatabaseApp::retrieveEntryInputData (cppcms::http::response & resp, const cppcms::json::value & requestData, entry_input_data & data,
					  const bool withKeyParts) const
{
	// check some special fields before retrieving them
	if (requestData.type ("configuration") != cppcms::json::is_object ||
	    (requestData.type ("tags") != cppcms::json::is_undefined && requestData.type ("tags") != cppcms::json::is_array))
	{
		RootApp::setBadRequest (resp, "The submitted data could not be parsed.", "REQUEST_MALFORMED_DATA");
		throw exception::InvalidPostDataFormatException (); // quit early
	}

	// retrieve submitted data
	if (withKeyParts)
	{
		try
		{
			data.organization = requestData.get<std::string> ("organization");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (resp, "You have to supply an organization.", "ENTRY_MISSING_ORGANIZATION");
			throw exception::EntryValidationException (); // quit early
		}

		try
		{
			data.application = requestData.get<std::string> ("application");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (resp, "You have to supply an application.", "ENTRY_MISSING_APPLICATION");
			throw exception::EntryValidationException (); // quit early
		}

		try
		{
			data.scope = requestData.get<std::string> ("scope");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (resp, "You have to supply a scope.", "ENTRY_MISSING_SCOPE");
			throw exception::EntryValidationException (); // quit early
		}

		try
		{
			data.slug = requestData.get<std::string> ("slug");
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (resp, "You have to supply a slug.", "ENTRY_MISSING_SLUG");
			throw exception::EntryValidationException (); // quit early
		}
	}

	try
	{
		data.title = requestData.get<std::string> ("title");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a title.", "ENTRY_MISSING_TITLE");
		throw exception::EntryValidationException (); // quit early
	}

	try
	{
		data.description = requestData.get<std::string> ("description");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a description.", "ENTRY_MISSING_DESCRIPTION");
		throw exception::EntryValidationException (); // quit early
	}

	try
	{
		data.conf_format = requestData.get<std::string> ("configuration.format");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a configuration format.", "ENTRY_MISSING_CONFIGURATION_FORMAT");
		throw exception::EntryValidationException (); // quit early
	}

	try
	{
		data.conf_value = requestData.get<std::string> ("configuration.value");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		RootApp::setBadRequest (resp, "You have to supply a configuration snippet.", "ENTRY_MISSING_CONFIGURATION_VALUE");
		throw exception::EntryValidationException (); // quit early
	}

	if (requestData.type ("tags") != cppcms::json::is_undefined)
	{
		data.tags = requestData["tags"].array ();
	}
}

/**
 * @brief validates input data for a snippet entry
 *
 * @param response a response to use for error output
 * @param input_data the input data to validate
 * @param withKeyParts whether to validate also key parts
 */
void DatabaseApp::validateEntryInputData (cppcms::http::response & resp, const entry_input_data & data, const bool withKeyParts) const
{
	// regex for formats
	std::regex regex_parts (REGEX_ENTRY_PARTS);
	std::regex regex_title (REGEX_ENTRY_TITLE);
	std::regex regex_description (REGEX_ENTRY_DESCRIPTION);
	std::regex regex_tags (REGEX_ENTRY_TAGS);

	// validate inputs
	if (withKeyParts)
	{
		if (!std::regex_match (data.organization, regex_parts))
		{
			RootApp::setBadRequest (resp, "The submitted organization has an invalid format.", "ENTRY_INVALID_ORGANIZATION");
			throw exception::EntryValidationException (); // quit early
		}
		if (!std::regex_match (data.application, regex_parts))
		{
			RootApp::setBadRequest (resp, "The submitted application has an invalid format.", "ENTRY_INVALID_APPLICATION");
			throw exception::EntryValidationException (); // quit early
		}
		if (!std::regex_match (data.scope, regex_parts))
		{
			RootApp::setBadRequest (resp, "The submitted scope has an invalid format.", "ENTRY_INVALID_SCOPE");
			throw exception::EntryValidationException (); // quit early
		}
		if (!std::regex_match (data.slug, regex_parts))
		{
			RootApp::setBadRequest (resp, "The submitted slug has an invalid format.", "ENTRY_INVALID_SLUG");
			throw exception::EntryValidationException (); // quit early
		}
	}

	if (!std::regex_match (data.title, regex_title))
	{
		RootApp::setBadRequest (resp,
					"The submitted title has an invalid format. It has to be at least 3 signs long and may not contain "
					"line breakings.",
					"ENTRY_INVALID_TITLE");
		throw exception::EntryValidationException (); // quit early
	}
	if (!std::regex_match (data.description, regex_description))
	{
		RootApp::setBadRequest (resp, "The submitted description has an invalid format. It has to be at least 3 signs long.",
					"ENTRY_INVALID_DESCRIPTION");
		throw exception::EntryValidationException (); // quit early
	}
	for (auto & tag : data.tags)
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
}

/**
 * @brief builds and validates a snippet entry from a request
 *
 * @note the method sets response messages itself in case an error occurs.
 *		 if that happens, an exception is thrown so that the calling method
 *		 has a chance to quit early too.
 *
 * @param req a request
 * @param resp a response
 * @param keyName in case an entry is built for an update, the keyname has to be provided
 * @return the built and validated entry
 */
model::Entry DatabaseApp::buildAndValidateEntry (cppcms::http::request & req, cppcms::http::response & resp,
						 const std::string keyName) const
{
	// check if request data is of type application/json
	if (req.content_type_parsed ().media_type () != MIME_APPLICATION_JSON)
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

	// required request fields
	entry_input_data input_data;
	try
	{
		this->retrieveEntryInputData (resp, requestData, input_data, keyName.empty ());
	}
	catch (kdbrest::exception::InvalidPostDataFormatException & e)
	{
		throw exception::EntryValidationException (); // error is set already
	}
	catch (kdbrest::exception::EntryValidationException & e)
	{
		throw; // error is set already
	}

	// validate the input data
	try
	{
		this->validateEntryInputData (resp, input_data, keyName.empty ());
	}
	catch (kdbrest::exception::EntryValidationException & e)
	{
		throw; // error is set already
	}

	// convert tags to vector<string>
	std::vector<std::string> taglist;
	for (auto & tag : input_data.tags)
	{
		taglist.push_back (tag.str ()); // at this point we know that tag is a string
	}

	// retrieve current user for author information
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

	// build the entry
	std::string entryKey = input_data.organization + "/" + input_data.application + "/" + input_data.scope + "/" + input_data.slug;
	if (!keyName.empty ())
	{
		entryKey = keyName;
	}
	model::Entry entry (entryKey);
	entry.setTitle (input_data.title);
	entry.setDescription (input_data.description);
	entry.setCreatedAt (static_cast<long> (time (0)));
	entry.setAuthor (currentUser.getUsername ());
	entry.setTags (taglist);

	// try to import the submitted snippet into the internal format
	try
	{
		model::ImportedConfig cfg =
			service::ConvertEngine::instance ().import (input_data.conf_value, input_data.conf_format, entry);
		auto subkeys = cfg.getKeySet ();
		entry.addSubkeys (subkeys.begin (), subkeys.end ());
		entry.setUploadPlugin (cfg.getPluginformat ().getPluginname ());
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

	// and finally return the built entry object
	return entry;
}


/**
 * @brief extracts the max number of rows to print from a request
 * 
 * @param req a request
 * @return the max number of rows to print or the default value if not set
 */
inline int DatabaseApp::getMaxrows (cppcms::http::request & req) const
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
 * @brief extracts the offset parameter from a request
 * 
 * @param req a request
 * @return the offset extracted from the request parameter, if not set 0
 */
inline int DatabaseApp::getOffset (cppcms::http::request & req) const
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
 * @brief filters a vector of snippet entries based on parameters of a request
 * 
 * @param req a request
 * @param entries the entry vector to filter
 */
inline void DatabaseApp::processFiltering (cppcms::http::request & req, std::vector<model::Entry> & entries) const
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
			filterby = Config::instance ().getConfig ().get<std::string> ("output.default.entry.filterby");
		}

		service::SearchEngine::instance ().findConfigurationsByFilter (entries, filter, filterby);
	}
}

/**
 * @brief sorts a vector of snippet entries based on parameters of a request
 * 
 * @param req a request
 * @param entries the entry vector to sort
 */
inline void DatabaseApp::processSorting (cppcms::http::request & req, std::vector<model::Entry> & entries) const
{
	// retrieve parameter values
	std::string sort = req.get (PARAM_SORT);
	std::string sortby = req.get (PARAM_SORTBY);

	// validate the sort direction input or set default
	if (!boost::iequals (sort, PARAM_VAL_SORT_ASC) && !boost::iequals (sort, PARAM_VAL_SORT_DESC))
	{
		sort = Config::instance ().getConfig ().get<std::string> ("output.default.entry.sort");
		;
	}

	// validate the sortby input or set default
	if (SORT_ENTRY_MAP.find (sortby) == SORT_ENTRY_MAP.end ())
	{
		sortby = Config::instance ().getConfig ().get<std::string> ("output.default.entry.sortby");
		;
	}

	// do the sorting
	if (boost::iequals (sort, PARAM_VAL_SORT_ASC))
	{
		// sort from left to right
		std::sort (entries.begin (), entries.end (), SORT_ENTRY_MAP.at (sortby));
	}
	else
	{
		// sort from right to left
		std::sort (entries.rbegin (), entries.rend (), SORT_ENTRY_MAP.at (sortby));
	}
}

/**
 * @brief creates the output for a list of snippet entries
 * 
 * @note not all snippets in the list may actually be printed to the output.
 *       which snippets are used in the output depends on additional parameters.
 * 
 * @param req a request
 * @param resp a response
 * @param entries a list of snippets, of which some may be used for the output
 */
void DatabaseApp::generateAndSendEntryList (cppcms::http::request & req, cppcms::http::response & resp,
					    const std::vector<model::Entry> & entries) const
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

			data["entries"][index]["key"]["full"] = elem.getPublicName ();
			data["entries"][index]["key"]["organization"] = elem.getOrganization ();
			data["entries"][index]["key"]["application"] = elem.getApplication ();
			data["entries"][index]["key"]["scope"] = elem.getScope ();
			data["entries"][index]["key"]["slug"] = elem.getSlug ();
			data["entries"][index]["title"] = elem.getTitle ();
			data["entries"][index]["description"] = elem.getDescription ();
			data["entries"][index]["created_at"] = elem.getCreatedAt ();
			data["entries"][index]["author"] = elem.getAuthor ();

			int i = 0;
			for (auto & tag : elem.getTags ())
			{
				data["entries"][index]["tags"][i] = tag;
				i++;
			}

			index++;
		}
	}

	RootApp::setOk (resp, data, MIME_APPLICATION_JSON);
}

/**
 * @brief increases the view count of an entry by 1
 *
 * @param entry the entry to change
 */
void DatabaseApp::addViewToEntry (model::Entry & entry) const
{
	entry.addViews (1);
	try
	{
		(void)service::StorageEngine::instance ().updateEntry (entry);
	}
	catch (kdbrest::exception::EntryNotFoundException & e)
	{
		// do not handle exception
	}
}

/**
 * @brief copies meta data from one entry to another
 *
 * does not copy the key of the entry itself, only meta data
 * and the configuration snippet. does also not copy data
 * that can only be changed by the system (creation date,
 * author and view count).
 *
 * @param from source entry
 * @param to target entry
 */
void DatabaseApp::copyEntryData (model::Entry & from, model::Entry & to) const
{
	to.setDescription (from.getDescription ()); // set new description
	to.setTitle (from.getTitle ());		    // set new title
	to.setTags (from.getTags ());		    // set new tags
	to.getSubkeys ().clear ();		    // clear old config
	auto subkeys = from.getSubkeys ();
	to.addSubkeys (subkeys.begin (), subkeys.end ()); // replace config
	to.setUploadPlugin (from.getUploadPlugin ());     // replace old upload format
}

} // namespace kdbrest
