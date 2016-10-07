#include <iostream>

#include "conversion_application.hpp"
#include "model_configformat.hpp"
#include "model_entry.hpp"
#include "model_importedconfig.hpp"
#include "root_application.hpp"
#include "service.hpp"

namespace kdbrest
{

/**
 * @brief the constructor of the conversion endpoint application.
 * @param srv a service container
 */
ConversionApp::ConversionApp (cppcms::service & srv) : cppcms::application (srv)
{
	dispatcher ().assign ("/formats", &ConversionApp::formats, this);
	mapper ().assign ("formats", "/formats");

	dispatcher ().assign ("", &ConversionApp::convert, this);
	mapper ().assign ("");
}

/**
 * @brief handler for the conversion resource.
 *        takes a snippet and configuration format as input and converts the
 *		  snippet into a desired output format.
 */
void ConversionApp::convert ()
{
	RootApp::setCORSHeaders (response (), "POST,OPTIONS");

	if (request ().request_method () == "POST")
	{
		// check if request data is of type application/json
		if (request ().content_type_parsed ().media_type () != "application/json")
		{
			RootApp::setNotAcceptable (response (), "You have supplied an unsupported Content-Type.",
						   "REQUEST_UNSUPPORTED_CONTENT_TYPE");
			return; // quit early
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
			return; // quit early
		}

		// required request fields
		std::string input_format;
		std::string input_value;
		std::string output_format;

		// retrieve submitted data
		try
		{
			input_format = requestData.get<std::string> (INDEX_INPUT_FORMAT);
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (response (), "You have to supply an input format.", "CONVERT_MISSING_INPUT_FORMAT");
			return; // quit early
		}
		try
		{
			input_value = requestData.get<std::string> (INDEX_INPUT_VALUE);
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (response (), "You have to supply an input value (configuration snippet).",
						"CONVERT_MISSING_INPUT_SNIPPET");
			return; // quit early
		}
		try
		{
			output_format = requestData.get<std::string> (INDEX_OUTPUT_FORMAT);
		}
		catch (cppcms::json::bad_value_cast & e)
		{
			RootApp::setBadRequest (response (), "You have to supply an output format.", "CONVERT_MISSING_OUTPUT_FORMAT");
			return; // quit early
		}

		// attempt to convert the snippet into internal format
		model::Entry entry ("");
		try
		{
			model::ImportedConfig cfg = service::ConvertEngine::instance ().import (input_value, input_format, entry);
			entry.addSubkeys (cfg.getKeySet ());
		}
		catch (kdbrest::exception::UnsupportedConfigurationFormatException & e)
		{
			RootApp::setBadRequest (response (), "The given input format is not supported.", "CONVERT_INVALID_INPUT_FORMAT");
			return; // quit early
		}
		catch (kdbrest::exception::ParseConfigurationException & e)
		{
			RootApp::setBadRequest (response (), "The given configuration could not be parsed within the given format.",
						"CONVERT_UNABLE_TO_PARSE_SNIPPET");
			return;
		}

		model::ConfigFormat cfg;
		try
		{
			model::PluginFormat pluginFormat = service::ConvertEngine::instance ().findSuitablePlugin (output_format);
			cfg = service::ConvertEngine::instance ().exportTo (pluginFormat, entry);
		}
		catch (kdbrest::exception::UnsupportedConfigurationFormatException & e)
		{
			RootApp::setUnprocessableEntity (response (), "The given output format is not supported.",
							 "CONVERT_INVALID_OUTPUT_FORMAT ");
			return; // quit early
		}
		catch (kdbrest::exception::ParseConfigurationException & e)
		{
			RootApp::setUnprocessableEntity (response (),
							 "The given configuration snippet cannot be represented by the"
							 " given output format.",
							 "CONVERT_UNABLE_TO_CONVERT_SNIPPET");
			return; // quit early
		}

		// create response
		cppcms::json::value data;

		data["message"] = "The configuration snippet has been converted successfully.";
		data["i18n"] = "CONVERT_SUCCESSFUL";

		data["output"]["format"] = output_format;
		data["output"]["snippet"] = cfg.getConfig ();
		data["output"]["validated"] = cfg.isValidated ();

		RootApp::setOk (response (), data, "application/json");
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
 * @brief handler to retrieve a list of supported formats with their corresponding plugin
 */
void ConversionApp::formats ()
{
	RootApp::setCORSHeaders (response (), "GET,OPTIONS");

	if (request ().request_method () == "GET")
	{
		cppcms::json::value data;

		// available config formats
		int index = 0;
		for (auto & elem : service::ConvertEngine::instance ().getEnabledFormats ())
		{
			data[index]["format"] = elem.getFileformat ();
			data[index]["plugin"] = elem.getPluginname ();
			index++;
		}

		RootApp::setOk (response (), data, "application/json");
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

} // namespace kdbrest
