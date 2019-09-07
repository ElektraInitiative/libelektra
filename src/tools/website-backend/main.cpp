/**
 * @file
 *
 * @brief application main file - entry point
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <iostream>

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/service.h>

#include <config.hpp>
#include <root_application.hpp>
#include <service.hpp>

/**
 * @brief main function that bootstraps the application
 *
 * @param argc command line argument count
 * @param argv command line argument list
 * @return 0 in case the service terminated gracefullly,
 *		   >0 otherwise
 */
int main ()
{
	// loading application configuration
	cppcms::json::value config = kdbrest::service::ConfigEngine::instance ().loadApplicationConfiguration ();
	std::cout << "Loaded Application Configuration\n" << config.save (cppcms::json::readable) << std::endl;
	if (!kdbrest::Config::instance ().initializeConfiguration (config))
	{
		std::cerr << "Please fix the applications configuration first!" << std::endl;
		return 1;
	}

	// launch rest API
	std::cout << "Starting REST API server..." << std::endl;
	try
	{
		cppcms::service srv (config.at ("cppcms"));
		srv.applications_pool ().mount (cppcms::applications_factory<kdbrest::RootApp> ());
		srv.run ();
	}
	catch (cppcms::json::bad_value_cast const & e)
	{
		std::cerr << "CppCMS configuration not found, cannot start service." << std::endl;
	}
	catch (std::exception const & e)
	{
		std::cerr << e.what () << std::endl;
	}

	return 0;
}
