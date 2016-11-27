/**
 * @file
 *
 * @brief application main file - entry point
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
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
	if (kdbrest::Config::initializeConfiguration (config))
	{
		std::cerr << "Please fix the applications configuration first!" << std::endl;
		return 1;
	}

	// force caching of database
	std::cout << "Pre-caching data..." << std::endl;
	(void)kdbrest::service::StorageEngine::instance ();

	// launch rest API
	std::cout << "Starting REST API server..." << std::endl;
	try
	{
		cppcms::service srv (config.at ("cppcms"));
		srv.applications_pool ().mount (cppcms::applications_factory<kdbrest::RootApp> ());
		srv.run ();
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		std::cerr << "CppCMS configuration not found, cannot start service." << std::endl;
	}
	catch (std::exception const & e)
	{
		std::cerr << e.what () << std::endl;
	}

	return 0;
}
