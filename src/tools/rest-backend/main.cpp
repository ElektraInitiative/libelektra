#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/service.h>
#include <iostream>

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
int main (int argc, char ** argv)
{
	// force caching of database
	std::cout << "Pre-caching data..." << std::endl;
	(void)kdbrest::service::StorageEngine::instance ();

	// launch rest API
	std::cout << "Starting REST API server..." << std::endl;
	try
	{
		cppcms::service srv (argc, argv);
		srv.applications_pool ().mount (cppcms::applications_factory<kdbrest::RootApp> ());
		srv.run ();
	}
	catch (std::exception const & e)
	{
		std::cerr << e.what () << std::endl;
	}

	return 0;
}
