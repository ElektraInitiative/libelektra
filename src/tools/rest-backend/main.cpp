#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/service.h>
#include <iostream>

#include "root_application.hpp"


int main (int argc, char ** argv)
{

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
}
