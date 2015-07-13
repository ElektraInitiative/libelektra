/**
* \file
*
* \brief Testcases for Struct
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/


#include <tests.hpp>

#include "checker.hpp"

#include <locale>

using namespace elektra;


int main()
{
	cout << "  STRUCT  TESTS" << endl;
	cout << "===============" << endl << endl;


	cout << endl;
	cout << "testmod_struct RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
