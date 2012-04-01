#include <tests.hpp>

#include "checker.hpp"

#include <locale>

using namespace elektra;


int main()
{
	cout << "  TYPE  TESTS" << endl;
	cout << "===============" << endl << endl;


	cout << endl;
	cout << "test_key RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
