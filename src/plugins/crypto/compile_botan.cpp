/**
 * @file
 *
 * @brief compilation test for checking if the Botan library is available.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <botan/init.h>

int main (void)
{
	Botan::LibraryInitializer::initialize ("");
	return 0;
}
