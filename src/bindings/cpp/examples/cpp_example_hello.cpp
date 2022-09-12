/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <key.hpp>
#include <keyio.hpp>

#include <iostream>

int main ()
{
	kdb::Key k ("user:/hello_world", ELEKTRA_KEY_VALUE, "Hello World", ELEKTRA_KEY_END);
	std::cout << k << std::endl;
}
