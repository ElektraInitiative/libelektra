/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <keyset.hpp>

#include <iostream>

int main ()
{
	using namespace kdb;

	Key k ("user:/name/a/very/long\\/name/to\\\\/iterate\\with\\/some\\\\\\/escaping", KEY_END);

	std::cout << "begin: " << *k.begin () << std::endl;
	std::cout << "end: " << *k.end () << std::endl;

	std::cout << "rbegin: " << *k.rbegin () << std::endl;
	std::cout << "rend: " << *k.rend () << std::endl;

	std::cout << "forward iterator: ";
	for (auto && elem : k)
	{
		std::cout << elem << " ";
	}
	std::cout << std::endl;

	std::cout << "output reverse except first: ";
	for (Key::iterator i = --k.end (); i != k.begin (); --i)
	{
		std::cout << *i << " ";
	}
	std::cout << std::endl;

	std::cout << "reverse iterator: ";
	for (Key::reverse_iterator i = k.rbegin (); i != k.rend (); ++i)
	{
		std::cout << *i << " ";
	}
	std::cout << std::endl;

	std::cout << "output except first: ";
	for (Key::reverse_iterator i = --k.rend (); i != k.rbegin (); --i)
	{
		std::cout << *i << " ";
	}
	std::cout << std::endl;
}
