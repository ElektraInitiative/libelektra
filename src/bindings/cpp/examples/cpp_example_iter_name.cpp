#include <keyset.hpp>

#include <iostream>

int main()
{
	using namespace kdb;

	Key k ("user/name/a/very/long\\/name/to\\\\/iterate\\with\\/some\\\\\\/escaping", KEY_END);

	std::cout << "forward iterate output: ";
	for (Key::iterator i = k.begin(); i != k.end(); ++i)
	{
		std::cout << *i << " ";
	}
	std::cout << std::endl;

	std::cout << "output reverse except first: ";
	for (Key::iterator i = --k.end(); i != k.begin(); --i)
	{
		std::cout << *i << " ";
	}
	std::cout << std::endl;
}
