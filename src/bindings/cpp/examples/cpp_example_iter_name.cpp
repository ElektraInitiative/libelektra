#include <keyset.hpp>

#include <iostream>

int main()
{
	using namespace kdb;

	Key k ("user/name/a/very/long\\/name/to\\\\/iterate\\with\\/some\\\\\\/escaping", KEY_END);

	Key::iterator it = k.begin();

	for (Key::iterator i = k.begin(); i != k.end(); ++i)
	{
		std::cout << *i << std::endl;
	}
}
