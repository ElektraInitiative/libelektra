#include <key.hpp>
#include <iostream>

int main()
{
	kdb::Key k("user/hello", KEY_VALUE, "Hello World", KEY_END);
	std::cout << k.getString() << std::endl;
}
