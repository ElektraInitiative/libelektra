#include <keyset.hpp>

#include <iostream>

int main()
{
	using namespace kdb;

	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	for (KeySet::iterator i = ks3.begin(); i != ks3.end(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}
}
