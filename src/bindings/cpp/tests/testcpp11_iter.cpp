#include <tests.hpp>

#include <vector>
#include <algorithm>

void test_iterate()
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	ks3.begin();
	std::cout << (*ks3.begin()).getName() << std::endl;
	// std::cout << ks3.begin()->getName(); // broken
}

void test_for_loop()
{
	KeySet ks3 (5,
		*Key ("user/key3/1", KEY_END),
		*Key ("user/key3/2", KEY_END),
		*Key ("user/key3/3", KEY_VALUE, "value", KEY_END),
		KS_END);

	for (Key k:ks3)
	{
		std::cout << k.getName() << std::endl;
	}

	for (auto i = ks3.begin(); i != ks3.end(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}

	for (auto i = ks3.cbegin(); i != ks3.cend(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}

	for (auto i = ks3.rbegin(); i != ks3.rend(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}

	for (auto i = ks3.crbegin(); i != ks3.crend(); ++i)
	{
		Key k(*i);
		std::cout << k.getName() << std::endl;
	}
}


int main()
{
	cout << "KEYSET CLASS TESTS" << endl;
	cout << "==================" << endl << endl;

	test_iterate();
	test_for_loop();

	cout << endl;
	cout << "test_key RESULTS: " << nbTest << " test(s) done. " << nbError << " error(s)." << endl;
	return nbError;
}
