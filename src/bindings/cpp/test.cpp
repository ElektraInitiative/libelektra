#include <kdb>
#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

Kdb config;

void getKeys()
{
	KeySet test;
	config.getKeys (test, "user/test"); // read in test key hierachy
}

void getKey()
{
	Key test;
	test.setName ("user/hello");
	config.getKey (test); // read in test key
	cout << "my first key read is: " << test.getFullName() << endl;
	cout << "the value is: " << test.getString() << endl;
}

void setKey()
{
	Key test;
	test.setName ("user/toset");
	test.setString ("für patrick");
	config.setKey (test);
}

int main()
{
	//getKey();
	//setKey();
	getKeys();
	return 0;
}
