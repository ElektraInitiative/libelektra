#include <ckdb.h>
#include <iostream>
#include <string>

using namespace std;

int main()
{
	ckdb config("sw/test");

	cout << config.get ("hallo") << endl;
	config.set ("what", "hejo");
	config.set ("hallo", "jo");
	config.set ("nono", "what");
	config.set ("hallo", "successfully overwritten");
	config.write();
	cout << config.get ("hallo") << endl;
	config.set ("destr", "works also");
	return 0;
}
