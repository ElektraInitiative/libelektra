#include <ckdb.h>
#include <iostream>
#include <string>

using namespace std;

int main(int argc, char ** argv)
{
	ckdb config("sw/test");
	if (argc < 3)
	{
		cout << argv[0] << " {set|get}" << " keyname" << endl;
		return 1;
	}
	
	if (strcmp (argv[1], "get") == 0)
	{	
		cout << config.get (argv[2]) << endl;
	} else if (strcmp (argv[1], "set") == 0)
	{
		string s;
		cout << "Enter Text for Value, End with ." << endl;
		getline (cin, s, '.');	
		config.set (argv[2], s);
	}
	return 0;
}
