#ifndef UNORDEREDMAP_BUILD
#define UNORDEREDMAP_BUILD

#include "unorderedmap_Interface.hpp"
#include <unordered_map>
#include <sys/time.h>
#include <iostream>

class unorderedmap_Build: public unorderedmap_Interface
{
public:
	unorderedmap_Build (bool m);
	int run (int n,int k,int r, Data * data, int * values);
	bool getMode ();
};

#endif
