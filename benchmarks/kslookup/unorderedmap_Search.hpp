#ifndef UNORDEREDMAP_SEARCH
#define UNORDEREDMAP_SEARCH

#include "unorderedmap_Interface.hpp"
#include <unordered_map>
#include <sys/time.h>
#include <iostream>

extern "C" {
  #include "benchmark.h"
}

class unorderedmap_Search: public unorderedmap_Interface
{
public:
	unorderedmap_Search (bool m);
	int run (int n,int k,int r, Data * data, int * values);
	bool getMode ();
};

#endif
