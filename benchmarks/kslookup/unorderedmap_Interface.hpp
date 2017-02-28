#ifndef UNORDEREDMAP_INTERFACE
#define UNORDEREDMAP_INTERFACE

#include <string>
#include <utility>
#include <vector>

#include <key.hpp>
#include <keyset.hpp>
#include <kdb.hpp>

typedef std::pair<std::string, kdb::Key> Data;

class unorderedmap_Interface
{
public:
	virtual int run (int n,int k,int r, Data * data, int * values) = 0;
	virtual bool getMode () = 0;
protected:
	bool mode;
};

#endif
