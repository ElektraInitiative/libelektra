#ifndef ELEKTRA_TYPE_CHECKER_HPP
#define ELEKTRA_TYPE_CHECKER_HPP

#include <set>
#include <map>
#include <string>
#include <sstream>
#include <locale>

#include "types.hpp"


namespace elektra {

using namespace kdb;
using namespace std;



class TypeChecker
{
	std::map<string, Type*> types;
	bool enforce;

public:
	TypeChecker(KeySet config)
	{
		enforce = config.lookup("/enforce");
		Key k = config.lookup("/require_version");
		if (k && k.getString() != "2") throw "Required Version does not match 2";

		types.insert (pair<string, Type*>("any", new AnyType()));
		types.insert (pair<string, Type*>("empty", new EmptyType()));

		types.insert (pair<string, Type*>("short", new MType<int16_t>()));
		types.insert (pair<string, Type*>("unsigned_short", new MType<uint16_t>()));
		types.insert (pair<string, Type*>("long", new MType<int32_t>()));
		types.insert (pair<string, Type*>("unsigned_long", new MType<uint32_t>()));
		types.insert (pair<string, Type*>("long_long", new MType<int64_t>()));
		types.insert (pair<string, Type*>("unsigned_long_long", new MType<uint64_t>()));

		types.insert (pair<string, Type*>("float", new TType<float>()));
		types.insert (pair<string, Type*>("double", new TType<double>()));
		types.insert (pair<string, Type*>("char", new TType<unsigned char>()));
		types.insert (pair<string, Type*>("boolean", new TType<bool>()));
		types.insert (pair<string, Type*>("octet", new TType<unsigned char>()));
		types.insert (pair<string, Type*>("FSType", new FSType()));
		types.insert (pair<string, Type*>("string", new StringType()));
	}

	bool check (Key &k)
	{
		Key const m = k.getMeta<const Key>("check/type");
		if (!m) return !enforce;

		istringstream istr (m.getString());
		string type;
		while (istr >> type)
		{
			if (types[type] && types[type]->check(k)) return true;
		}

		/* Type could not be checked successfully */
		return false;
	}

	bool check (KeySet &ks)
	{
		Key k;
		while (k = ks.next())
		{
			if (!check(k)) return false;
		}
		return true;
	}

	~TypeChecker()
	{
		map<string,Type*>::iterator it;
		for ( it=types.begin() ; it != types.end(); it++)
		{
			delete it->second;
		}
	}

};

} // end namespace elektra

#endif
