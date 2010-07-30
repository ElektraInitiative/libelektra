#ifndef CHECKER_HPP
#define CHECKER_HPP


#include <map>
#include <string>
#include <sstream>


namespace elektra {

using namespace kdb;
using namespace std;

class Type
{
public:
	virtual bool check(Key k) = 0;
	virtual ~Type();
};

Type::~Type()
{}

class AnyType : public Type
{
public:
	bool check(Key)
	{
		return true;
	}
};

class EmptyType : public Type
{
public:
	bool check(Key k)
	{
		return k.getString().empty();
	}
};

class ShortType : public Type
{
public:
	bool check(Key k)
	{
		istringstream i (k.getString());
		int16_t n;
		i >> n;
		if (i.fail()) return false;
		return true;
	}
};

class TypeChecker
{
	std::map<string, Type*> types;

public:
	TypeChecker()
	{
		types.insert (pair<string, Type*>("any", new AnyType()));
		types.insert (pair<string, Type*>("empty", new EmptyType()));
		types.insert (pair<string, Type*>("short", new ShortType()));
	}

	bool check (Key &k)
	{
		string typeList;
		/* TODO: meta interface without exceptions needed */
		try { typeList = k.getMeta<string>("check/type"); }
		catch (...) { return true; }
		istringstream istr (typeList);
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
