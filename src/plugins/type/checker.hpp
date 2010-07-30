#ifndef CHECKER_HPP
#define CHECKER_HPP


#include <map>
#include <string>
#include <sstream>
#include <locale>


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

template <typename T>
class TType : public Type
{
public:
	bool check(Key k)
	{
		istringstream i (k.getString());
		i.imbue (locale("C"));
		T n;
		i >> n;
		if (i.fail()) return false;
		if (!i.eof()) return false;
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
		types.insert (pair<string, Type*>("short", new TType<int16_t>()));
		types.insert (pair<string, Type*>("unsigned_short", new TType<uint16_t>()));
		types.insert (pair<string, Type*>("long", new TType<int32_t>()));
		types.insert (pair<string, Type*>("unsigned_long", new TType<uint32_t>()));
		types.insert (pair<string, Type*>("long_long", new TType<int64_t>()));
		types.insert (pair<string, Type*>("unsigned_long_long", new TType<uint64_t>()));
		types.insert (pair<string, Type*>("float", new TType<float>()));
		types.insert (pair<string, Type*>("double", new TType<double>()));
		types.insert (pair<string, Type*>("char", new TType<unsigned char>()));
		types.insert (pair<string, Type*>("boolean", new TType<bool>()));
		types.insert (pair<string, Type*>("octet", new TType<unsigned char>()));
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
