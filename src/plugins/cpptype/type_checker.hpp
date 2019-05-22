/**
 * @file
 *
 * @brief Implementation of data type checker
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CPPTYPE_TYPE_CHECKER_HPP
#define ELEKTRA_CPPTYPE_TYPE_CHECKER_HPP

#include <locale>
#include <map>
#include <set>
#include <sstream>
#include <string>

#include "kdbtypes.h"
#include "types.hpp"


namespace elektra
{

using namespace kdb;
using namespace std;


class TypeChecker
{
	std::map<string, Type *> types;
	bool enforce;

public:
	explicit TypeChecker (KeySet config)
	{
		enforce = config.lookup ("/enforce");
		Key k = config.lookup ("/require_version");
		if (k && k.getString () != "2") throw "Required Version does not match 2";

		types.insert (pair<string, Type *> ("short", new MType<kdb::short_t> ()));
		types.insert (pair<string, Type *> ("unsigned_short", new MType<kdb::unsigned_short_t> ()));
		types.insert (pair<string, Type *> ("long", new MType<kdb::long_t> ()));
		types.insert (pair<string, Type *> ("unsigned_long", new MType<kdb::unsigned_long_t> ()));
		types.insert (pair<string, Type *> ("long_long", new MType<kdb::long_long_t> ()));
		types.insert (pair<string, Type *> ("unsigned_long_long", new MType<kdb::unsigned_long_long_t> ()));

		types.insert (pair<string, Type *> ("float", new TType<kdb::float_t> ()));
		types.insert (pair<string, Type *> ("double", new TType<kdb::double_t> ()));
		types.insert (pair<string, Type *> ("long_double", new TType<kdb::long_double_t> ()));
		types.insert (pair<string, Type *> ("boolean", new TType<kdb::boolean_t> ()));

		types.insert (pair<string, Type *> ("char", new CharType ()));
		types.insert (pair<string, Type *> ("octet", new CharType ()));

		types.insert (pair<string, Type *> ("any", new AnyType ()));
		types.insert (pair<string, Type *> ("string", new StringType ()));

		// non-standard types (deprecated, just for
		// compatibility):
		types.insert (pair<string, Type *> ("empty", new EmptyType ()));
		types.insert (pair<string, Type *> ("FSType", new FSType ()));
	}

	bool check (Key & k)
	{
		Key m = k.getMeta<const Key> ("check/type");
		if (!m) m = k.getMeta<const Key> ("type");
		if (!m) return !enforce;

		istringstream istr (m.getString ());
		string type;
		while (istr >> type)
		{
			if (types[type] && types[type]->check (k)) return true;
		}

		/* Type could not be checked successfully */
		return false;
	}

	bool check (KeySet & ks)
	{
		Key k;
		while ((k = ks.next ()))
		{
			if (!check (k)) return false;
		}
		return true;
	}

	~TypeChecker ()
	{
		map<string, Type *>::iterator it;
		for (it = types.begin (); it != types.end (); ++it)
		{
			delete it->second;
		}
	}
};

} // end namespace elektra

#endif
