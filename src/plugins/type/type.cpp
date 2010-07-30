/***************************************************************************
                     type.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "type.hpp"

#include <key>
#include <keyset>

#include <map>
#include <string>
#include <sstream>


// TODO: remove debug
#include <iostream>

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

	bool check (KeySet &ks)
	{
		Key k;
		while (k = ks.next())
		{
			string typeList;
			/* TODO: meta interface without exceptions needed */
			try { typeList = k.getMeta<string>("check/type"); }
			catch (...) { continue; }
			istringstream istr (typeList);
			string type;
			while (istr >> type)
			{
				cout << "check " << k.getName() << " with " << type << endl;
				if (types[type] && types[type]->check(k)) goto next;
			}

			/* Type could not be checked successfully */
			return false;
next:;
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


using namespace ckdb;

#include <kdberrors.h>

extern "C"
{

int elektraTypeOpen(ckdb::Plugin *handle, ckdb::Key *)
{
	/* plugin initialization logic */

	elektraPluginSetData (handle, new elektra::TypeChecker());

	return 1; /* success */
}

int elektraTypeClose(ckdb::Plugin *handle, ckdb::Key *)
{
	/* free all plugin resources and shut it down */

	delete static_cast<elektra::TypeChecker*>(elektraPluginGetData (handle));

	return 1; /* success */
}

int elektraTypeGet(ckdb::Plugin *, ckdb::KeySet *returned, ckdb::Key *)
{
	/* configuration only */
	KeySet *n;
	ksAppend (returned, n=ksNew (30,
		keyNew ("system/elektra/modules/type",
			KEY_VALUE, "type plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/type/exports", KEY_END),
		keyNew ("system/elektra/modules/type/exports/open",
			KEY_FUNC, elektraTypeOpen,
			KEY_END),
		keyNew ("system/elektra/modules/type/exports/close",
			KEY_FUNC, elektraTypeClose,
			KEY_END),
		keyNew ("system/elektra/modules/type/exports/get",
			KEY_FUNC, elektraTypeGet,
			KEY_END),
		keyNew ("system/elektra/modules/type/exports/set",
			KEY_FUNC, elektraTypeSet,
			KEY_END),
		keyNew ("system/elektra/modules/type/infos",
			KEY_VALUE, "All information you want to know", KEY_END),
		keyNew ("system/elektra/modules/type/infos/author",
			KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system/elektra/modules/type/infos/licence",
			KEY_VALUE, "BSD", KEY_END),
		keyNew ("system/elektra/modules/type/infos/description",
			KEY_VALUE, "Copies meta data to keys using typebing", KEY_END),
		keyNew ("system/elektra/modules/type/infos/provides",
			KEY_VALUE, "type", KEY_END),
		keyNew ("system/elektra/modules/type/infos/placements",
			KEY_VALUE, "presetstorage", KEY_END),
		keyNew ("system/elektra/modules/type/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/type/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraTypeSet(ckdb::Plugin *handle, ckdb::KeySet *returned, ckdb::Key *parentKey)
{
	/* set all keys */

	if (!static_cast<elektra::TypeChecker*>(elektraPluginGetData (handle))->check
			(reinterpret_cast<kdb::KeySet&>(returned)))
	{
		std::string msg = "None of supplied types matched for ";
		const char *name = keyName (ksCurrent(returned));
		if (name) msg += name;
		msg += " with string: ";
		const char *value = keyString (ksCurrent(returned));
		if (value) msg += value;
		ELEKTRA_SET_ERROR (52, parentKey, msg.c_str());
		return -1;
	}

	return 1; /* success */
}

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(type)
{
	return elektraPluginExport("type",
		ELEKTRA_PLUGIN_OPEN,	&elektraTypeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTypeClose,
		ELEKTRA_PLUGIN_GET,	&elektraTypeGet,
		ELEKTRA_PLUGIN_SET,	&elektraTypeSet,
		ELEKTRA_PLUGIN_END);
}

}
