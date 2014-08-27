/***************************************************************************
                     tcl.c  -  Skeleton of a plugin
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


#include "tcl.hpp"

#include <keyset.hpp>
#include <key.hpp>

#include <fstream>

#include <boost/spirit/include/qi_expect.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>

using namespace ckdb;
#include <kdberrors.h>

extern "C"
{

int elektraTclGet(Plugin *, KeySet *returned, Key *parentKey)
{
	std::string cur = keyName(parentKey);
	if (cur == "system/elektra/modules/tcl")
	{
		/* get config */
		KeySet *n;
		void (*serialise) (void) = (void (*) (void)) elektra::serialise;
		void (*unserialise) (void) = (void (*) (void)) elektra::unserialise;
		ksAppend (returned, n=ksNew (30,
			keyNew ("system/elektra/modules/tcl",
				KEY_VALUE, "tcl plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/tcl/exports", KEY_END),
			keyNew ("system/elektra/modules/tcl/exports/get",
				KEY_FUNC, elektraTclGet,
				KEY_END),
			keyNew ("system/elektra/modules/tcl/exports/set",
				KEY_FUNC, elektraTclSet,
				KEY_END),
			keyNew ("system/elektra/modules/tcl/exports/cpp_serialise",
				KEY_SIZE, sizeof (serialise),
				KEY_BINARY,
				KEY_VALUE, &serialise, KEY_END),
			keyNew ("system/elektra/modules/tcl/exports/cpp_unserialise",
				KEY_SIZE, sizeof (unserialise),
				KEY_BINARY,
				KEY_VALUE, &unserialise, KEY_END),
#include "readme_tcl.c"
			keyNew ("system/elektra/modules/tcl/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END));
		ksDel (n);
	}
	/* get all keys */

	std::ifstream in(keyString(parentKey), std::ios::binary);    // we get our input from this file
	if (!in.is_open()) return 0;

	kdb::KeySet input (returned);

	int ret = 0;
	try
	{
		elektra::unserialise (in, input);
	}
	catch(boost::spirit::qi::expectation_failure<boost::spirit::istream_iterator> const& e)
	{
		ELEKTRA_SET_ERROR (61, parentKey,
				std::string(std::string("file: ") +
				keyString(parentKey) +
				" could not be parsed because: " +
				std::string(e.first, e.last)).c_str());
		ret = -1;
	}
	catch(std::exception const& e)
	{
		ELEKTRA_SET_ERROR (61, parentKey,
				std::string(std::string("file: ") +
				keyString(parentKey) +
				" could not be parsed because: " +
				e.what()).c_str());
		ret = -1;
	}
	input.release();

	return ret;
}

int elektraTclSet(Plugin *, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	std::ofstream ofs(keyString(parentKey), std::ios::binary);
	if (!ofs.is_open())
	{
		ELEKTRA_SET_ERROR (9, parentKey, "file is not open in tcl");
		return -1;
	}

	kdb::KeySet output (returned);

	elektra::serialise (ofs, output);
	output.release();

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(tcl)
{
	return elektraPluginExport("tcl",
		ELEKTRA_PLUGIN_GET,	&elektraTclGet,
		ELEKTRA_PLUGIN_SET,	&elektraTclSet,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
