/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "tcl.hpp"

#include <keyset.hpp>
#include <key.hpp>

#include <fstream>

#include "errno.h"

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
				KEY_SIZE, sizeof (&elektra::serialise),
				KEY_BINARY,
				KEY_VALUE, &elektra::serialise, KEY_END),
			keyNew ("system/elektra/modules/tcl/exports/cpp_unserialise",
				KEY_SIZE, sizeof (&elektra::unserialise),
				KEY_BINARY,
				KEY_VALUE, &elektra::unserialise, KEY_END),
#include "readme_tcl.c"
			keyNew ("system/elektra/modules/tcl/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END));
		ksDel (n);
	}
	/* get all keys */

	int errnosave = errno;
	std::ifstream in(keyString(parentKey), std::ios::binary);    // we get our input from this file
	if (!in.is_open())
	{
		ELEKTRA_SET_ERROR_GET(parentKey);
		errno = errnosave;
		return -1;
	}

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

	int errnosave = errno;
	std::ofstream ofs(keyString(parentKey), std::ios::binary);
	if (!ofs.is_open())
	{
		ELEKTRA_SET_ERROR_SET(parentKey);
		errno = errnosave;
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
