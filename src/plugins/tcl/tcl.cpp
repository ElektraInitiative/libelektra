/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "tcl.hpp"

#include <key.hpp>
#include <keyset.hpp>

#include <fstream>

#include "errno.h"

#include <boost/spirit/include/qi_expect.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>

#include <kdberrors.h>

extern "C" {
using namespace ckdb;

int elektraTclGet (Plugin *, KeySet * returned, Key * parentKey)
{
	kdb::Key parent (parentKey);
	if (parent.getName () == "system:/elektra/modules/tcl")
	{
		/* get config */
		KeySet * n;
		ksAppend (returned,
			  n = ksNew (30, keyNew ("system:/elektra/modules/tcl", KEY_VALUE, "tcl plugin waits for your orders", KEY_END),
				     keyNew ("system:/elektra/modules/tcl/exports", KEY_END),
				     keyNew ("system:/elektra/modules/tcl/exports/get", KEY_FUNC, elektraTclGet, KEY_END),
				     keyNew ("system:/elektra/modules/tcl/exports/set", KEY_FUNC, elektraTclSet, KEY_END),
				     keyNew ("system:/elektra/modules/tcl/exports/cpp_serialise", KEY_SIZE, sizeof (&elektra::serialise),
					     KEY_BINARY, KEY_VALUE, &elektra::serialise, KEY_END),
				     keyNew ("system:/elektra/modules/tcl/exports/cpp_unserialise", KEY_SIZE,
					     sizeof (&elektra::unserialise), KEY_BINARY, KEY_VALUE, &elektra::unserialise, KEY_END),
#include "readme_tcl.c"
				     keyNew ("system:/elektra/modules/tcl/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
		ksDel (n);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	/* get all keys */

	int errnosave = errno;
	std::ifstream in (parent.getString (), std::ios::binary); // we get our input from this file
	if (!in.is_open ())
	{
		ELEKTRA_SET_ERROR_GET (*parent);
		errno = errnosave;
		parent.release ();
		return -1;
	}

	kdb::KeySet input (returned);

	int ret = 0;
	try
	{
		elektra::unserialise (in, input, parent);
	}
	catch (boost::spirit::qi::expectation_failure<boost::spirit::istream_iterator> const & e)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (*parent,
							std::string (std::string ("File '") + parent.getString () +
								     "' could not be parsed. Reason: " + std::string (e.first, e.last))
								.c_str ());
		ret = -1;
	}
	catch (std::exception const & e)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (
			*parent, std::string (std::string ("File '") + parent.getString () + "' could not be parsed. Reason: " + e.what ())
					 .c_str ());
		ret = -1;
	}
	input.release ();
	parent.release ();

	return ret;
}

int elektraTclSet (Plugin *, KeySet * returned, Key * parentKey)
{
	/* set all keys */

	int errnosave = errno;
	kdb::Key parent (parentKey);
	std::ofstream ofs (parent.getString (), std::ios::binary);
	if (!ofs.is_open ())
	{
		ELEKTRA_SET_ERROR_SET (*parent);
		errno = errnosave;
		return -1;
	}

	kdb::KeySet output (returned);

	elektra::serialise (ofs, output, parent);
	parent.release ();
	output.release ();

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("tcl",
		ELEKTRA_PLUGIN_GET,	&elektraTclGet,
		ELEKTRA_PLUGIN_SET,	&elektraTclSet,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
