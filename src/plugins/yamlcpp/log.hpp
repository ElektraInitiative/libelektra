/**
 * @file
 *
 * @brief Log data using Elektra’s logging facility
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAMLCPP_LOG_HPP
#define ELEKTRA_PLUGIN_YAMLCPP_LOG_HPP

#include <kdb.hpp>
#include <kdblogger.h>

#ifdef HAVE_LOGGER

namespace
{

/**
 * @brief This function uses Elektra’s logging facility to print the contents of a key set.
 *
 * @param keys This parameter stores the key set this function prints.
 */
void logKeySet (kdb::KeySet const & keys)
{
	for (auto key : keys)
	{
		std::string metadata;
		ckdb::KeySet * metaKeys = ckdb::keyMeta (key.getKey ());

		for (elektraCursor it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
		{
			const kdb::Key curMeta (ckdb::ksAtCursor (metaKeys, it));
			metadata += ", “" + curMeta.getName () + "”: “" + curMeta.getString () + "”";
		}

		ELEKTRA_LOG_DEBUG ("\t“%s”: “%s”%s", key.getName ().c_str (),
				   key.getBinarySize () == 0 ? "NULL" :
				   key.isBinary ()	     ? "binary value!" :
							       key.getString ().c_str (),
				   metadata.c_str ());
	}
}

} // end namespace

#endif // HAVE_LOGGER

#endif // ELEKTRA_PLUGIN_YAMLCPP_LOG_HPP
