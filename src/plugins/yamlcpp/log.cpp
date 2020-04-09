/**
 * @file
 *
 * @brief Log data using Elektra’s logging facility
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.hpp>
#include <kdblogger.h>

#include "log.hpp"

#ifdef HAVE_LOGGER

namespace yamlcpp
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
		key.rewindMeta ();
		while (kdb::Key meta = key.nextMeta ())
		{
			metadata += ", “" + meta.getName () + "”: “" + meta.getString () + "”";
		}

		ELEKTRA_LOG_DEBUG ("\t“%s”: “%s”%s", key.getName ().c_str (),
				   key.getBinarySize () == 0 ? "NULL" : key.isBinary () ? "binary value!" : key.getString ().c_str (),
				   metadata.c_str ());
	}
}

} // end namespace yamlcpp

#endif // HAVE_LOGGER
