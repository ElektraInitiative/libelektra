/**
 * @file
 *
 * @brief Implementation of proposed API enhancements.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <ctype.h>
#include <string.h>

#include <kdbassert.h>
#include <kdbinternal.h>
#include <kdblogger.h>

/**
 * @defgroup proposal Proposals for Elektra
 * @brief Might be added to, changed or removed from future Elektra releases.
 */

/**
 * @defgroup api API Proposals for Elektra
 * @brief for kdb.h.
 * @ingroup proposal
 *
 * @warning Do not use these methods if you do not want to depend on
 * exactly the Elektra version your binary was built for.
 *
 * These methods are a technical preview of what might be added in
 * future Elektra releases. It is a requirement that methods are first
 * added here, before they are added to the public API.
 *
 * Usually, names in proposal stage should be prefixed with elektra to
 * clearly mark that the signature is likely to be changed and not yet
 * ABI compatible.
 *
 * @{
 */

/**
 * @brief Return metadata as keyset
 *
 * @param key the key object to work with
 *
 * @return a duplication of the keyset representing the metadata
 */
KeySet * elektraKeyGetMetaKeySet (const Key * key)
{
	if (!key) return 0;
	if (!key->meta) return 0;

	return ksDup (key->meta);
}
