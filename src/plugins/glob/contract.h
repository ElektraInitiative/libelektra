/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CONTRACT_H_
#define CONTRACT_H_

// clang-format off

ksNew (30,
		keyNew ("system:/elektra/modules/glob",
			ELEKTRA_KEY_VALUE, "glob plugin waits for your orders", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/glob/exports", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/open",
			ELEKTRA_KEY_FUNC, elektraGlobOpen,
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/close",
			ELEKTRA_KEY_FUNC, elektraGlobClose,
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/get",
			ELEKTRA_KEY_FUNC, elektraGlobGet,
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/set",
			ELEKTRA_KEY_FUNC, elektraGlobSet,
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/elektraGlobMatch",
			ELEKTRA_KEY_FUNC, elektraGlobMatch,
			ELEKTRA_KEY_END),
#include "readme_glob.c"
		keyNew ("system:/elektra/modules/glob/infos/version",
			ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
		ELEKTRA_KS_END);

// clang-format on

#endif /* CONTRACT_H_ */
