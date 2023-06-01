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
			KEY_VALUE, "glob plugin waits for your orders", KEY_END),
		keyNew ("system:/elektra/modules/glob/exports", KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/open",
			KEY_FUNC, elektraGlobOpen,
			KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/close",
			KEY_FUNC, elektraGlobClose,
			KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/get",
			KEY_FUNC, elektraGlobGet,
			KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/set",
			KEY_FUNC, elektraGlobSet,
			KEY_END),
		keyNew ("system:/elektra/modules/glob/exports/elektraGlobMatch",
			KEY_FUNC, elektraGlobMatch,
			KEY_END),
#include "./readme_glob.c"
		keyNew ("system:/elektra/modules/glob/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);

// clang-format on

#endif /* CONTRACT_H_ */
