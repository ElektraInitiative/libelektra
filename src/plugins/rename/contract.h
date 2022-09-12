/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CONTRACT_RENAME_H_
#define CONTRACT_RENAME_H_

// clang-format off

elektraKeysetNew (30,
	elektraKeyNew ("system:/elektra/modules/rename",
		ELEKTRA_KEY_VALUE, "rename plugin waits for your orders", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/rename/exports", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/rename/exports/get",
		ELEKTRA_KEY_FUNC, elektraRenameGet,
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/rename/exports/set",
		ELEKTRA_KEY_FUNC, elektraRenameSet,
		ELEKTRA_KEY_END),
#include "readme_rename.c"
	elektraKeyNew ("system:/elektra/modules/rename/infos",
		ELEKTRA_KEY_VALUE, "All information you want to know", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/rename/infos/version",
		ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
	ELEKTRA_KS_END);

// clang-format on

#endif /* CONTRACT_RENAME_H_ */
