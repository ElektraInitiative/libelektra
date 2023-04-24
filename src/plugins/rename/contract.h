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

ksNew (30,
	keyNew ("system:/elektra/modules/rename",
		KEY_VALUE, "rename plugin waits for your orders", KEY_END),
	keyNew ("system:/elektra/modules/rename/exports", KEY_END),
	keyNew ("system:/elektra/modules/rename/exports/get",
		KEY_FUNC, elektraRenameGet,
		KEY_END),
	keyNew ("system:/elektra/modules/rename/exports/set",
		KEY_FUNC, elektraRenameSet,
		KEY_END),
#include "./readme_rename.c"
	keyNew ("system:/elektra/modules/rename/infos",
		KEY_VALUE, "All information you want to know", KEY_END),
	keyNew ("system:/elektra/modules/rename/infos/version",
		KEY_VALUE, PLUGINVERSION, KEY_END),
	KS_END);

// clang-format on

#endif /* CONTRACT_RENAME_H_ */
