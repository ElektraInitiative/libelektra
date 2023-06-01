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
			keyNew ("system:/elektra/modules/keytometa",
				KEY_VALUE, "keytometa plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/keytometa/exports", KEY_END),
			keyNew ("system:/elektra/modules/keytometa/exports/get",
				KEY_FUNC, elektraKeyToMetaGet,
				KEY_END),
			keyNew ("system:/elektra/modules/keytometa/exports/set",
				KEY_FUNC, elektraKeyToMetaSet,
				KEY_END),
			keyNew ("system:/elektra/modules/keytometa/exports/close",
				KEY_FUNC, elektraKeyToMetaClose,
				KEY_END),
#include "./readme_keytometa.c"
			keyNew ("system:/elektra/modules/keytometa/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system:/elektra/modules/keytometa/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);

// clang-format on

#endif /* CONTRACT_H_ */
