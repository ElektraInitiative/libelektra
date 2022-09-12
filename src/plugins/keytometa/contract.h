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

elektraKeysetNew (30,
			elektraKeyNew ("system:/elektra/modules/keytometa",
				ELEKTRA_KEY_VALUE, "keytometa plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/keytometa/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/keytometa/exports/get",
				ELEKTRA_KEY_FUNC, elektraKeyToMetaGet,
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/keytometa/exports/set",
				ELEKTRA_KEY_FUNC, elektraKeyToMetaSet,
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/keytometa/exports/close",
				ELEKTRA_KEY_FUNC, elektraKeyToMetaClose,
				ELEKTRA_KEY_END),
#include "readme_keytometa.c"
			elektraKeyNew ("system:/elektra/modules/keytometa/infos",
				ELEKTRA_KEY_VALUE, "All information you want to know", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/keytometa/infos/version",
				ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
			ELEKTRA_KS_END);

// clang-format on

#endif /* CONTRACT_H_ */
