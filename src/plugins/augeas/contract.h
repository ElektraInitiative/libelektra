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
		elektraKeyNew ("system:/elektra/modules/augeas",
				ELEKTRA_KEY_VALUE, "Augeas plugin waits for your orders", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/exports", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/exports/get",
				ELEKTRA_KEY_FUNC, elektraAugeasGet,
				ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/exports/set",
				ELEKTRA_KEY_FUNC, elektraAugeasSet,
				ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/exports/open",
				ELEKTRA_KEY_FUNC, elektraAugeasOpen,
				ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/exports/genconf",
				ELEKTRA_KEY_FUNC, elektraAugeasGenConf,
				ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/exports/close",
				ELEKTRA_KEY_FUNC, elektraAugeasClose,
				ELEKTRA_KEY_END),
#include "readme_augeas.c"
		elektraKeyNew ("system:/elektra/modules/augeas/infos/version",
				ELEKTRA_KEY_VALUE, PLUGINVERSION,
				ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/config", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/config/needs", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/config/needs/glob/get/#1",
			ELEKTRA_KEY_VALUE, "*#comment*",
			ELEKTRA_KEY_META, "convert/metaname", "comment", /* comment keys are converted to comments */
			ELEKTRA_KEY_META, "convert/append", "next", /* usually comments belong to the following key */
			ELEKTRA_KEY_META, "convert/append/samelevel", "1", /* if the configuration has nested structures, comments should stay in the same hierarchy */
			ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/augeas/config/needs/glob/get/#1/flags",
			ELEKTRA_KEY_VALUE, "", /* disable the path matching mode */
			ELEKTRA_KEY_END),
		ELEKTRA_KS_END);

// clang-format on

#endif /* CONTRACT_H_ */
