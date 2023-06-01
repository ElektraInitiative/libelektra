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
		keyNew ("system:/elektra/modules/augeas",
				KEY_VALUE, "Augeas plugin waits for your orders", KEY_END),
		keyNew ("system:/elektra/modules/augeas/exports", KEY_END),
		keyNew ("system:/elektra/modules/augeas/exports/get",
				KEY_FUNC, elektraAugeasGet,
				KEY_END),
		keyNew ("system:/elektra/modules/augeas/exports/set",
				KEY_FUNC, elektraAugeasSet,
				KEY_END),
		keyNew ("system:/elektra/modules/augeas/exports/open",
				KEY_FUNC, elektraAugeasOpen,
				KEY_END),
		keyNew ("system:/elektra/modules/augeas/exports/genconf",
				KEY_FUNC, elektraAugeasGenConf,
				KEY_END),
		keyNew ("system:/elektra/modules/augeas/exports/close",
				KEY_FUNC, elektraAugeasClose,
				KEY_END),
#include "./readme_augeas.c"
		keyNew ("system:/elektra/modules/augeas/infos/version",
				KEY_VALUE, PLUGINVERSION,
				KEY_END),
		keyNew ("system:/elektra/modules/augeas/config", KEY_END),
		keyNew ("system:/elektra/modules/augeas/config/needs", KEY_END),
		keyNew ("system:/elektra/modules/augeas/config/needs/glob/get/#1",
			KEY_VALUE, "*#comment*",
			KEY_META, "convert/metaname", "comment/#0", /* comment keys are converted to comments */
			KEY_META, "convert/append", "next", /* usually comments belong to the following key */
			KEY_META, "convert/append/samelevel", "1", /* if the configuration has nested structures, comments should stay in the same hierarchy */
			KEY_END),
		keyNew ("system:/elektra/modules/augeas/config/needs/glob/get/#1/flags",
			KEY_VALUE, "", /* disable the path matching mode */
			KEY_END),
		KS_END);

// clang-format on

#endif /* CONTRACT_H_ */
