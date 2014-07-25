/*
 * contract.h
 *
 *  Created on: 25 Jul 2014
 *      Author: felixl
 */

#ifndef CONTRACT_H_
#define CONTRACT_H_

// @formatter:off

ksNew (30,
			keyNew ("system/elektra/modules/keytometa",
				KEY_VALUE, "keytometa plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/keytometa/exports", KEY_END),
			keyNew ("system/elektra/modules/keytometa/exports/get",
				KEY_FUNC, elektraKeyToMetaGet,
				KEY_END),
			keyNew ("system/elektra/modules/keytometa/exports/set",
				KEY_FUNC, elektraKeyToMetaSet,
				KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/author",
				KEY_VALUE, "Felix Berlakovich <elektra@berlakovich.net>", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/description",
				KEY_VALUE, "Converts keys to meta keys and vice versa", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/placements",
				KEY_VALUE, "presetstorage postgetstorage", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/provides",
				KEY_VALUE, "conversion", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);

// @formatter:on

#endif /* CONTRACT_H_ */
