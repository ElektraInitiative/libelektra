/*
 * contract.h
 *
 *  Created on: 10 May 2014
 *      Author: felixl
 */

#ifndef CONTRACT_H_
#define CONTRACT_H_

ksNew (30,
		keyNew ("system/elektra/modules/augeas",
				KEY_VALUE,
				"Augeas plugin waits for your orders",
				KEY_END),
		keyNew ("system/elektra/modules/augeas/exports",
				KEY_END),
		keyNew (
				"system/elektra/modules/augeas/exports/get",
				KEY_FUNC, elektraAugeasGet, KEY_END),
		keyNew (
				"system/elektra/modules/augeas/exports/set",
				KEY_FUNC, elektraAugeasSet, KEY_END),
		keyNew (
				"system/elektra/modules/augeas/exports/open",
				KEY_FUNC, elektraAugeasOpen, KEY_END),
		keyNew (
				"system/elektra/modules/augeas/exports/close",
				KEY_FUNC, elektraAugeasClose, KEY_END),
		keyNew ("system/elektra/modules/augeas/infos",
				KEY_VALUE,
				"All information you want to know",
				KEY_END),
		keyNew (
				"system/elektra/modules/augeas/infos/author",
				KEY_VALUE,
				"Felix Berlakovich <elektra@berlakovich.net>",
				KEY_END),
		keyNew (
				"system/elektra/modules/augeas/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
		keyNew (
				"system/elektra/modules/augeas/infos/description",
				KEY_VALUE,
				"Reads and writes configurations with libaugeas",
				KEY_END),
		keyNew (
				"system/elektra/modules/augeas/infos/provides",
				KEY_VALUE, "storage", KEY_END),
		keyNew (
				"system/elektra/modules/augeas/infos/placements",
				KEY_VALUE, "getstorage setstorage",
				KEY_END),
		keyNew (
				"system/elektra/modules/augeas/infos/needs",
				KEY_VALUE, "", KEY_END),
		keyNew (
				"system/elektra/modules/augeas/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);

#endif /* CONTRACT_H_ */
