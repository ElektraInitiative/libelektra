/*
 * contract.h
 *
 *  Created on: 10 May 2014
 *      Author: felixl
 */

#ifndef CONTRACT_H_
#define CONTRACT_H_

// @formatter:off

ksNew (30,
		keyNew ("system/elektra/modules/augeas",
				KEY_VALUE, "Augeas plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/augeas/exports", KEY_END),
		keyNew ("system/elektra/modules/augeas/exports/get",
				KEY_FUNC, elektraAugeasGet,
				KEY_END),
		keyNew ("system/elektra/modules/augeas/exports/set",
				KEY_FUNC, elektraAugeasSet,
				KEY_END),
		keyNew ("system/elektra/modules/augeas/exports/open",
				KEY_FUNC, elektraAugeasOpen,
				KEY_END),
		keyNew ("system/elektra/modules/augeas/exports/close",
				KEY_FUNC, elektraAugeasClose,
				KEY_END),
		keyNew ("system/elektra/modules/augeas/infos",
				KEY_VALUE, "All information you want to know",
				KEY_END),
		keyNew ("system/elektra/modules/augeas/infos/author",
				KEY_VALUE, "Felix Berlakovich <elektra@berlakovich.net>",
				KEY_END),
		keyNew ("system/elektra/modules/augeas/infos/licence",
				KEY_VALUE, "BSD",
				KEY_END),
		keyNew ("system/elektra/modules/augeas/infos/description",
				KEY_VALUE,
				"== INTRODUCTION == \n"
				"\n"
				"This is a plugin for reading and writing configuration files with help from Augeas. \n"
				"The plugin should be able to read all configuration files for which an Augeas lens exists. \n"
				"However, not all stock lenses of Augeas have been tested yet. \n"
				"A detailed description of the lens langauge and a tutorial on how to write new lenses"
				"can be found at http://augeas.net/ \n"
				"\n"
				"\n"
				"== INSTALLATION == \n"
				"\n"
				"If you have installed Augeas manually, it may be neccessary to update the ld configuration. This is especially \n"
				"true if an older version of Augeas is installed also. Such a situation may lead to an error similar to this: \n"
				"\n"
				"/usr/lib/libaugeas.so.0: version `AUGEAS_0.16.0' not found (required by kdb) \n"
				"\n"
				"This is because ld tries to link /usr/lib/libaugeas.so.0 which is an older version of Augeas. Simply add \n"
				"the path to the newer library to your ld search paths (consult your system documentation on how to do this)\n"
				"\n"
				"\n"
				"== MOUNTING AND CONFIGURATION == \n"
				"\n"
				"The plugin can be mounted via the mount command like any other plugin. \n"
				"For example, in order to mount the hosts file with the augeas plugin, issue the following command: \n"
				"\n"
				"kdb mount /etc/hosts system/hosts augeas \n"
				"\n"
				"However, additional configuration is needed. Without configuring a lens the plugin will bail out an error: \n"
				"\n"
				"kdb ls system/hosts \n"
				"The command ls terminated unsuccessfully with the info: Error (#85) occurred! \n"
				"Description: an Augeas error occurred \n"
				"Ingroup: plugin \n"
				"Module: storage \n"
				"At: /path/augeas.c:166 \n"
				"Reason: Lens not found\n"
				"\n"
				"This is because the plugin does not know yet which lens to use to read the configuration. \n"
				"A lens can be configured by setting the config/lens key in the mountpoint configuration: \n"
				"\n"
				"kdb mount \n"
				"... output omitted ... \n"
				"/etc/hosts on system/hosts with name system_hosts \n"
				"\n"
				"kdb set system/elektra/mountpoints/system_hosts/config/lens Hosts.lns \n"
				"\n"
				"The value of this key should be the module name of the lens (Hosts in the example) with a '.lns' suffix. \n"
				"Depending on your distribution and kind of installation, lenses can be found at /usr/share/augeas/lenses/dist, \n"
				"/usr/local/share/augeas/lenses/dist, or something similar. \n"
				"The lens module name is equal to the filename without extension in pascal notation. \n"
				"For example, the lens /usr/share/augeas/lenses/dist/hosts.aug contains the module Hosts. \n"
				"\n"
				"\n"
				"== RESTRICTIONS == \n"
				"\n"
				"=== Inner node values === \n"
				"Currently no Augeas lens supports values for inner nodes. "
				"Unfortunately no validation plugin exists yet that would prevent such modifications early: \n"
				"\n"
				"kdb set system/hosts/1 somevalue \n"
				"The command set terminated unsuccessfully with the info: Error (#85) occurred! \n"
				"Description: an Augeas error occurred \n"
				"Ingroup: plugin \n"
				"Module: storage \n"
				"At: /path/augeas.c:166 \n"
				"Reason: Malformed child node '1' \n"
				"\n"
				"The operation simply fails with an undescriptive error.\n"
				"\n"
				"=== Leaky abstraction of order ===\n"
				"Most Augeas lenses require subtrees to be in a specific order. For example the hosts lens requires the ipaddr node \n"
				"of an entry to precede the canonical node. Unfortunately the Augeas storage plugin has no knowledge about this required \n"
				"order. Therefore the correct order must be ensured via order meta keys. Otherwise saving the KeySet may fail. As an example \n"
				"consider the following kdb shell script: \n"
				"\n"
				"kdbGet system/hosts \n"
				"keySetName system/hosts/6 \n"
				"ksAppendKey \n"
				"keySetName system/hosts/6/ipaddr \n"
				"keySetString 14.14.14.14 \n"
				"ksAppendKey \n"
				"keySetName system/hosts/6/canonical \n"
				"keySetString newhost \n"
				"ksAppendKey \n"
				"kdbSet system/hosts \n"
				"\n"
				"This fails with an error similar to this \n"
				"Description: an Augeas error occurred \n"
				"Ingroup: plugin \n"
				"Module: storage \n"
				"At: /path/augeas.c:179 \n"
				"Reason: Failed to match \n"
				"some augeas match expression \n"
				"with tree \n"
				"{ \"canonical\" = \"newhost\" } { \"ipaddr\" = \"14.14.14.14\" } \n"
				"\n"
				"Wheras the following script succeeds due to the correct order\n"
				"\n"
				"kdbGet system/hosts \n"
				"keySetName system/hosts/6 \n"
				"ksAppendKey \n"
				"keySetName system/hosts/6/ipaddr \n"
				"keySetString 14.14.14.14 \n"
				"keySetMeta order 100 \n"
				"ksAppendKey \n"
				"keySetName system/hosts/6/canonical \n"
				"keySetString newhost \n"
				"keySetMeta order 110 \n"
				"ksAppendKey \n"
				"kdbSet system/hosts \n"
				"\n"
				"\n"
				"== PLANNED IMPROVEMENTS ==\n"
				"\n"
				"* simplified mounting and configuration \n"
				"* a validation plugin preventing inner node values \n",
				KEY_END),
		keyNew ("system/elektra/modules/augeas/infos/provides",
				KEY_VALUE, "storage",
				KEY_END),
		keyNew ("system/elektra/modules/augeas/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/augeas/infos/placements",
				KEY_VALUE, "getstorage setstorage",
				KEY_END),
		keyNew ("system/elektra/modules/augeas/infos/recommends",
				KEY_VALUE, "glob keytometa",
				KEY_END),
		keyNew ("system/elektra/modules/augeas/infos/version",
				KEY_VALUE, PLUGINVERSION,
				KEY_END),
		keyNew ("system/elektra/modules/augeas/config", KEY_END),
		keyNew ("system/elektra/modules/augeas/config/needs", KEY_END),
		keyNew ("system/elektra/modules/augeas/config/needs/glob/get/#1",
			KEY_VALUE, "*#comment*",
			KEY_META, "convert/metaname", "comment", /* comment keys are converted to comments */
			KEY_META, "convert/append", "next", /* usually comments belong to the following key */
			KEY_END),
		keyNew ("system/elektra/modules/augeas/config/needs/glob/get/#1/flags",
			KEY_VALUE, "0", /* disable the path matching mode */
			KEY_END),
		KS_END);

// @formatter:on

#endif /* CONTRACT_H_ */
