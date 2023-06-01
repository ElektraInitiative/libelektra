/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CONTRACT_HOSTS_H_
#define CONTRACT_HOSTS_H_

// clang-format off


ksNew (30,
		keyNew ("system:/elektra/modules/hosts",
			KEY_VALUE, "hosts plugin waits for your orders", KEY_END),
		keyNew ("system:/elektra/modules/hosts/exports", KEY_END),
		keyNew ("system:/elektra/modules/hosts/exports/get",
			KEY_FUNC, elektraHostsGet,
			KEY_END),
		keyNew ("system:/elektra/modules/hosts/exports/set",
			KEY_FUNC, elektraHostsSet,
			KEY_END),
#include "./readme_hosts.c"
		keyNew ("system:/elektra/modules/hosts/infos/version",
			KEY_VALUE, PLUGINVERSION,
			KEY_END),
		keyNew ("system:/elektra/modules/hosts/config", KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs", KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#0",
			KEY_VALUE, "/ipv4/*",
			KEY_META, "check/ipaddr", "ipv4", 				/* Preferred way to check */
			KEY_META, "check/validation", "^[0-9.]+$", /* Can be checked additionally */
			KEY_META, "check/validation/match", "LINE",
			KEY_META, "check/validation/message", "Character present not suitable for ipv4 address",
			KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#1",
			KEY_VALUE, "/ipv6/*",
			KEY_META, "check/ipaddr", "ipv6", 				/* Preferred way to check */
			KEY_META, "check/validation", "^[0-9A-Fa-f.:]+$", /* Can be checked additionally */
			KEY_META, "check/validation/match", "LINE",
			KEY_META, "check/validation/message", "Character present not suitable for ipv6 address",
			KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#2",
				KEY_VALUE, "/ipv4/*/*",
				KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#3",
				KEY_VALUE, "/ipv6/*/*",
				KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#4",
			KEY_VALUE, "/*",
			KEY_META, "trigger/error", "162",
			KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#4/flags",
			KEY_VALUE, "", /* disable the path matching mode */
			KEY_END),
		KS_END);

// clang-format on

#endif /* CONTRACT_HOSTS_H_ */
