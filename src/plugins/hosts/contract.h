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
			ELEKTRA_KEY_VALUE, "hosts plugin waits for your orders", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/exports", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/exports/get",
			ELEKTRA_KEY_FUNC, elektraHostsGet,
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/exports/set",
			ELEKTRA_KEY_FUNC, elektraHostsSet,
			ELEKTRA_KEY_END),
#include "readme_hosts.c"
		keyNew ("system:/elektra/modules/hosts/infos/version",
			ELEKTRA_KEY_VALUE, PLUGINVERSION,
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/config", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#0",
			ELEKTRA_KEY_VALUE, "/ipv4/*",
			ELEKTRA_KEY_META, "check/ipaddr", "ipv4", 				/* Preferred way to check */
			ELEKTRA_KEY_META, "check/validation", "^[0-9.]+$", /* Can be checked additionally */
			ELEKTRA_KEY_META, "check/validation/match", "LINE",
			ELEKTRA_KEY_META, "check/validation/message", "Character present not suitable for ipv4 address",
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#1",
			ELEKTRA_KEY_VALUE, "/ipv6/*",
			ELEKTRA_KEY_META, "check/ipaddr", "ipv6", 				/* Preferred way to check */
			ELEKTRA_KEY_META, "check/validation", "^[0-9A-Fa-f.:]+$", /* Can be checked additionally */
			ELEKTRA_KEY_META, "check/validation/match", "LINE",
			ELEKTRA_KEY_META, "check/validation/message", "Character present not suitable for ipv6 address",
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#2",
				ELEKTRA_KEY_VALUE, "/ipv4/*/*",
				ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#3",
				ELEKTRA_KEY_VALUE, "/ipv6/*/*",
				ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#4",
			ELEKTRA_KEY_VALUE, "/*",
			ELEKTRA_KEY_META, "trigger/error", "162",
			ELEKTRA_KEY_END),
		keyNew ("system:/elektra/modules/hosts/config/needs/glob/set/#4/flags",
			ELEKTRA_KEY_VALUE, "", /* disable the path matching mode */
			ELEKTRA_KEY_END),
		ELEKTRA_KS_END);

// clang-format on

#endif /* CONTRACT_HOSTS_H_ */
