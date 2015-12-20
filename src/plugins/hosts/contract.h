/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef CONTRACT_HOSTS_H_
#define CONTRACT_HOSTS_H_

// @formatter:off


ksNew (30,
		keyNew ("system/elektra/modules/hosts",
			KEY_VALUE, "hosts plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/hosts/exports", KEY_END),
		keyNew ("system/elektra/modules/hosts/exports/get",
			KEY_FUNC, elektraHostsGet,
			KEY_END),
		keyNew ("system/elektra/modules/hosts/exports/set",
			KEY_FUNC, elektraHostsSet,
			KEY_END),
#include "readme_hosts.c"
		keyNew ("system/elektra/modules/hosts/infos/version",
			KEY_VALUE, PLUGINVERSION,
			KEY_END),
		keyNew ("system/elektra/modules/hosts/config", KEY_END),
		keyNew ("system/elektra/modules/hosts/config/needs", KEY_END),
		keyNew ("system/elektra/modules/hosts/config/needs/glob/set/#1",
			KEY_VALUE, "/*/*",
			KEY_META, "check/ipaddr", "", 				/* Preferred way to check */
			KEY_META, "validation/regex", "^[0-9.:]+$", /* Can be checked additionally */
			KEY_META, "validation/message", "Character present not suitable for ip address",
			KEY_END),
		keyNew ("system/elektra/modules/hosts/config/needs/glob/set/#2",
			KEY_VALUE, "/*/*",
			KEY_META, "validation/regex", "^[0-9a-zA-Z.:]+$", /* Only basic character validation */
			KEY_META, "validation/message", "Character present not suitable for host address",
			KEY_END),
		KS_END);

// @formatter:on

#endif /* CONTRACT_HOSTS_H_ */
