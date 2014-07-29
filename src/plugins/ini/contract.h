/*
 * contract.h
 *
 *  Created on: 10 May 2014
 *      Author: felixl
 */

#ifndef CONTRACT_H_
#define CONTRACT_H_


static inline KeySet *getPluginContract()
{
	// @formatter:off
	return ksNew (30,
			keyNew ("system/elektra/modules/ini",
					KEY_VALUE, "Ini plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/ini/exports", KEY_END),
			keyNew ("system/elektra/modules/ini/exports/get",
					KEY_FUNC, elektraIniGet,
					KEY_END),
			keyNew ("system/elektra/modules/ini/exports/set",
					KEY_FUNC, elektraIniSet,
					KEY_END),
	#include "readme_ini.c"
			keyNew ("system/elektra/modules/ini/infos/version",
					KEY_VALUE, PLUGINVERSION,
					KEY_END),
			KS_END);
	// @formatter:on
}

#endif /* CONTRACT_H_ */
