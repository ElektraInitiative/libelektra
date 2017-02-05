/**
 * @file
 *
 * @brief Source for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <sys/types.h>
#include <regex.h>
#include <kdberrors.h>

#include "ipaddr.h"

#include <kdbhelper.h>


int elektraIpaddrOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return 1; // success
}

int elektraIpaddrClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return 1; // success
}

static int validateIPv4(const char *addr)
{
    if(!addr)
	return 0;
    unsigned int a, b, c, d;
    a = b = c = d = 0;
    const char *regexString = "^([0-9]{1,3}\\.){3}([0-9]{1,3})$";
    regex_t regex;
    regmatch_t offsets;
    int ret = regcomp(&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
    if(ret)
	return -1;
    ret = regexec(&regex, addr, 1, &offsets, 0);
    regfree(&regex);
    if(!ret)
    {
	sscanf(addr, "%u.%u.%u.%u", &a, &b, &c, &d);
	if(a > 255 || b > 255 || c > 255 || d > 255)
	{
	    return 0;
	}
	else
	    return 1;
    }
    return 0;

}

static int validateIPv6(const char *addr)
{
    if(!addr)
	return 0;
    const char *regexString = "(^((:(([0-9A-Fa-f]{0,4}):){1,6}(([0-9A-Fa-f]{1,4})))|(([0-9A-Fa-f]{1,4})(:([0-9A-Fa-f]{0,4})){1,7}))$)|(^((:(([0-9A-Fa-f]{0,4}):){1,4}(([0-9A-Fa-f]{1,4})))|(([0-9A-Fa-f]{1,4})(:([0-9A-Fa-f]{0,4})){1,5}))((([0-9]{1,3}\\.){3})([0-9]{1,3}))$)";
    regex_t regex;
    regmatch_t offsets;
    int ret = regcomp(&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
    if(ret)
	return -1;
    ret = regexec(&regex, addr, 1, &offsets, 0);
    regfree(&regex);
    if(ret)
	return 0;
    else
    {
	char *ptr = (char *)addr;
	int count = 0;
	while(*ptr)
	{
	    if(*ptr == ':')
		++count;
	    ++ptr;
	}
	if(count > 7)
	{
	    return 0;
	}
	else if(count < 7)
	{
	    if(!strstr(addr, "::"))
		return 0;
	}
	if(strchr(addr, '.'))
	{
	    char *ipv4ptr = strrchr(addr, ':');
	    ++ipv4ptr;
	    ret = validateIPv4(ipv4ptr);
	    if(!ret)
		return 0;
	}
	return 1;
    }
}

static int validateKey(Key *key, Key *parentKey)
{
	const Key *meta = keyGetMeta(key, "check/ipaddr");
	if(!meta)
	    return 1;
	int rc = 0;
	if(!strcasecmp(keyString(meta), "ipv4"))
	    rc = validateIPv4(keyString(key));
	else if(!strcasecmp(keyString(meta), "ipv6"))
	    rc = validateIPv6(keyString(key));
	else 
	    rc = 1;
	if(!rc)
	{
	    ELEKTRA_SET_ERRORF(51, parentKey, "Validation of key %s with value %s failed.", keyName(key), keyString(key)); 
	}
	else if(rc == -1)
	{
	    ELEKTRA_SET_ERROR(87, parentKey, "Out of memory");
	    rc = 0;
	}

	return rc;
}

int elektraIpaddrGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/ipaddr"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/ipaddr", KEY_VALUE, "ipaddr plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/ipaddr/exports", KEY_END),
			       keyNew ("system/elektra/modules/ipaddr/exports/open", KEY_FUNC, elektraIpaddrOpen, KEY_END),
			       keyNew ("system/elektra/modules/ipaddr/exports/close", KEY_FUNC, elektraIpaddrClose, KEY_END),
			       keyNew ("system/elektra/modules/ipaddr/exports/get", KEY_FUNC, elektraIpaddrGet, KEY_END),
			       keyNew ("system/elektra/modules/ipaddr/exports/set", KEY_FUNC, elektraIpaddrSet, KEY_END),
			       keyNew ("system/elektra/modules/ipaddr/exports/error", KEY_FUNC, elektraIpaddrError, KEY_END),
			       keyNew ("system/elektra/modules/ipaddr/exports/checkconf", KEY_FUNC, elektraIpaddrCheckConfig, KEY_END),
#include ELEKTRA_README (ipaddr)
			       keyNew ("system/elektra/modules/ipaddr/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraIpaddrSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key *cur;
	ksRewind(returned);
	while((cur = ksNext(returned)) != NULL)
	{
	    const Key *meta = keyGetMeta(cur, "check/ipaddr");
	    if(!meta)
		continue;
	    int rc = validateKey(cur, parentKey);
	    if(!rc)
		return -1;
	}
	return 1; // success
}

int elektraIpaddrError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return 1; // success
}

int elektraIpaddrCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (ipaddr)
{
	// clang-format off
	return elektraPluginExport ("ipaddr",
		ELEKTRA_PLUGIN_OPEN,	&elektraIpaddrOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraIpaddrClose,
		ELEKTRA_PLUGIN_GET,	&elektraIpaddrGet,
		ELEKTRA_PLUGIN_SET,	&elektraIpaddrSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraIpaddrError,
		ELEKTRA_PLUGIN_END);
}

