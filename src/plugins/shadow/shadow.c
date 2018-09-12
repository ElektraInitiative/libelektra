/**
 * @file
 *
 * @brief Source for shadow plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * some of the code is inspired by / taken from musl
 */



#include "shadow.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdberrors.h>
#include <kdbhelper.h>

#include "../cpasswd/getline.c"

static long xatol (char ** s)
{
	long x;
	if (**s == ':' || **s == '\n') return -1;
	for (x = 0; (unsigned) (**s - '0') < 10U; ++*s)
		x = 10 * x + (**s - '0');
	return x;
}
static struct spwd * strToShadow (char * line)
{
	struct spwd * sp = elektraMalloc (sizeof (struct spwd));
	char * s = line;
	sp->sp_namp = s;
	if (!(s = strchr (s, ':'))) goto fail;
	*s = 0;
	sp->sp_pwdp = ++s;
	if (!(s = strchr (s, ':'))) goto fail;
	*s = 0;
	s++;
	sp->sp_lstchg = xatol (&s);
	if (*s != ':') goto fail;
	s++;
	sp->sp_min = xatol (&s);
	if (*s != ':') goto fail;
	s++;
	sp->sp_max = xatol (&s);
	if (*s != ':') goto fail;
	s++;
	sp->sp_warn = xatol (&s);
	if (*s != ':') goto fail;
	s++;
	sp->sp_inact = xatol (&s);
	if (*s != ':') goto fail;
	s++;
	sp->sp_expire = xatol (&s);
	if (*s != ':') goto fail;
	s++;
	sp->sp_flag = xatol (&s);
	if (*s != '\n') goto fail;
	return sp;
fail:
	elektraFree (sp);
	return NULL;
}

static KeySet * spentToKS (struct spwd * sp, Key * parentKey)
{
	const char * keys[SHADOW_FIELDS] = { "login", "password", "ltschg", "min", "max", "warn", "inact", "expire", "flag" };
	void * fields[SHADOW_FIELDS] = {
		sp->sp_namp,  sp->sp_pwdp,   &sp->sp_lstchg, &sp->sp_min,  &sp->sp_max,
		&sp->sp_warn, &sp->sp_inact, &sp->sp_expire, &sp->sp_flag,
	};
	KeySet * ks = ksNew (0, KS_END);
	Key * append = keyNew (keyName (parentKey), KEY_END);
	keyAddBaseName (append, sp->sp_namp);
	ksAppendKey (ks, keyDup (append));
	for (int i = 0; i < SHADOW_FIELDS; ++i)
	{
		keyAddBaseName (append, keys[i]);
		if (i < 2)
			keySetString (append, fields[i]);
		else
		{
			char longBuf[11];
			snprintf (longBuf, sizeof (longBuf), "%ld", *(long *) fields[i]);
			keySetString (append, longBuf);
		}
		ksAppendKey (ks, keyDup (append));
		keySetBaseName (append, 0);
	}
	keyDel (append);
	return ks;
}


int elektraShadowGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/shadow"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/shadow", KEY_VALUE, "shadow plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/shadow/exports", KEY_END),
			       keyNew ("system/elektra/modules/shadow/exports/get", KEY_FUNC, elektraShadowGet, KEY_END),
			       keyNew ("system/elektra/modules/shadow/exports/set", KEY_FUNC, elektraShadowSet, KEY_END),
#include ELEKTRA_README (shadow)
			       keyNew ("system/elektra/modules/shadow/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	FILE * f = fopen (keyString (parentKey), "rbe");
	if (!f)
	{
		ELEKTRA_SET_ERRORF (110, parentKey, "Failed to open configuration file %s\n", keyString (parentKey));
		return -1;
	}
	char * line = NULL;
	size_t len = 0;
	ssize_t l = 0;
	struct spwd * sp;
	size_t lineno = 0;
	while ((l = __getline (&line, &len, f)) != -1)
	{
		sp = strToShadow (line);
		if (!sp)
		{
			ELEKTRA_ADD_WARNINGF (201, parentKey, "Failed to parse line '%s' of shadow file, skipping to next line\n", line);
			++lineno;
			continue;
		}
		++lineno;
		KeySet * ks = spentToKS (sp, parentKey);
		ksAppend (returned, ks);
		ksRewind (returned);
		ksDel (ks);
		elektraFree (sp);
	}
	elektraFree (line);
	fclose (f);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}


static int writeKS (FILE * f, const Key * key, KeySet * ks)
{
	const char * keys[SHADOW_FIELDS] = { "login", "password", "ltschg", "min", "max", "warn", "inact", "expire", "flag" };
	struct spwd * sp = elektraMalloc (sizeof (struct spwd));
	void * fields[SHADOW_FIELDS] = {
		&sp->sp_namp, &sp->sp_pwdp,  &sp->sp_lstchg, &sp->sp_min,  &sp->sp_max,
		&sp->sp_warn, &sp->sp_inact, &sp->sp_expire, &sp->sp_flag,
	};
	Key * searchKey = keyNew (keyName (key), KEY_END);
	int rv = 1;
	for (int i = 0; i < SHADOW_FIELDS; ++i)
	{
		keyAddBaseName (searchKey, keys[i]);
		Key * lookup = ksLookup (ks, searchKey, KDB_O_NONE);
		if (i < 2)
			*(char **) (fields[i]) = (char *) keyString (lookup);
		else
		{
			if (!strcmp (keyString (lookup), "-1"))
				*(long *) (fields[i]) = -1;
			else
				*(long *) (fields[i]) = atol (keyString (lookup));
		}
		keySetBaseName (searchKey, 0);
	}
	rv = fprintf (f, "%s:%s:%.*ld:%.*ld:%.*ld:%.*ld:%.*ld:%.*ld:%.*ld\n", STR (sp->sp_namp), STR (sp->sp_pwdp), NUM (sp->sp_lstchg),
		      NUM (sp->sp_min), NUM (sp->sp_max), NUM (sp->sp_warn), NUM (sp->sp_inact), NUM (sp->sp_expire),
		      NUM ((long) sp->sp_flag));
	if (rv < 0)
		rv = -1;
	else if (rv > 0)
		rv = 1;
	keyDel (searchKey);
	elektraFree (sp);
	return rv;
}


int elektraShadowSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	Key * cur;
	ksRewind (returned);
	FILE * f = fopen (keyString (parentKey), "w");
	if (!f)
	{
		ELEKTRA_SET_ERRORF (75, parentKey, "Failed to open %s for writing\n", keyString (parentKey));
		return -1;
	}
	int rv = 1;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyIsDirectBelow (parentKey, cur) != 1) continue;
		rv = writeKS (f, cur, returned);
		if (rv == -1) break;
	}
	fclose (f);
	return rv;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (shadow)
{
	// clang-format off
    return elektraPluginExport ("shadow",
            ELEKTRA_PLUGIN_GET,	&elektraShadowGet,
            ELEKTRA_PLUGIN_SET, &elektraShadowSet,
            ELEKTRA_PLUGIN_END);
}
