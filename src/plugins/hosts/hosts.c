/***************************************************************************
            hosts.c  -  Access the /etc/hosts file
                             -------------------
    begin                : Nov 2007
    copyright            : (C) 2007 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libelektra.so a valid backend.                             *
 *   Simple fill the empty _hosts functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "hosts.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <kdbextension.h>

size_t elektraStrLen(const char *s);


/** Appends a comment to key found in line.
 *
 * Comments are marked with number signs, also called pound sign (#).
 *
 * White spaces are space and tab.
 * Lines can not start with blank or tab.
 * Blank lines are allowed and preserved in oomments
 * Trailing white spaces are allowed.
 * Only one host entry per line is allowed.
 *
 *
 *
 * @param line Will be closed with \0 where the comment begins
 * @return 0 when the caller should go on
 * @return 1 when the caller should continue (with next iteration)
 */
int elektraHostsAppendComment (char *comment, char *line)
{
	size_t i;
	size_t s = elektraStrLen(line);
	size_t c = elektraStrLen(comment);
	char *endline;

	if (line[0] == '\n')
	{
		strncat (comment, "\n", HOSTS_KDB_BUFFER_SIZE-c-1);
		return 1; /* Empty line, so go on.. */
	}

	if (line[0] == '#')
	{
		strncat (comment, line+1, HOSTS_KDB_BUFFER_SIZE-c-2);
		return 1; /* Complete line is comment, so go on.. */
	}


	for (i=0; i<s; i++)
	{
		if (line [i] == '#') /* comment found */
		{
			/* Remove the endline, might be not there
			 * if size limit of fgets hit or at end of file */
			endline = strrchr (line, '\n');
			if (endline) *endline = '\0';

			/* Copy the comment */
			strncat (comment, line+i+1, HOSTS_KDB_BUFFER_SIZE-c-s-2);
			line[i] = '\0';
			return 0; /* There should be a key here */
		}
	}
	return 0; /* No # found */
}

/**Finds a token (which might be ip, canonical name or an alias).
 *
 * Token will be a pointer to the null terminated string containing
 * the token.
 *
 * @return the size that next time the caller have to jump (line+s)
 * in order to find the next token.
 * @return 0 if no more token is available
 */
size_t elektraHostsFindToken (char **token, char *line)
{
	size_t i = 0;

	/* Step 1, skip whitespaces */
	while (line[i] == ' ' || line[i] == '\t') i++;
	if (line[i] == '\0' || line[i] == '\n') return 0; /* We found the end of the line */

	/* Step 2, parse the token */
	*token = &line[i];
	while (line[i] != ' ' && line[i] != '\t' && line[i] != '\0' && line[i] != '\n') i++;
	if (line[i] == '\0' || line[i] == '\n')
	{
		line[i] = '\0'; /* Terminate the token. */
		return i; /* elektraHostsFindToken will quit next time in Step 1 */
	}

	/* Step 3, terminate the token */
	line[i] = '\0';
	return i+1; /* let elektraHostsFindToken continue next time one byte after termination */
}

void elektraHostsSetMeta(Key *key, int order)
{
	char buffer[MAX_ORDER_SIZE];
	snprintf (buffer, MAX_ORDER_SIZE, "%d", order);
	keySetMeta(key, "order", buffer);
}

int elektraHostsGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
	FILE * fp;
	char readbuffer [HOSTS_KDB_BUFFER_SIZE];
	char *fieldbuffer;
	size_t readsize;
	char *fret;
	int   sret;
	Key *key, *alias, *tmp;
	char comment [HOSTS_KDB_BUFFER_SIZE] = "";
	KeySet *append = 0;
	size_t order = 1;

	if (!strcmp (keyName(parentKey), "system/elektra/modules/hosts"))
	{
		KeySet *moduleConfig = ksNew (30,
			keyNew ("system/elektra/modules/hosts",
				KEY_VALUE, "hosts plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/hosts/exports", KEY_END),
			keyNew ("system/elektra/modules/hosts/exports/get",
				KEY_FUNC, elektraHostsGet,
				KEY_END),
			keyNew ("system/elektra/modules/hosts/exports/set",
				KEY_FUNC, elektraHostsSet,
				KEY_END),
			keyNew ("system/elektra/modules/hosts/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/description",
				KEY_VALUE,
"This plugin reads and writes /etc/hosts files.\n"
"\n"
"The /etc/hosts file is a simple text file that associates IP addresses\n"
"with hostnames, one line per IP address. The format is described in hosts(5).\n"
"\n"
"Canonical hostnames are stored as key names with the IP address as key\n"
"value. Aliases are stored as sub keys with a read only duplicate of the\n"
"associated ip address as value. Comments are stored using meta keys of\n"
"type \"comment\" with the '#'-char stripped of.\n"
"\n"
"== Example ==\n"
"Mount the plugin:\n"
"$ kdb mount /etc/hosts system/hosts hosts\n"
"\n"
"Print out all known hosts and their aliases:\n"
"$ kdb ls system/hosts\n"
"\n"
"Get IP address of host \"localhost\":\n"
"$ kdb get system/hosts/localhost\n"
"\n"
"Fetch comment belonging to host \"localhost\":\n"
"$ kdb getmeta system/hosts/localhost comment\n"
"\n"
"== Multiline comments ==\n"
"Since line breaks are preserved, you can identify multi line comments\n"
"by their trailing line break.\n"
"\n"
"== Ordering ==\n"
"The ordering of the hosts is stored in meta keys of type \"order\".\n"
"The value is an ascending number. Ordering of aliases is NOT preserved."
				, KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/recommends",
				KEY_VALUE, "glob network", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			keyNew ("system/elektra/modules/hosts/config", KEY_END),
			keyNew ("system/elektra/modules/hosts/config/needs", KEY_END),
			keyNew ("system/elektra/modules/hosts/config/needs/glob/set/#1",
				KEY_VALUE, "/*",
				KEY_META, "check/ipaddr", "", /* Preferred way to check */
				KEY_META, "validation/regex", "^[0-9.:]+$", /* Can be checked additionally */
				KEY_META, "validation/message", "Character present not suitable for ip address",
				KEY_END),
			keyNew ("system/elektra/modules/hosts/config/needs/glob/set/#2",
				KEY_VALUE, "/*/*",
				KEY_META, "validation/regex", "^[0-9a-zA-Z.:]+$", /* Only basic character validation */
				KEY_META, "validation/message", "Character present not suitable for host address",
				KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	fp = fopen (keyValue(parentKey), "r");

	if (fp == 0)
	{
		errno = errnosave;
		return 0;
	}

	ksClear (returned);
	append = ksNew(ksGetSize(returned)*2, KS_END);

	key = keyDup (parentKey);
	ksAppendKey(append, key);

	while (1)
	{
		fret = fgets (readbuffer, HOSTS_KDB_BUFFER_SIZE, fp);
		if (fret == 0) 
		{
			fclose (fp);

			ksClear (returned);
			ksAppend (returned, append);
			ksDel (append);
			errno = errnosave;
			return 1;
		}

		if (elektraHostsAppendComment(comment, readbuffer)) continue;

		sret = elektraHostsFindToken (&fieldbuffer, readbuffer);
		if (sret == 0) continue;

		key = ksLookupByName(returned, fieldbuffer, KDB_O_POP);
		if (!key) key = keyDup (parentKey);
		keySetString (key, fieldbuffer);
		keySetComment (key, comment);
		*comment = '\0'; /* Start with a new comment */

		readsize = sret;
		sret = elektraHostsFindToken (&fieldbuffer, readbuffer+readsize);
		keyAddBaseName (key, fieldbuffer);

		elektraHostsSetMeta(key, order);
		++ order; /* Next key gets next number */

		ksAppendKey(append, key);

		ssize_t nr_alias = 0;
		while (1) /*Read in aliases*/
		{
			readsize += sret;
			sret = elektraHostsFindToken (&fieldbuffer, readbuffer+readsize);
			if (sret == 0) break;

			tmp = keyDup (key);
			keyAddBaseName (tmp, fieldbuffer);
			alias = ksLookup(returned, tmp, KDB_O_POP);
			if (!alias) alias = tmp;
			else keyDel (tmp);

			ksAppendKey(append, alias);
			++ nr_alias;
		}
	}

	ELEKTRA_SET_ERROR(10, parentKey, readbuffer);
	ksDel (append);
	// kdbbUnlock (fp);
	fclose (fp);
	errno = errnosave;
	return -1;
}

int elektraHostsSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
	FILE *fp;
	Key *key, *alias=0;
	char * lastline;

	fp = fopen (keyValue(parentKey), "w");

	if (fp == 0)
	{
		ELEKTRA_SET_ERROR(9, parentKey, strerror(errno));
		errno = errnosave;
		return -1;
	}

	Key **keyarray;
	size_t retsize = ksGetSize(returned);
	size_t keyarraysize = retsize *2 +2;
	keyarray = calloc (keyarraysize, sizeof (Key*));
	size_t keyarrayend = retsize +1;

	ksRewind (returned);
	while ((key = ksNext (returned)) != 0)
	{
		/* Only accept keys direct below parentKey */
		if (keyRel (parentKey, key) != 1) continue;

		const Key *orderkey = keyGetMeta (key, "order");
		int order = 0;
		if (orderkey) order = atoi (keyString(orderkey));
		if (order <= 0 || (size_t)order > retsize)
		{
			/* Append to the end */
			keyarray[keyarrayend] = key;
			++ keyarrayend;
		} else {
			keyarray[order] = key;
			++ order;
		}
	}

	for (size_t i=0; i< keyarraysize; ++i)
	{
		key = keyarray[i];
		if (!key) continue;
		lastline = strrchr (keyComment(key), '\n');
		if (lastline)
		{
			*lastline = '\0';
			char *token, *saveptr, *mcomment = malloc (keyGetCommentSize(key));
			strcpy (mcomment, keyComment(key));
			token = strtok_r (mcomment, "\n", &saveptr);
			while (token != 0)
			{
				fprintf (fp, "#%s\n", token);
				token = strtok_r (NULL, "\n", &saveptr);
			}
			free (mcomment);
			*lastline = '\n'; /* preserve comment */
		}

		fprintf (fp, "%s\t%s", (char*)keyValue(key), (char*)keyBaseName (key));

		ksLookup(returned, key, 0);
		while ((alias = ksNext (returned)) != 0)
		{
			if (keyRel (key, alias) < 1) break;

			fprintf (fp, " %s", (char*)keyBaseName(alias));
		}

		if (lastline)
		{
			if (*(lastline+1) != '\0') fprintf (fp, " #%s", lastline+1);
		} else {
			if (*keyComment(key) != '\0') fprintf (fp, " #%s", keyComment(key));
		}

		fprintf (fp, "\n");
	}

	fclose (fp);
	errno = errnosave;
	free (keyarray);
	return 1;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(hosts)
{
	return elektraPluginExport("hosts",
		ELEKTRA_PLUGIN_GET,	&elektraHostsGet,
		ELEKTRA_PLUGIN_SET,	&elektraHostsSet,
		ELEKTRA_PLUGIN_END);
}

