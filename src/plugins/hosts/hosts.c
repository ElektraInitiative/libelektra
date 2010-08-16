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
		strncat (comment, "\n", HOSTS_BUFFER_SIZE-c-1);
		return 1; /* Empty line, so go on.. */
	}

	if (line[0] == '#')
	{
		strncat (comment, line, HOSTS_BUFFER_SIZE-c-2);
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
			strncat (comment, line+i+1, HOSTS_BUFFER_SIZE-c-s-2);
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

int elektraHostsGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
	FILE * fp;
	char readbuffer [HOSTS_BUFFER_SIZE];
	char *fieldbuffer;
	size_t readsize;
	char *fret;
	int   sret;
	Key *key, *alias, *tmp;
	char comment [HOSTS_BUFFER_SIZE] = "";
	KeySet *append = 0;

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
				KEY_VALUE, "/etc/hosts file", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/hosts/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
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
		fret = fgets (readbuffer, HOSTS_BUFFER_SIZE, fp);
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
		keySetMode(key, 0664);
		keySetString (key, fieldbuffer);
		keySetComment (key, comment);
		*comment = '\0'; /* Start with a new comment */

		readsize = sret;
		sret = elektraHostsFindToken (&fieldbuffer, readbuffer+readsize);

		keyAddBaseName (key, fieldbuffer);
		ksAppendKey(append, key);
		clear_bit (key->flags, KEY_FLAG_SYNC);

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
			nr_alias ++;
		}
	}

	ELEKTRA_SET_ERROR(10, parentKey, readbuffer);
	ksDel (append);
	// kdbbUnlock (fp);
	fclose (fp);
	errno = errnosave;
	return -1;
}

int elektraHostsSet(Plugin *handle, KeySet *returned, Key *parentKey)
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

	ksRewind (returned);
	key = ksNext (returned); /* skip parentKey */

	while ((key = ksNext (returned)) != 0)
	{
		lastline = strrchr (keyComment(key), '\n');
		if (lastline)
		{
			*lastline = '\0';
			fprintf (fp, "%s\n", keyComment(key));
			*lastline = '\n'; /* preserve comment */
		}

		fprintf (fp, "%s\t%s", (char*)keyValue(key), (char*)keyBaseName (key));

		cursor_t restore = ksGetCursor (returned);
		while ((alias = ksNext (returned)) != 0)
		{
			if (keyRel (key, alias) < 1) break;
			restore = ksGetCursor (returned);

			fprintf (fp, "\t%s", (char*)keyBaseName(alias));
		}
		ksSetCursor (returned, restore);
		/* Now we know how many aliases we have...
		   So go over them. */

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
	return 1;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(hosts)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_GET,	&elektraHostsGet,
		ELEKTRA_PLUGIN_SET,	&elektraHostsSet,
		ELEKTRA_PLUGIN_END);
}

