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


int kdbOpen_hosts(Plugin *p)
{
	return 0;
}

int kdbClose_hosts(Plugin *p)
{
	return 0; /* success */
}

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
static int append_comment (char *comment, char *line)
{
	size_t i;
	size_t s = kdbiStrLen(line);
	size_t c = kdbiStrLen(comment);
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
static size_t find_token (char **token, char *line)
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
		return i; /* find_token will quit next time in Step 1 */
	}

	/* Step 3, terminate the token */
	line[i] = '\0';
	return i+1; /* let find_token continue next time one byte after termination */
}

ssize_t kdbGet_hosts(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	int errnosave = errno;
	ssize_t nr_keys = 0, nr_alias;
	FILE * fp;
	char readbuffer [HOSTS_BUFFER_SIZE];
	char *fieldbuffer;
	size_t readsize;
	char aliasname[] = "alias00";
	char *fret;
	int   sret;
	Key *key, *alias, *tmp;
	char comment [HOSTS_BUFFER_SIZE] = "";
	KeySet *append = 0;

	/* if (strcmp (keyName(kdbhGetMountpoint(handle)), keyName(parentKey))) return 0; */

	fp = fopen (keyString(ksLookupByName(pluginGetConfig (handle), "/path", 0)), "r");

	if (fp == 0)
	{
		/*kdbhSetError (handle, Plugin_ERR_NODIR);*/
		errno = errnosave;
		return -1;
	}

	// kdbbReadLock (fp);

	ksClear (returned);
	append = ksNew(ksGetSize(returned)*2, KS_END);

	key = keyDup (parentKey);
	keySetDir (key);
	ksAppendKey(append, key);
	clear_bit (key->flags, KEY_FLAG_SYNC);
	nr_keys ++;

	while (1)
	{
		fret = fgets (readbuffer, HOSTS_BUFFER_SIZE, fp);
		if (fret == 0) 
		{
			/* success */
			// kdbbUnlock(fp);
			fclose (fp);

			ksClear (returned);
			ksAppend (returned, append);
			ksDel (append);
			errno = errnosave;
			return nr_keys;
		}

		if (append_comment(comment, readbuffer)) continue;

		sret = find_token (&fieldbuffer, readbuffer);
		if (sret == 0) continue;

		key = ksLookupByName(returned, fieldbuffer, KDB_O_POP);
		if (!key) key = keyDup (parentKey);
		keySetMode(key, 0664);
		keySetString (key, fieldbuffer);
		keySetComment (key, comment);
		*comment = '\0'; /* Start with a new comment */

		readsize = sret;
		sret = find_token (&fieldbuffer, readbuffer+readsize);

		keyAddBaseName (key, fieldbuffer);
		ksAppendKey(append, key);
		clear_bit (key->flags, KEY_FLAG_SYNC);

		nr_alias = 0;
		while (1) /*Read in aliases*/
		{
			readsize += sret;
			sret = find_token (&fieldbuffer, readbuffer+readsize);
			if (sret == 0) break;

			tmp = keyDup (key);
			aliasname[5] = nr_alias / 10 + '0';
			aliasname[6] = nr_alias % 10 + '0';
			keyAddBaseName (tmp, aliasname);
			alias = ksLookup(returned, tmp, KDB_O_POP);
			if (!alias) alias = tmp;
			else keyDel (tmp);
			keySetMode(alias, 0664);
			keySetString (alias, fieldbuffer);
			keySetComment (alias, "");
			ksAppendKey(append, alias);
			clear_bit (alias->flags, KEY_FLAG_SYNC);
			nr_alias ++;
			if (nr_alias == 1)
			{
				keySetDir (key);
				clear_bit (key->flags, KEY_FLAG_SYNC);
			}
		}
		nr_keys += nr_alias + 1;
	}
#if DEBUG
	printf ("error at line: %s\n", readbuffer);
#endif
	ksDel (append);
	// kdbbUnlock (fp);
	fclose (fp);
	errno = errnosave;
	return -1;
}

ssize_t kdbSet_hosts(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	int errnosave = errno;
	ssize_t nr_keys = 0, nr_alias = 0;
	FILE *fp;
	Key *key, *alias=0;
	char * lastline;

	/* if (strcmp (keyName(kdbhGetMountpoint(handle)), keyName(parentKey))) return 0; */

	fp = fopen (keyString(ksLookupByName(pluginGetConfig (handle), "/path", 0)), "w");

	if (fp == 0)
	{
		/*kdbhSetError (handle, Plugin_ERR_NODIR);*/
		errno = errnosave;
		return -1;
	}

	// kdbbWriteLock (fp);

	ksRewind (returned);
	key = ksNext (returned); /* skip parentKey */

	nr_keys ++;

	while (1)
	{
		if (!alias) key = ksNext (returned);
		else key = alias;
		if (!key) break;

		lastline = strrchr (keyComment(key), '\n');
		if (lastline)
		{
			*lastline = '\0';
			fprintf (fp, "%s\n", keyComment(key));
			*lastline = '\n'; /* preserve comment */
		}

		fprintf (fp, "%s\t%s", (char*)keyValue(key), (char*)keyBaseName (key));

		nr_alias = 0;
		if (keyIsDir (key))
		{
			while ((alias = ksNext (returned)) != 0)
			{
				if (keyIsDir(alias)) break;
				if (strncmp (keyName(key), keyName(alias), strlen(keyName(key)))) break;;
				if (strlen(keyName(key)) + strlen (keyBaseName (alias)) + 1 != strlen (keyName(alias))) goto error;
				if (strncmp (keyBaseName (alias), "alias", 5)) goto error;
				fprintf (fp, "\t%s", (char*)keyValue (alias));
				nr_alias++;
			}
		} else alias = 0;

		if (lastline)
		{
			if (*(lastline+1) != '\0') fprintf (fp, " #%s", lastline+1);
		} else {
			if (*keyComment(key) != '\0') fprintf (fp, " #%s", keyComment(key));
		}

		fprintf (fp, "\n");
		nr_keys += nr_alias + 1;
	}

	// kdbbUnlock (fp);
	fclose (fp);
	errno = errnosave;
	return nr_keys;

error:
	// kdbbUnlock (fp);
	fclose (fp);
	/* Make the file empty */
	fp = fopen ("/tmp/hosts", "w");
	fclose (fp);
	errno = errnosave;
	return -1;
}

Plugin *KDBEXPORT(hosts)
{
	return pluginExport(BACKENDNAME,
		KDB_PLUGIN_OPEN,	&kdbOpen_hosts,
		KDB_PLUGIN_CLOSE,	&kdbClose_hosts,
		KDB_PLUGIN_GET,		&kdbGet_hosts,
		KDB_PLUGIN_SET,		&kdbSet_hosts,
		KDB_PLUGIN_VERSION,	BACKENDVERSION,
		KDB_PLUGIN_AUTHOR,	"Markus Raab <elektra@markus-raab.org>",
		KDB_PLUGIN_LICENCE,	"BSD",
		KDB_PLUGIN_DESCRIPTION,	"Reads and writes /etc/hosts content",
		KDB_PLUGIN_PROVIDES,	"storage",
		KDB_PLUGIN_NEEDS,	"",
		KDB_PLUGIN_END);
}

