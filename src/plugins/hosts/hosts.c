/**
 * \file
 *
 * \brief Plugin for reading and writing the hosts file
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "hosts.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <kdbextension.h>
#include <kdbproposal.h>

size_t elektraStrLen(const char *s);

/*
 * Determines the address family of the supplied network address
 *
 * @param address the network address to be analysed
 * @return a number identifying the network address (e.g. AF_INET) or -1 if an error occurred
 */
static int getAddressFamily(const char *address)
{
	struct addrinfo hint;
	struct addrinfo *info;
	memset (&hint, 0, sizeof (hint));

	/* no specific family is requested and name lookups are disabled */
	hint.ai_family = AF_UNSPEC;
	hint.ai_flags = AI_NUMERICHOST;

	int ret = getaddrinfo (address, 0, &hint, &info);

	if (ret != 0)
	{
		return -1;
	}

	int result = info->ai_family;
	freeaddrinfo (info);
	return result;
}

static void addAddressHierarchy(Key* key, char* fieldbuffer)
{
	/* determine whether this is an ipv4 or ipv6 entry */
	int family = getAddressFamily (fieldbuffer);

	/* in case of an error default to ipv4 */
	switch (family)
	{
	case AF_INET6:
		keyAddBaseName (key, "ipv6");
		break;
	default:
		keyAddBaseName (key, "ipv4");
	}
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
	char readBuffer [HOSTS_KDB_BUFFER_SIZE];
	char *fieldBuffer;
	size_t readsize;
	char *fret;
	int   sret;
	Key *alias;
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
#include "readme_hosts.c"
			keyNew ("system/elektra/modules/hosts/infos/version",
				KEY_VALUE, PLUGINVERSION, 
				KEY_END),
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

	Key *key = keyDup (parentKey);
	ksAppendKey(append, key);

	while (1)
	{
		fret = fgets (readBuffer, HOSTS_KDB_BUFFER_SIZE, fp);

		if (fret == 0) 
		{
			/* we are done, flush everything we have collected */
			fclose (fp);
			ksClear (returned);
			ksAppend (returned, append);
			ksDel (append);
			errno = errnosave;
			return 1;
		}

		/* search for a comment in the current line (if any) */
		if (elektraHostsAppendComment(comment, readBuffer)) continue;

		/* read the ip address (if any) */
		sret = elektraHostsFindToken (&fieldBuffer, readBuffer);
		if (sret == 0) continue;

		/* from now on we know that we are in a line with a hosts entry */
		key = keyDup (parentKey);

		/* determine whether this is an ipv4 or ipv6 entry */
		addAddressHierarchy (key, fieldBuffer);

		keySetString (key, fieldBuffer);

		/* read the canonical name */
		readsize = sret;
		sret = elektraHostsFindToken (&fieldBuffer, readBuffer+readsize);
		keyAddBaseName (key, fieldBuffer);

		/* canonical names have to be unique. If the hosts file contains
		 * duplicates, we honor only the first entry. This mirrors the
		 * behaviour of most name resolution implementations
		 */
		if (ksLookup(append, key, KDB_O_NONE))
		{
			keyDel (key);
			continue;
		}

		/* flush any collected comment lines and start with a new comment */
		keySetComment (key, comment);
		*comment = '\0';

		/* assign an order to the entry */
		elektraHostsSetMeta(key, order);
		++ order;

		ksAppendKey(append, key);

		/* Read in aliases */
		while (1)
		{
			readsize += sret;
			sret = elektraHostsFindToken (&fieldBuffer, readBuffer+readsize);
			if (sret == 0) break;

			alias = keyDup (key);
			keyAddBaseName (alias, fieldBuffer);

			/* only add the alias if it does not exist already */
			if(ksLookup(returned, alias, KDB_O_NONE))
			{
				keyDel (alias);
			}
			else
			{
				ksAppendKey(append, alias);
			}
		}
	}

	ELEKTRA_SET_ERROR(10, parentKey, readBuffer);
	ksDel (append);
	// kdbbUnlock (fp);
	fclose (fp);
	errno = errnosave;
	return -1;
}

static int keyCmpOrderWrapper(const void *a, const void *b)
{
	return elektraKeyCmpOrder(*((const Key **)a), *((const Key **)b));
}

static void writeHostsComment(FILE *fp, Key *key)
{
	char *saveptr = 0;
	char *commentLine;
	char *commentCopy = malloc (keyGetCommentSize(key));
	strcpy (commentCopy, keyComment(key));
	commentLine = strtok_r (commentCopy, "\n", &saveptr);
	while (commentLine != 0)
	{
		fprintf (fp, "# %s\n", commentLine);
		commentLine = strtok_r (NULL, "\n", &saveptr);
	}
	free (commentCopy);
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

	/* build an array of entries and sort them according to their order metadata */
	Key **keyArray;
	size_t arraySize = ksGetSize(returned);
	keyArray = calloc (arraySize, sizeof (Key*));

	ksRewind (returned);
	int ret = elektraKsToMemArray(returned, keyArray);

	if (ret < 0)
	{
		ELEKTRA_SET_ERROR (67, parentKey, strerror (errno));
		fclose (fp);
		return -1;
	}

	qsort (keyArray, arraySize, sizeof(Key *), keyCmpOrderWrapper);

	Key *ipv4Base = keyDup (parentKey);
	keyAddBaseName(ipv4Base, "ipv4");
	Key *ipv6Base = keyDup (parentKey);
	keyAddBaseName (ipv6Base, "ipv6");

	/* now write the hosts file */
	for (size_t i=0; i< arraySize; ++i)
	{
		key = keyArray[i];

		/* only process canonical name keys */
		if (!keyIsDirectBelow(ipv4Base, key) && !keyIsDirectBelow(ipv6Base, key)) continue;

		lastline = strrchr (keyComment(key), '\n');
		if (lastline)
		{
			*lastline = '\0';
			writeHostsComment (fp, key);
			*lastline = '\n'; /* preserve comment */
		}

		fprintf (fp, "%s\t%s", (char*)keyValue(key), (char*)keyBaseName (key));

		/* position the cursor at the current key and
		 * iterate over its subkeys
		 */
		ksLookup(returned, key, KDB_O_NONE);
		while ((alias = ksNext (returned)) != 0)
		{
			if (keyRel (key, alias) < 1) break;

			fprintf (fp, " %s", (char*)keyBaseName(alias));
		}

		if (lastline)
		{
			if (*(lastline+1) != '\0') fprintf (fp, " # %s", lastline+1);
		} else {
			if (*keyComment(key) != '\0') fprintf (fp, " # %s", keyComment(key));
		}

		fprintf (fp, "\n");
	}
	keyDel (ipv4Base);
	keyDel (ipv6Base);

	fclose (fp);
	errno = errnosave;
	free (keyArray);
	return 1;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(hosts)
{
	return elektraPluginExport("hosts",
		ELEKTRA_PLUGIN_GET,	&elektraHostsGet,
		ELEKTRA_PLUGIN_SET,	&elektraHostsSet,
		ELEKTRA_PLUGIN_END);
}

