/**
 * @file
 *
 * @brief Contains the get direction of the hosts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "hosts.h"
#include "keymetaformatting.h"

#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/types.h>

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif


typedef void CommentConstructor (ElektraKeyset *, size_t, const char *, const char *);

/*
 * Determines the address family of the supplied network address
 *
 * @param address the network address to be analyzed
 * @return a number identifying the network address (e.g. AF_INET) or -1 if an error occurred
 */
static int getAddressFamily (const char * address)
{
	struct addrinfo hint;
	struct addrinfo * info;
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

static void addAddressHierarchy (ElektraKey * key, char * fieldbuffer)
{
	/* determine whether this is an ipv4 or ipv6 entry */
	int family = getAddressFamily (fieldbuffer);

	/* in case of an error default to ipv4 */
	switch (family)
	{
	case AF_INET6:
		elektraKeyAddBaseName (key, "ipv6");
		break;
	default:
		elektraKeyAddBaseName (key, "ipv4");
	}
}

size_t elektraParseToken (char ** token, const char * line)
{
	size_t i = 0;

	/* skip whitespaces */
	while (line[i] == ' ' || line[i] == '\t')
		i++;

	/* end of line or string, abort */
	if (line[i] == '\0' || line[i] == '\n') return 0;

	size_t start = i;

	/* count the number of characters in the token */
	while (line[i] != ' ' && line[i] != '\t' && line[i] != '\0' && line[i] != '\n')
		i++;

	size_t tokenSize = i - start + 1;
	*token = (char *) elektraMalloc (tokenSize);
	strncpy (*token, line + start, tokenSize);
	(*token)[tokenSize - 1] = '\0';

	return i;
}

static void setOrderMeta (ElektraKey * key, int order)
{
	char buffer[MAX_ORDER_SIZE];
	snprintf (buffer, MAX_ORDER_SIZE, "%d", order);
	elektraKeySetMeta (key, "order", buffer);
}

static int parseComment (ElektraKeyset * comments, char * line, const char * commentStart, CommentConstructor constructor)
{
	/* count the number of whitespace characters before the comment */
	size_t spaces = elektraCountStartSpaces (line);

	if (*(line + spaces) == '\n')
	{
		constructor (comments, spaces, 0, 0);
		return 1;
	}

	size_t commentStartLen = strlen (commentStart);
	if (!strncmp (line + spaces, commentStart, commentStartLen))
	{
		/* check for newlines */
		char * newLine = strchr (line, '\n');
		if (newLine)
		{
			*newLine = '\0';
		}

		constructor (comments, spaces, commentStart, line + spaces + commentStartLen);
		return 1;
	}

	return 0;
}

static char * parseCanonicalName (ElektraKey * result, char * line)
{
	char * fieldBuffer;
	char * tokenPointer = line;

	/* read the ip address (if any) */
	int sret = elektraParseToken (&fieldBuffer, line);

	if (sret == 0) return 0;

	tokenPointer += sret;

	/* determine whether this is an ipv4 or ipv6 entry */
	addAddressHierarchy (result, fieldBuffer);

	/* store the ip address */
	elektraKeySetString (result, fieldBuffer);

	elektraFree (fieldBuffer);

	/* read the canonical name */
	sret = elektraParseToken (&fieldBuffer, tokenPointer);

	if (sret == 0) return 0;

	tokenPointer += sret;
	elektraKeyAddBaseName (result, fieldBuffer);
	elektraFree (fieldBuffer);

	return tokenPointer;
}

static char * parseAlias (ElektraKeyset * append, const ElektraKey * hostParent, char * tokenPointer)
{
	char * fieldBuffer;
	int sret = 0;
	sret = elektraParseToken (&fieldBuffer, tokenPointer);
	if (sret == 0) return 0;

	ElektraKey * alias = elektraKeyDup (hostParent, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (alias, fieldBuffer);
	elektraFree (fieldBuffer);

	/* only add the alias if it does not exist already */
	if (elektraKeysetLookup (append, alias, ELEKTRA_KDB_O_NONE))
	{
		elektraKeyDel (alias);
	}
	else
	{
		elektraKeysetAppendKey (append, alias);
	}

	return tokenPointer + sret;
}

static int elektraKeySetMetaKeySet (ElektraKey * key, ElektraKeyset * metaKeySet)
{
	if (!key) return 0;
	if (!metaKeySet) return 0;

	ElektraKey * currentMeta;
	elektraCursor initialCursor = elektraKeysetGetCursor (metaKeySet);
	elektraKeysetRewind (metaKeySet);
	while ((currentMeta = elektraKeysetNext (metaKeySet)))
	{
		elektraKeySetMeta (key, elektraKeyName (currentMeta), elektraKeyString (currentMeta));
	}

	elektraKeysetSetCursor (metaKeySet, initialCursor);

	return 1;
}

int elektraHostsGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	int errnosave = errno;
	char readBuffer[HOSTS_KDB_BUFFER_SIZE];

	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/hosts"))
	{
		ElektraKeyset * moduleConfig =
#include "contract.h"
			elektraKeysetAppend (returned, moduleConfig);
		elektraKeysetDel (moduleConfig);
		return 1;
	}

	FILE * fp = fopen (elektraKeyValue (parentKey), "r");

	if (fp == 0)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	elektraKeysetClear (returned);
	ElektraKeyset * append = elektraKeysetNew (elektraKeysetGetSize (returned) * 2, ELEKTRA_KS_END);

	ElektraKey * key = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeysetAppendKey (append, key);

	ElektraKey * currentKey = 0;
	ElektraKeyset * comments = elektraKeysetNew (0, ELEKTRA_KS_END);
	size_t order = 1;
	char * tokenPointer = 0;
	while (1)
	{
		char * fret = fgets (readBuffer, HOSTS_KDB_BUFFER_SIZE, fp);

		if (!fret) break;

		if (!currentKey)
		{
			currentKey = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
		}

		if (parseComment (comments, readBuffer, "#", &elektraAddLineComment)) continue;

		tokenPointer = parseCanonicalName (currentKey, readBuffer);
		if (tokenPointer == 0) continue;

		/* canonical names have to be unique. If the hosts file contains
		 * duplicates, we honor only the first entry. This mirrors the
		 * behaviour of most name resolution implementations
		 */
		if (elektraKeysetLookup (append, currentKey, ELEKTRA_KDB_O_NONE))
		{
			elektraKeyDel (currentKey);
			currentKey = 0;
			elektraKeysetClear (comments);
			continue;
		}

		/* assign an order to the entry */
		setOrderMeta (currentKey, order);
		++order;

		elektraKeysetAppendKey (append, currentKey);

		/* Read in aliases */
		while (1)
		{
			/* if we find a comment, there cannot be any more aliases */
			if (parseComment (comments, tokenPointer, "#", &elektraAddInlineComment)) break;

			/* if we reach the end of the line, there cannot be any more aliases */
			tokenPointer = parseAlias (append, currentKey, tokenPointer);
			if (tokenPointer == 0) break;
		}

		/* flush collected comments and start over with a new entry */
		elektraKeySetMetaKeySet (currentKey, comments);
		elektraKeysetClear (comments);
		currentKey = 0;
	}

	elektraKeyDel (currentKey);

	if (comments)
	{
		/* save comments without a matching entry to the parentkey */
		elektraKeySetMetaKeySet (parentKey, comments);
		elektraKeysetClear (comments);
		elektraKeysetDel (comments);
	}

	int ret;
	if (!ferror (fp))
	{
		elektraKeysetClear (returned);
		elektraKeysetAppend (returned, append);
		elektraKeysetDel (append);
		ret = 1;
	}
	else
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "General parse error. Reason: %s", strerror (errno));
		elektraKeysetDel (append);
		ret = -1;
	}

	fclose (fp);
	errno = errnosave;
	return ret;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("hosts",
		ELEKTRA_PLUGIN_GET,	&elektraHostsGet,
		ELEKTRA_PLUGIN_SET,	&elektraHostsSet,
		ELEKTRA_PLUGIN_END);
}

