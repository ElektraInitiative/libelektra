/**
 * @file
 *
 * @brief Contains the get direction of the hosts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./hosts.h"
#include "./keymetaformatting.h"

#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/types.h>

#include <internal/kdb/config.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/old_helper.h>


typedef void CommentConstructor (KeySet *, size_t, const char *, const char *);

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

static void addAddressHierarchy (Key * key, char * fieldbuffer)
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

static void setOrderMeta (Key * key, int order)
{
	char buffer[MAX_ORDER_SIZE];
	snprintf (buffer, MAX_ORDER_SIZE, "%d", order);
	keySetMeta (key, "order", buffer);
}

static int parseComment (KeySet * comments, char * line, const char * commentStart, CommentConstructor constructor)
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

static char * parseCanonicalName (Key * result, char * line)
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
	keySetString (result, fieldBuffer);

	elektraFree (fieldBuffer);

	/* read the canonical name */
	sret = elektraParseToken (&fieldBuffer, tokenPointer);

	if (sret == 0) return 0;

	tokenPointer += sret;
	keyAddBaseName (result, fieldBuffer);
	elektraFree (fieldBuffer);

	return tokenPointer;
}

static char * parseAlias (KeySet * append, const Key * hostParent, char * tokenPointer)
{
	char * fieldBuffer;
	int sret = 0;
	sret = elektraParseToken (&fieldBuffer, tokenPointer);
	if (sret == 0) return 0;

	Key * alias = keyDup (hostParent, KEY_CP_ALL);
	keyAddBaseName (alias, fieldBuffer);
	elektraFree (fieldBuffer);

	/* only add the alias if it does not exist already */
	if (ksLookup (append, alias, KDB_O_NONE))
	{
		keyDel (alias);
	}
	else
	{
		ksAppendKey (append, alias);
	}

	return tokenPointer + sret;
}

static int elektraKeySetMetaKeySet (Key * key, KeySet * metaKeySet)
{
	if (!key) return 0;
	if (!metaKeySet) return 0;

	Key * currentMeta;

	for (elektraCursor it = 0; it < ksGetSize (metaKeySet); it++)
	{
		currentMeta = ksAtCursor (metaKeySet, it);
		keySetMeta (key, keyName (currentMeta), keyString (currentMeta));
	}

	return 1;
}

int elektraHostsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	int errnosave = errno;
	char readBuffer[HOSTS_KDB_BUFFER_SIZE];

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/hosts"))
	{
		KeySet * moduleConfig =
#include "./contract.h"
			ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	FILE * fp = fopen (keyValue (parentKey), "r");

	if (fp == 0)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	ksClear (returned);
	KeySet * append = ksNew (ksGetSize (returned) * 2, KS_END);

	Key * key = keyDup (parentKey, KEY_CP_ALL);
	ksAppendKey (append, key);

	Key * currentKey = 0;
	KeySet * comments = ksNew (0, KS_END);
	size_t order = 1;
	char * tokenPointer = 0;
	while (1)
	{
		char * fret = fgets (readBuffer, HOSTS_KDB_BUFFER_SIZE, fp);

		if (!fret) break;

		if (!currentKey)
		{
			currentKey = keyDup (parentKey, KEY_CP_ALL);
		}

		if (parseComment (comments, readBuffer, "#", &elektraAddLineComment)) continue;

		tokenPointer = parseCanonicalName (currentKey, readBuffer);
		if (tokenPointer == 0) continue;

		/* canonical names have to be unique. If the hosts file contains
		 * duplicates, we honor only the first entry. This mirrors the
		 * behaviour of most name resolution implementations
		 */
		if (ksLookup (append, currentKey, KDB_O_NONE))
		{
			keyDel (currentKey);
			currentKey = 0;
			ksClear (comments);
			continue;
		}

		/* assign an order to the entry */
		setOrderMeta (currentKey, order);
		++order;

		ksAppendKey (append, currentKey);

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
		ksClear (comments);
		currentKey = 0;
	}

	keyDel (currentKey);

	if (comments)
	{
		/* save comments without a matching entry to the parentkey */
		elektraKeySetMetaKeySet (parentKey, comments);
		ksClear (comments);
		ksDel (comments);
	}

	int ret;
	if (!ferror (fp))
	{
		ksClear (returned);
		ksAppend (returned, append);
		ksDel (append);
		ret = 1;
	}
	else
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "General parse error. Reason: %s", strerror (errno));
		ksDel (append);
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

