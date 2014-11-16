/**
 * \file
 *
 * \brief Plugin for reading and writing the hosts file
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "hosts.h"
#include "keymetaformatting.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <kdbextension.h>
#include <kdbproposal.h>

#define MAX_SPACE_BUFFER = 16;

typedef void CommentConstructor(KeySet *, size_t, const char *, const char *);

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

size_t elektraParseToken (char **token, const char *line)
{
	size_t i = 0;

	/* skip whitespaces */
	while (line[i] == ' ' || line[i] == '\t') i++;

	/* end of line or string, abort */
	if (line[i] == '\0' || line[i] == '\n') return 0;

	size_t start = i;

	/* count the number of characters in the token */
	while (line[i] != ' ' && line[i] != '\t' && line[i] != '\0' && line[i] != '\n') i++;

	size_t tokenSize = i - start + 1;
	*token = (char *)elektraMalloc(tokenSize);
	strncpy(*token, line + start, tokenSize);
	(*token)[tokenSize - 1] = '\0';

	return i;
}

static void setOrderMeta(Key *key, int order)
{
	char buffer[MAX_ORDER_SIZE];
	snprintf (buffer, MAX_ORDER_SIZE, "%d", order);
	keySetMeta(key, "order", buffer);
}



static int parseComment(KeySet *comments, char *line, const char *commentStart, CommentConstructor constructor)
{
	/* count the number of whitespace characters before the comment */
	size_t spaces = elektraCountStartSpaces (line);

	if ( *(line + spaces) == '\n')
	{
		constructor(comments, spaces, 0, 0);
		return 1;
	}

	size_t commentStartLen = strlen(commentStart);
	if (!strncmp (line + spaces, commentStart, commentStartLen))
	{
		/* check for newlines */
		char *newLine = strchr(line, '\n');
		if (newLine)
		{
			*newLine = '\0';
		}

		constructor(comments, spaces, commentStart, line + spaces + commentStartLen);
		return 1;
	}

	return 0;
}

static char *parseCanonicalName(Key *result, char *line)
{
	char *fieldBuffer;
	char *tokenPointer = line;

	/* read the ip address (if any) */
	int sret = elektraParseToken(&fieldBuffer, line);

	if (sret == 0) return 0;

	tokenPointer += sret;

	/* determine whether this is an ipv4 or ipv6 entry */
	addAddressHierarchy (result, fieldBuffer);

	/* store the ip address */
	keySetString (result, fieldBuffer);

	elektraFree(fieldBuffer);

	/* read the canonical name */
	sret = elektraParseToken (&fieldBuffer, tokenPointer);

	if (sret == 0) return 0;

	tokenPointer += sret;
	keyAddBaseName (result, fieldBuffer);
	elektraFree(fieldBuffer);

	return tokenPointer;
}

static char *parseAlias(KeySet *append, const Key *hostParent, char *tokenPointer)
{
	char *fieldBuffer;
	int sret = 0;
	sret = elektraParseToken (&fieldBuffer, tokenPointer);
	if (sret == 0) return 0;

	Key *alias = keyDup (hostParent);
	keyAddBaseName (alias, fieldBuffer);
	elektraFree(fieldBuffer);

	/* only add the alias if it does not exist already */
	if(ksLookup(append, alias, KDB_O_NONE))
	{
		keyDel (alias);
	}
	else
	{
		ksAppendKey(append, alias);
	}

	return tokenPointer + sret;
}

static int elektraKeySetMetaKeySet(Key *key, KeySet *metaKeySet)
{
	if (!key) return 0;
	if (!metaKeySet) return 0;

	Key *currentMeta;
	cursor_t initialCursor = ksGetCursor(metaKeySet);
	ksRewind(metaKeySet);
	while ((currentMeta = ksNext(metaKeySet)))
	{
		keySetMeta(key, keyName(currentMeta), keyString(currentMeta));
	}

	ksSetCursor(metaKeySet, initialCursor);

	return 1;
}

int elektraHostsGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
	char readBuffer [HOSTS_KDB_BUFFER_SIZE];

	if (!strcmp (keyName(parentKey), "system/elektra/modules/hosts"))
	{
		KeySet *moduleConfig =
#include "contract.h"
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	FILE *fp = fopen (keyValue(parentKey), "r");

	if (fp == 0)
	{
		errno = errnosave;
		return 0;
	}

	ksClear (returned);
	KeySet *append = ksNew(ksGetSize(returned)*2, KS_END);

	Key *key = keyDup (parentKey);
	ksAppendKey(append, key);

	Key *currentKey = 0;
	KeySet *comments = ksNew(0);
	size_t order = 1;
	char *tokenPointer = 0;
	char *fret = 0;
	while(1)
	{
		fret = fgets (readBuffer, HOSTS_KDB_BUFFER_SIZE, fp);

		if (!fret) break;

		if (!currentKey)
		{
			currentKey = keyDup(parentKey);
		}

		if (parseComment(comments, readBuffer, "#", &elektraAddLineComment)) continue;

		tokenPointer = parseCanonicalName(currentKey, readBuffer);
		if (tokenPointer == 0) continue;

		/* canonical names have to be unique. If the hosts file contains
		 * duplicates, we honor only the first entry. This mirrors the
		 * behaviour of most name resolution implementations
		 */
		if (ksLookup(append, currentKey, KDB_O_NONE))
		{
			keyDel (currentKey);
			currentKey = 0;
			ksClear(comments);
			continue;
		}

		/* assign an order to the entry */
		setOrderMeta(currentKey, order);
		++ order;

		ksAppendKey(append, currentKey);

		/* Read in aliases */
		while (1)
		{
			/* if we find a comment, there cannot be any more aliases */
			if (parseComment(comments, tokenPointer, "#", &elektraAddInlineComment)) break;

			/* if we reach the end of the line, there cannot be any more aliases */
			tokenPointer = parseAlias (append, currentKey, tokenPointer);
			if (tokenPointer == 0) break;
		}

		/* flush collected comments and start over with a new entry */
		elektraKeySetMetaKeySet(currentKey, comments);
		ksClear(comments);
		currentKey = 0;
	}

	keyDel(currentKey);

	if (comments)
	{
		/* save comments without a matching entry to the parentkey */
		elektraKeySetMetaKeySet(parentKey, comments);
		ksClear(comments);
		ksDel(comments);
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
		ELEKTRA_SET_ERROR(10, parentKey, strerror(errno));
		ksDel (append);
		ret = -1;
	}

	fclose (fp);
	errno = errnosave;
	return ret;
}

static int keyCmpOrderWrapper(const void *a, const void *b)
{
	return elektraKeyCmpOrder(*((const Key **)a), *((const Key **)b));
}

static void writeComment(const char *spaces, const char *start, const char *comment, FILE *fp)
{
	if (spaces)
	{
		char *endptr;
		long spaceValue = strtol (spaces, &endptr, 10);

		if (*endptr == '\0')
		{
			for (int i = 0; i < spaceValue; i++)
			{
				fprintf (fp, " ");
			}
		}
	}

	if (start)
	{
		fprintf (fp, "%s",  start);
	}

	if (comment)
	{
		fprintf (fp, "%s", comment);
	}

}

static const char *getMetaValue(Key *key, const char *metaName)
{
	const Key *metaKey = keyGetMeta(key, metaName);

	if (metaKey) return keyString(metaKey);

	return 0;
}

static void writeLineComments(Key *key, FILE *fp)
{
	// TODO: this is really inefficient
	KeySet *metaKeys = elektraKeyGetMetaKeySet(key);
	Key *commentParent = keyNew("comment", KDB_O_META_NAME, KEY_END);
	KeySet *comments = elektraArrayGet(commentParent, metaKeys);
	keyDel(commentParent);

	ksRewind(comments);
	Key *current;
	while ((current = ksNext (comments)))
	{
		if (strcmp (keyName (current), "comment/#0"))
		{
			Key *spaceKey = keyDup (current);
			keyAddBaseName (spaceKey, "space");
			Key *startKey = keyDup (current);
			keyAddBaseName (startKey, "start");
			const char *spaces = getMetaValue (key, keyName (spaceKey));
			const char *start = getMetaValue (key, keyName (startKey));
			const char *comment = getMetaValue (key, keyName (current));
			keyDel (spaceKey);
			keyDel (startKey);
			writeComment (spaces, start, comment, fp);
			fprintf (fp, "\n");
		}
	}

	ksDel(metaKeys);
	ksDel(comments);
}

static void writeInlineComment(Key *key, FILE *fp)
{

	const char *spaces = getMetaValue(key, "comment/#0/space");
	const char *start = getMetaValue(key, "comment/#0/start");
	const char *comment = getMetaValue(key, "comment/#0");

	writeComment(spaces, start, comment, fp);
}

static void writeHostsEntry(Key* key, KeySet* returned, FILE* fp)
{
	fprintf (fp, "%s\t%s", (char*) keyValue (key), (char*) keyBaseName (key));
	/* position the cursor at the current key and
	 * iterate over its subkeys
	 */
	ksLookup (returned, key, KDB_O_NONE);
	Key* alias;
	while ((alias = ksNext (returned)) != 0)
	{
		if (keyRel (key, alias) < 1) break;

		fprintf (fp, " %s", (char*) keyBaseName (alias));
	}
}

int elektraHostsSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
	FILE *fp;
	Key *key;

	fp = fopen (keyString(parentKey), "w");

	if (fp == 0 && errno == EACCES)
	{
		ELEKTRA_SET_ERROR(9, parentKey, keyString(parentKey));
		errno = errnosave;
		return -1;
	}
	else if (fp == 0)
	{
		ELEKTRA_SET_ERRORF(75, parentKey, "%s could not be written because %s", keyString(parentKey), strerror(errno));
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

		writeLineComments(key, fp);
		writeHostsEntry (key, returned, fp);
		writeInlineComment(key, fp);
		fprintf (fp, "\n");
	}

	writeLineComments(parentKey, fp);

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

