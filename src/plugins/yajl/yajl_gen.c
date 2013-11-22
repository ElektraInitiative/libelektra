#include "yajl.h"

#include <stdio.h>
#include <string.h>

#include <kdbconfig.h>
#include <kdberrors.h>
#include <yajl/yajl_gen.h>

#include "name.h"
#include "iterator.h"


/**
 * @brief open so many levels as needed for key next
 *
 * Iterates over next and generate
 * needed groups for every / and needed arrays
 * for every name/#.
 * Yields name for leaf.
 *
 * TODO: cur should be renamed to prev and next should be renamed to cur
 *
 * @pre keys are not allowed to be below,
 *      except: first run where everything below root/parent key is
 *      opened
 *
 * @example
 *
 * Example for elektraNextNotBelow:
 * cur:  user/sw/org
 * next: user/sw/org/deeper
 * -> do nothing, "deeper" is value
 *
 *  -- cut --
 *
 * cur:  user/sw/org/deeper
 * next: user/sw/org/other
 * -> this cannot happen (see elektraNextNotBelow)
 *
 * cur:  user/sw/org/other
 * next: user/sw/org/other/deeper/below
 * -> this cannot happen (see elektraNextNotBelow)
 *
 *  -- cut --
 *
 * instead of cut two entries above following would happen:
 * cur:  user/sw/org/deeper
 * next: user/sw/org/other/deeper/below
 * -> and "other" and "deeper" would be opened
 *
 * cur:  user/sw/org/other/deeper/below
 * next: user/no
 * -> do nothing, because "no" is value
 *
 * cur:  user/no
 * next: user/oops/it/is/below
 * -> create map "oops" "it" "is"
 *
 * cur:  user/oops/it/is/below
 * next: user/x/t/s/x
 * -> create "x" "t" "s"
 *
 * @example
 *
 * cur:  user/sw/org/#0
 * next: user/sw/org/#1
 * -> will not open org or array (because that did not change),
 *    but will open group test (because within arrays every key
 *    needs a group).
 *
 * cur:  user/sw/org/#0
 * next: user/sw/oth/#0
 * -> will open new group oth and new array and yield blah
 *
 * cur:  user/sw
 * next: user/sw/array/#0
 * -> will yield a new array using name "array"
 *
 * @pre cur and next have a name which is not equal
 *
 * @param g handle to generate to
 * @param cur current key of iteration
 * @param next next key of iteration
 */
static void elektraGenOpen(yajl_gen g, const Key *cur, const Key *next)
{
	const char *pcur = keyName(cur);
	const char *pnext = keyName(next);
	// size_t curLevels = elektraKeyCountLevel(cur);
	size_t nextLevels = elektraKeyCountLevel(next);
	size_t size=0;
	size_t csize=0;

	int equalLevels = elektraKeyCountEqualLevel(cur, next);
	// forward all equal levels, nothing to do there
	for (int i=0; i < equalLevels; ++i)
	{
		pnext=keyNameGetOneLevel(pnext+size,&size);
		pcur=keyNameGetOneLevel(pcur+csize,&csize);
	}

	int levels = nextLevels - equalLevels;

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("Open %d: pcur: %s , pnext: %s\n", (int) levels,
		pcur, pnext);
#endif

	for (int i=0; i<levels-2; ++i)
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf("Level %d: \"%.*s\"\n", (int)i,
				(int)size, pnext);
#endif
		pnext=keyNameGetOneLevel(pnext+size,&size);

#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("GEN string %.*s for ordinary group\n",
				(int)size, pnext);
#endif
		yajl_gen_string(g, (const unsigned char *)pnext, size);

#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("GEN map open because we stepped\n");
#endif
		yajl_gen_map_open(g);
	}

	if (!strcmp(keyBaseName(next), "#0"))
	{
		pnext=keyNameGetOneLevel(pnext+size,&size);
		// we have found the first element of an array
#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("GEN string %.*s for array\n",
				(int)size, pnext);
#endif
		yajl_gen_string(g, (const unsigned char *)pnext, size);

#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("GEN array open\n");
#endif
		yajl_gen_array_open(g);
	}
	else if (*keyBaseName(next) != '#')
	{
		if (levels > 1)
		{
			// not an array, print missing element + name for later
			// value
			pnext=keyNameGetOneLevel(pnext+size,&size);

#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN string %.*s for last group\n",
					(int)size, pnext);
#endif
			yajl_gen_string(g, (const unsigned char *)pnext, size);

#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN map open because we stepped\n");
#endif
			yajl_gen_map_open(g);
		}

#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("GEN string %.*s for value's name\n",
				(int)keyGetBaseNameSize(next)-1,
				keyBaseName(next));
#endif
		yajl_gen_string(g, (const unsigned char *)keyBaseName(next),
				keyGetBaseNameSize(next)-1);
	}
}

/**
 * @brief Close all levels from cur to next
 *
 * @pre keys are not allowed to be below,
 *      except: last run where everything below root/parent key is
 *      closed
 *
 * cur:  user/sw/org/deeper
 * next: user/sw/org/other/deeper/below
 * -> nothing will be done ("deeper" is value)
 * [eq: 3, cur: 4, next: 6, gen: 0]
 *
 * cur:  user/sw/org/other/deeper/below
 * next: user/no
 * -> "deeper", "other", "org" and "sw" maps will be closed ("below" is value)
 * [eq: 1, cur: 6, next: 2, gen: 4]
 *
 * cur:  user/no
 * next: user/oops/it/is/below
 * -> nothing will be done ("no" is value)
 * [eq: 1, cur: 2, next: 5, gen: 0]
 *
 * cur:  user/oops/it/is/below
 * next: user/x/t/s/x
 * -> close "is", "it", "oops"
 * [eq: 1, cur: 5, next: 5, gen: 3]
 *
 * last iteration (e.g. close down to root)
 * cur:  user/x/t/s/x
 * next: user
 * -> close "s", "t" and "x" maps
 * [eq: 1, cur: 5, next: 1, gen: 3]
 *
 * cur:  user/#0/1/1/1
 * next: user/#1/1/1/1
 * -> close "1", "1", "1", but not array
 * [eq: 1, cur: 5, next: 5, gen: 3]
 *
 * @param g
 * @param cur
 * @param next
 */
static void elektraGenClose(yajl_gen g, const Key *cur, const Key *next)
{
	int curLevels = elektraKeyCountLevel(cur);
#ifdef ELEKTRA_YAJL_VERBOSE
	int nextLevels = elektraKeyCountLevel(next);
#endif
	int equalLevels = elektraKeyCountEqualLevel(cur, next);

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("in close, eq: %d, cur: %s %d, next: %s %d\n",
			equalLevels,
			keyName(cur), curLevels,
			keyName(next), nextLevels);
#endif


	ssize_t counter = curLevels;

	keyNameReverseIterator curIt =
		elektraKeyNameGetReverseIterator(cur);
	keyNameReverseIterator nextIt =
		elektraKeyNameGetReverseIterator(next);

	// go to last element of cur (which is a value)
	elektraKeyNameReverseNext(&curIt);
	elektraKeyNameReverseNext(&nextIt);
	counter--;

	// we are closing an array
	if (*curIt.current == '#' && *nextIt.current != '#')
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf("GEN array close\n");
#endif
		yajl_gen_array_close(g);
		counter --;
	}

	while ( elektraKeyNameReverseNext(&curIt) &&
		counter > equalLevels)
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf("Close [%d > %d]: \"%.*s\"\n",
			(int)counter,
			(int)equalLevels,
			(int)curIt.size, curIt.current);
#endif
		counter --;

#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("GEN map close ordinary group\n");
#endif
		yajl_gen_map_close(g);
	}
}


/**
 * @brief Generate the value for the current key
 *
 * No auto-guessing takes place, because that can be terrible wrong and
 * is not reversible. So make sure that all your boolean and numbers
 * have the proper type in meta value "type".
 *
 * In case of type problems it will be rendered as string but a warning
 * will be added. Use a type checker to avoid such problems.
 *
 * @param g handle to generate to
 * @param parentKey needed for adding warnings/errors
 * @param cur the key to generate the value from
 */
static void elektraGenValue(yajl_gen g, Key *parentKey, const Key *cur)
{
#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("GEN value %s for %s\n", keyString(cur), keyName(cur));
#endif

	const Key * type = keyGetMeta(cur, "type");
	if (!type && keyGetValueSize(cur) == 0) // empty binary type is null
	{
		yajl_gen_null(g);
	}
	else if (!type && keyGetValueSize(cur) >= 1) // default is string
	{
		yajl_gen_string(g, (const unsigned char *)keyString(cur), keyGetValueSize(cur)-1);
	}
	else if (!strcmp(keyString(type), "boolean"))
	{
		if (!strcmp(keyString(cur), "true"))
		{
			yajl_gen_bool(g, 1);
		}
		else if (!strcmp(keyString(cur), "false"))
		{
			yajl_gen_bool(g, 0);
		}
		else
		{
			ELEKTRA_ADD_WARNING(78, parentKey, "got boolean which is neither true nor false");
			yajl_gen_string(g, (const unsigned char *)keyString(cur), keyGetValueSize(cur)-1);
		}
	}
	else if (!strcmp(keyString(type), "number")) // TODO: distuingish between float and int
	{
		yajl_gen_number(g, keyString(cur), keyGetValueSize(cur)-1);
	}
	else { // unknown or unsupported type, render it as string but add warning
		ELEKTRA_ADD_WARNING(78, parentKey, keyString(type));
		yajl_gen_string(g, (const unsigned char *)keyString(cur), keyGetValueSize(cur)-1);
	}
}

static int elektraRemoveFile(Key *parentKey)
{
	FILE *fp = fopen(keyString(parentKey), "w"); // truncates file
	if (!fp)
	{
		ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		return -1;
	}

	fclose (fp);
	return 0;
}

int elektraYajlSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
#if YAJL_MAJOR == 1
	yajl_gen_config conf = { 1, "  " };
	yajl_gen g = yajl_gen_alloc(&conf, NULL
#else
	yajl_gen g = yajl_gen_alloc(NULL);
	yajl_gen_config(g, yajl_gen_beautify, 1);
	yajl_gen_config(g, yajl_gen_validate_utf8, 1);
#endif

	ksRewind (returned);
	Key *cur = elektraNextNotBelow(returned);
	if (!cur)
	{
		return elektraRemoveFile(parentKey);
	}

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("GEN map open START\n");
#endif
	yajl_gen_map_open(g);

	KeySet *config= elektraPluginGetConfig(handle);
	if (!strncmp(keyName(parentKey), "user", 4))
	{
		const Key * lookup = ksLookupByName(config, "/user_path", 0);
		if (!lookup)
		{
			elektraGenOpen(g, parentKey, cur);
		} else {
			elektraGenOpen(g, lookup, cur);
		}
	}
	else
	{
		const Key * lookup = ksLookupByName(config, "/system_path", 0);
		if (!lookup)
		{
			elektraGenOpen(g, parentKey, cur);
		} else {
			elektraGenOpen(g, lookup, cur);
		}
	}

	Key *next = 0;
	while ((next = elektraNextNotBelow(returned)) != 0)
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("\nin iter: %s next: %s\n", keyName(cur), keyName(next));
		printf ("in f: %s next: %s\n", keyName(cur), keyName(next));
#endif

		elektraGenValue(g, parentKey, cur);
		elektraGenClose(g, cur, next);
		elektraGenOpen(g, cur, next);

		cur = next;
	}

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("\nleaving loop: %s\n", keyName(cur));
#endif

	elektraGenValue(g, parentKey, cur);

	// Close what we opened in the beginning
	if (!strncmp(keyName(parentKey), "user", 4))
	{
		const Key * lookup = ksLookupByName(config, "/user_path", 0);
		if (!lookup)
		{
			elektraGenClose(g, cur, parentKey);
		} else {
			elektraGenClose(g, cur, lookup);
		}
	}
	else
	{
		const Key * lookup = ksLookupByName(config, "/system_path", 0);
		if (!lookup)
		{
			elektraGenClose(g, cur, parentKey);
		} else {
			elektraGenClose(g, cur, lookup);
		}
	}

	// hack: because "user" or "system" never gets closed
	// TODO: do properly by using dirname for closing
#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("GEN map close FINAL\n");
#endif
	yajl_gen_map_close(g);

	FILE *fp = fopen(keyString(parentKey), "w");
	if (!fp)
	{
		ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		return -1;
	}

	const unsigned char * buf;
	yajl_size_type len;
	yajl_gen_get_buf(g, &buf, &len);
	fwrite(buf, 1, len, fp);
	yajl_gen_clear(g);
	yajl_gen_free(g);


	fclose (fp);

	return 1; /* success */
}
