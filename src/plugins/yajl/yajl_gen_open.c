/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "yajl_gen.h"

// TODO: use unesacped names instead
static char * keyNameGetOneLevel (const char * name, size_t * size)
{
	char * real = (char *) name;
	size_t cursor = 0;
	int end = 0;	     // bool to check for end of level
	int escapeCount = 0; // counter to check if / was escaped

	/* skip all repeating '/' in the beginning */
	while (*real && *real == '/')
	{
		++real;
	}

	/* now see where this basename ends handling escaped chars with '\' */
	while (real[cursor] && !end)
	{
		switch (real[cursor])
		{
		case '\\':
			++escapeCount;
			break;
		case '/':
			if (!(escapeCount % 2))
			{
				end = 1;
			}
		// fallthrough
		default:
			escapeCount = 0;
		}
		++cursor;
	}

	/* if a '/' stopped our loop, balance the counter */
	if (end)
	{
		--cursor;
	}

	*size = cursor;
	return real;
}

/**
 * @brief Iterate over string and open everything
 *
 * @pre Sometimes the first or last value needs special handling, the
 * caller needs to do that.
 * Implements lookahead for arrays, but does not implement leaf
 * semantics.
 *
 * @pre g must be in a map environment
 *
 * @post g is left in map or array environment
 *
 *
 * It handles following scenarios:
 *
 * (N0)
 * #/_
 * found array start, but followed by non-array so we need a map
 * yield array and map (name of array done already)
 *
 * (N1)
 * #
 * found array start, yield array (name done already)
 *
 * (N2)
 * _/# (or empty array/map)
 *
 * found array name, yield string (array done later)
 *
 * (N3)
 * _
 * found map start, yield string + map
 *
 * @param g to yield maps, strings
 * @param pnext a pointer to the name of the key at the correct pos
 * @param levels to iterate, if smaller or equal zero it does nothing
 */
static void elektraGenOpenIterate (yajl_gen g, const char * pnext, int levels)
{
	size_t size = 0;

	ELEKTRA_LOG_DEBUG ("levels: %d,  next: \"%s\"", levels, pnext);

	for (int i = 0; i < levels; ++i)
	{
		pnext = keyNameGetOneLevel (pnext + size, &size);

		lookahead_t lookahead = elektraLookahead (pnext, size);

		ELEKTRA_LOG_DEBUG ("level by name %d: \"%.*s\", lookahead: %d", (int) i, (int) size, pnext, lookahead);

		// do not yield array indizes as names
		if (*pnext == '#')
		{
			ELEKTRA_LOG_DEBUG ("GEN (N1) array by name");
			yajl_gen_array_open (g);

			if (lookahead == LOOKAHEAD_MAP)
			{
				ELEKTRA_LOG_DEBUG ("GEN (N0) anon-map");
				yajl_gen_map_open (g);
			}
		}
		else if (lookahead == LOOKAHEAD_ARRAY || lookahead == LOOKAHEAD_EMPTY_ARRAY || lookahead == LOOKAHEAD_EMPTY_MAP)
		{
			ELEKTRA_LOG_DEBUG ("GEN (N2) string for start/empty array/map %.*s", (int) size, pnext);
			yajl_gen_string (g, (const unsigned char *) pnext, size);

			// opening (empty) array will be handled later
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("GEN (N3) string %.*s", (int) size, pnext);
			yajl_gen_string (g, (const unsigned char *) pnext, size);

			ELEKTRA_LOG_DEBUG ("GEN (N3) map by name");
			yajl_gen_map_open (g);
		}
	}
}


/**
 * @brief fixes elektraGenOpenIterate for the special handling of
 * arrays at very last position.
 *
 * @param g generate array there
 * @param key the key to look at
 */
static void elektraGenOpenLast (yajl_gen g, const Key * key)
{
	keyNameReverseIterator last = elektraKeyNameGetReverseIterator (key);
	elektraKeyNameReverseNext (&last);

	ELEKTRA_LOG_DEBUG ("last startup entry: \"%.*s\"", (int) last.size, last.current);

	if (last.current[0] == '#' && strcmp (last.current, "###empty_array") != 0)
	{
		// is an array, but not an empty one
		ELEKTRA_LOG_DEBUG ("GEN array open last");
		yajl_gen_array_open (g);
	}
}

/**
 * @brief Open initially
 *
 * Unlike in elektraGenOpen there is no precondition that parentKey
 * must be below first. Instead:
 *
 * @pre first must be below parentKey
 *
 * @pre g expects to be in a map
 *
 * @post g left in map or array
 *
 * Does not have special handling for first key (only for last key)
 *
 * @see elektraGenOpen
 *
 * @param g
 * @param parentKey
 * @param first
 */
void elektraGenOpenInitial (yajl_gen g, Key * parentKey, const Key * first)
{
	const char * pfirst = keyName (first);
	size_t csize = 0;

	int equalLevels = elektraKeyCountEqualLevel (parentKey, first);
	int firstLevels = elektraKeyCountLevel (first);

	// forward all equal levels
	for (int i = 0; i < equalLevels + 1; ++i)
	{
		pfirst = keyNameGetOneLevel (pfirst + csize, &csize);
	}

	// calculate levels: do not iterate over last element
	const int levelsToOpen = firstLevels - equalLevels - 1;

	ELEKTRA_LOG_DEBUG ("open by name Initial %s, equal: %d, to open: %d", pfirst, equalLevels, levelsToOpen);


	if (pfirst && *pfirst == '#')
	{
		ELEKTRA_LOG_DEBUG ("array open INITIAL");
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("GEN map open INITIAL");
		yajl_gen_map_open (g);
	}


	elektraGenOpenIterate (g, pfirst, levelsToOpen);

	elektraGenOpenLast (g, first);
}


/**
 * @brief Implements special handling for the first found name
 *
 * @pre expects to be in a map or array
 *
 * @post will leave in a map or array
 *
 * Looks at first place where two key name differ and need a lookahead
 * of one in next to not generate too much.
 *
 * It handles the first level entirely, except of values.
 *
 * _ .. is an arbitrary map
 * # .. is an arbitrary member of array
 * Note that "test/" would not be contained in the string
 * given by argument.
 *
 * (O1)
 * cur:  test/#
 * next: test/#
 *
 * Will do nothing, because we are in a situation where we just iterate
 * over leaves in the array.
 *
 * (O2)
 * cur:  test/#
 * next: test/#/#
 *
 * Will create an array in the array.
 * Array won't have a name.
 * Staying in the same array "test".
 *
 * (O3)
 * cur:  test/#
 * next: test/#/_
 *
 * Will yield a map (name will be yield later).
 * Staying in the same array.
 *
 * (O4)
 * cur:  test/_
 * next: test/_
 *
 * Will yield a value. (Value yield later)
 *
 * (O5)
 * cur:  test/_
 * next: test/_/# (or empty map/array)
 *
 * Will yield the name of the array (array handled later)
 *
 * (O6s O6m O6e)
 * cur:  test/_
 * next: test/_/_
 *
 * Will yield the name (O6s) of the map and the map (O6m)
 * if it is an empty map, do not yield a map (O6e)
 *
 * @pre
 * (P1) Precondition
 * cur:  test/_
 * next: test
 *
 * Is not possible.
 * @see elektraNextNotBelow
 *
 * @pre
 * (P2) Precondition
 * cur:  test
 * next: test/_
 *
 * Is not possible.
 * @see elektraGenOpenInitial
 *
 * (E1)
 * cur:  test/_
 * next: test/#
 *
 * Error situation: cannot change to array within map.
 * Lookahead does not matter.
 *
 * (E2)
 * cur:  test/#
 * next: test/_
 *
 * Error situation: leaving array in the middle.
 * Lookahead does not matter.
 *
 * Rationale for arrays:
 * Error situations should be handled by struct checker.
 * They could be avoided by encoding array names like
 * array#0, but this would lead to the possibility to create
 * non-unique names which would be kicked away in json.
 * It is considered that always generating correct files
 * is more important than not having the possibility to generate
 * wrong keysets for a particual storage.
 *
 * @param g
 * @param cur
 * @param next
 */
static void elektraGenOpenFirst (yajl_gen g, const char * cur, const char * next, size_t nextSize)
{
	lookahead_t lookahead = elektraLookahead (next, nextSize);
	ELEKTRA_LOG_DEBUG ("cur: \"%s\" next: \"%s\", lookahead: %d", cur, next, lookahead);

	if (*cur == '#')
	{
		if (*next == '#')
		{
			if (lookahead == LOOKAHEAD_MAP)
			{
				ELEKTRA_LOG_DEBUG ("GEN (O3) next anon map");
				yajl_gen_map_open (g);
			}
			else
			{
				ELEKTRA_LOG_DEBUG ("we are iterating over array, nothing to do");
			}
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("ERROR should not happen");
		}
	}
	else
	{
		if (lookahead == LOOKAHEAD_END)
		{
			ELEKTRA_LOG_DEBUG ("GEN string (O4)");
			yajl_gen_string (g, (const unsigned char *) next, nextSize);
		}
		else if (lookahead == LOOKAHEAD_ARRAY || lookahead == LOOKAHEAD_EMPTY_ARRAY || lookahead == LOOKAHEAD_EMPTY_MAP)
		{
			ELEKTRA_LOG_DEBUG ("GEN string for start/empty array (O5)");
			yajl_gen_string (g, (const unsigned char *) next, nextSize);
			// opening (empty) array will be handled later
		}
		else if (lookahead == LOOKAHEAD_MAP)
		{
			ELEKTRA_LOG_DEBUG ("GEN string (O6s)");
			yajl_gen_string (g, (const unsigned char *) next, nextSize);

			ELEKTRA_LOG_DEBUG ("GEN map (O6m)");
			yajl_gen_map_open (g);
		}
	}
}

/**
 * @brief open so many levels as needed for key next
 *
 * Iterates over next and generate
 * needed groups for every name/ and needed arrays
 * for every name/#.
 *
 * Yields names for leafs, but suppresses to yield names
 * for array entries (like #0)
 *
 * @see elektraGenOpenFirst
 *
 * @pre keys are not allowed to be below
 *
 * @example
 *
 * Example for elektraNextNotBelow:
 * cur:  user:/sw/org
 * next: user:/sw/org/deeper
 * -> do nothing, "deeper" is value
 *
 *  -- cut --
 *
 * cur:  user:/sw/org/deeper
 * next: user:/sw/org/other
 * -> this cannot happen (see elektraNextNotBelow)
 *
 * cur:  user:/sw/org/other
 * next: user:/sw/org/other/deeper/below
 * -> this cannot happen (see elektraNextNotBelow)
 *
 *  -- cut --
 *
 * instead of cut two entries above following would happen:
 * cur:  user:/sw/org/deeper
 * next: user:/sw/org/other/deeper/below
 * -> and "other" and "deeper" would be opened
 *
 * cur:  user:/sw/org/other/deeper/below
 * next: user:/no
 * -> do nothing, because "no" is value
 *
 * cur:  user:/no
 * next: user:/oops/it/is/below
 * -> create map "oops" "it" "is"
 *
 * cur:  user:/oops/it/is/below
 * next: user:/x/t/s/x
 * -> create "x" "t" "s"
 *
 * @example
 *
 * cur:  user:/sw/org/#0
 * next: user:/sw/org/#1
 * -> will not open org or array (because that did not change),
 *    but will open group test (because within arrays every key
 *    needs a group).
 *
 * cur:  user:/sw/org/#0
 * next: user:/sw/oth/#0
 * -> will open new group oth and new array and yield blah
 *
 * cur:  user:/sw
 * next: user:/sw/array/#0
 * -> will yield a new array using name "array"
 *
 * @pre cur and next have a name which is not equal
 *
 * @param g handle to generate to
 * @param cur current key of iteration
 * @param next next key of iteration
 */
void elektraGenOpen (yajl_gen g, const Key * cur, const Key * next)
{
	const char * pcur = keyName (cur);
	const char * pnext = keyName (next);
	// size_t curLevels = elektraKeyCountLevel(cur);
	size_t nextLevels = elektraKeyCountLevel (next);
	size_t size = 0;
	size_t csize = 0;

	size_t equalLevels = elektraKeyCountEqualLevel (cur, next);

	// forward all equal levels
	for (size_t i = 0; i < equalLevels + 1; ++i)
	{
		pnext = keyNameGetOneLevel (pnext + size, &size);
		pcur = keyNameGetOneLevel (pcur + csize, &csize);
	}

	// always skip first and last level
	const int levelsToSkip = 2;

	// calculate levels which are neither already handled
	// nor the last one
	int levels = (int) nextLevels - (int) (equalLevels + levelsToSkip);

	int actionRequired = equalLevels + 1 < nextLevels;

	ELEKTRA_LOG_DEBUG ("%d: pcur: %s , pnext: %s, action: %d", (int) levels, pcur, pnext, actionRequired);

	// check if anything needs to be done at all
	if (actionRequired)
	{
		elektraGenOpenFirst (g, pcur, pnext, size);

		// skip the first level we did already
		pnext = keyNameGetOneLevel (pnext + size, &size);

		// now yield everything else in the string but the last value
		elektraGenOpenIterate (g, pnext, levels);

		elektraGenOpenLast (g, next);
	}
}
