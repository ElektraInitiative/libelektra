#include "yajl.h"

#include <stdio.h>
#include <string.h>

#include <kdbconfig.h>
#include <kdberrors.h>
#include <yajl/yajl_gen.h>
#include <yajl/yajl_version.h>

#include "name.h"
#include "iterator.h"

#define ELEKTRA_YAJL_VERBOSE

typedef enum
{
	/**
	 * We are at end of string, so no lookahead.
	 */
	LOOKAHEAD_END,
	/**
	 * We are iterating in the middle of an array (or at least
	 * certainly not starting it)
	 */
	LOOKAHEAD_ARRAY,
	/**
	 * We are starting a new array.
	 */
	LOOKAHEAD_START_ARRAY,
	/**
	 * We found a special marker for empty arrays.
	 * @todo not implemented yet
	 */
	LOOKAHEAD_EMPTY_ARRAY,
	/**
	 * We found a special marker for empty maps.
	 * @todo not implemented yet
	 */
	LOOKAHEAD_EMPTY_MAP,
	/**
	 * We are iterating a map or starting it.
	 */
	LOOKAHEAD_MAP
} lookahead_t;

// TODO defined privately in keyhelpers.c, API break possible..
char *keyNameGetOneLevel(const char *name, size_t *size);

/**
 * @brief Return the first character of next name level
 *
 * @pre it must be safe to look at pnext+size
 * @pre string must be null terminated
 *
 * @param pnext pointer to current level
 * @param size size of name in that level
 *
 * @return #lookahead
 */
static lookahead_t elektraLookahead(const char* pnext, size_t size)
{
	lookahead_t lookahead = LOOKAHEAD_END; // found end
	if (*(pnext+size) == '/')
	{
		// we are not at end, so we can look one further
		if (*(pnext+size+1) == '#')
		{
			if (*(pnext+size+2) == '0')
			{
				lookahead = LOOKAHEAD_START_ARRAY;
			}
			else if (!strcmp(pnext+size, "###empty_array"))
			{
				lookahead = LOOKAHEAD_EMPTY_ARRAY;
			}
			else
			{
				lookahead = LOOKAHEAD_ARRAY;
			}
		}
		else
		{
			if (!strcmp(pnext+size, "___empty_map"))
			{
				lookahead = LOOKAHEAD_EMPTY_MAP;
			}
			else
			{
				lookahead = LOOKAHEAD_MAP;
			}
		}
	}

	// / and not NULL for nice printing
	return lookahead; // found End

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
 * _/#
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
static void elektraGenOpenIterate(yajl_gen g,
		const char *pnext,
		int levels)
{
	size_t size=0;

#ifdef ELEKTRA_YAJL_VERBOSE
	printf("elektraGenOpenIterate levels: %d,  next: \"%s\"\n",
			levels, pnext);
#endif

	for (int i=0; i<levels; ++i)
	{
		pnext=keyNameGetOneLevel(pnext+size,&size);

		lookahead_t lookahead = elektraLookahead(pnext, size);

#ifdef ELEKTRA_YAJL_VERBOSE
		printf("level by name %d: \"%.*s\", lookahead: %c\n",
				(int)i,
				(int)size, pnext,
				lookahead);
#endif

		// do not yield array indizes as names
		if (*pnext == '#')
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN (N1) array by name\n");
#endif
			yajl_gen_array_open(g);

			if (lookahead == LOOKAHEAD_MAP)
			{
#ifdef ELEKTRA_YAJL_VERBOSE
				printf ("GEN (N0) anon-map\n");
#endif
				yajl_gen_map_open(g);

			}
		}
		else if (lookahead == LOOKAHEAD_START_ARRAY)
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN (N2) string %.*s\n",
				(int)size, pnext);
#endif
			yajl_gen_string(g, (const unsigned char *)pnext,
					size);
		}
		else
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN (N3) string %.*s\n",
				(int)size, pnext);
#endif
			yajl_gen_string(g, (const unsigned char *)pnext,
					size);

#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN (N3) map by name\n");
#endif
			yajl_gen_map_open(g);
		}
	}
}

/**
 * @brief Implements special handling for last element of key name
 *
 * @pre g is in a map or array
 *
 * @post g need value as next step
 *
 * (L1)
 * #/_
 * If # is in the same counter stage, we just have to yield the name
 *
 * If # is in a new counter stage elektraGenOpenFirst already has
 * yield the map, so we also just have to yield the name
 *
 * (L2)
 * /#
 * does nothing (even for #/# the array was already done because of
 * lookahead)
 *
 * (L3)
 * /_
 * yields the name for the value
 *
 * @param g the generator
 * @param next the key
 */
static void elektraGenOpenLast(yajl_gen g, const Key *next)
{
	keyNameReverseIterator last =
		elektraKeyNameGetReverseIterator(next);
	elektraKeyNameReverseNext(&last);

#ifdef ELEKTRA_YAJL_VERBOSE
	printf("elektraGenOpenLast next: \"%.*s\"\n",
			(int)last.size, last.current);
#endif

	if (last.current[0] != '#')
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf("GEN string (L1,3)\n");
#endif
		yajl_gen_string(g,
			(const unsigned char *)last.current,
			last.size-1);
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
static void elektraGenOpenInitial(yajl_gen g, Key *parentKey,
		const Key *first)
{
	const char *pfirst = keyName(first);
	size_t csize=0;

	int equalLevels = elektraKeyCountEqualLevel(parentKey, first);
	int firstLevels = elektraKeyCountLevel(first);

	// forward all equal levels
	for (int i=0; i < equalLevels+1; ++i)
	{
		pfirst=keyNameGetOneLevel(pfirst+csize,&csize);
	}

	// calculate levels: do not iterate over last element
	const int levelsToOpen = firstLevels - equalLevels - 1;

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("open by name Initial %s, equal: %d, to open: %d\n",
			pfirst, equalLevels, levelsToOpen);
#endif


	if (pfirst && *pfirst == '#')
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf("array open INITIAL\n");
#endif
	}
	else
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf("GEN map open INITIAL\n");
#endif
		yajl_gen_map_open(g);
	}



	elektraGenOpenIterate(g, pfirst, levelsToOpen);

	// fixes elektraGenOpenIterate for the special handling of
	// arrays at startup
	keyNameReverseIterator last =
		elektraKeyNameGetReverseIterator(first);
	elektraKeyNameReverseNext(&last);

#ifdef ELEKTRA_YAJL_VERBOSE
	printf("last startup entry: \"%.*s\"\n",
			(int)last.size, last.current);
#endif

	if (last.current[0] == '#')
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf("GEN array open (startup)\n");
#endif
		yajl_gen_array_open(g);
	}
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
 * _ .. is a arbitrary map
 * # .. is a arbitrary member of array
 * Note that "test/" would not be contained in the string
 * given by argument.
 *
 * (1)
 * cur:  test/#
 * next: test/#
 *
 * Will do nothing, because we are in a situation where we just iterate
 * over leaves in the array.
 *
 * (2)
 * cur:  test/#
 * next: test/#/#
 *
 * Will create an array in the array.
 * Array won't have a name.
 * Staying in the same array "test".
 *
 * (3)
 * cur:  test/#
 * next: test/#/_
 *
 * Will yield a map (name will be yield later).
 * Staying in the same array.
 *
 * (4)
 * cur:  test/_
 * next: test/_
 *
 * Will yield a value. (Value yield later)
 *
 * (5)
 * cur:  test/_
 * next: test/_/#
 *
 * Will yield the name of the array (array handled later)
 *
 * (6)
 * cur:  test/_
 * next: test/_/_
 *
 * Will yield the name of the map and the map.
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
static void elektraGenOpenFirst(yajl_gen g,
		const char *cur,
		const char *next,
		size_t nextSize)
{
#ifdef ELEKTRA_YAJL_VERBOSE
	printf("elektraGenOpenFirst cur: \"%s\" next: \"%.*s\"\n",
			cur,
			(int)nextSize+2, next);
#endif

	if (*cur == '#')
	{
		if (*next == '#')
		{
			lookahead_t lookahead =
				elektraLookahead(next, nextSize);
			// see if we are at end
			if (lookahead == LOOKAHEAD_END)
			{
				// (1)
			}
			else if (lookahead == LOOKAHEAD_START_ARRAY)
			{
#ifdef ELEKTRA_YAJL_VERBOSE
				printf("GEN start array (2)\n");
#endif
				yajl_gen_array_open(g);
			}
			else if (lookahead == LOOKAHEAD_ARRAY)
			{
#ifdef ELEKTRA_YAJL_VERBOSE
				printf("GEN array (2)\n");
#endif
				yajl_gen_array_open(g);
			}
			else if (lookahead == LOOKAHEAD_MAP)
			{
#ifdef ELEKTRA_YAJL_VERBOSE
				printf("GEN map (3)\n");
#endif
				yajl_gen_map_open(g);
			}
		}
		else
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("ERROR should not happen");
#endif
			// TODO: handle by adding warning
		}
	}
	else
	{
		lookahead_t lookahead =
			elektraLookahead(next, nextSize);

		if (lookahead == LOOKAHEAD_END)
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("GEN string (4)\n");
#endif
			yajl_gen_string(g,
				(const unsigned char *)next,
				nextSize);
		}
		else if (lookahead == LOOKAHEAD_START_ARRAY)
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("GEN string for start array (5)\n");
#endif
			yajl_gen_string(g,
				(const unsigned char *)next,
				nextSize);
			// opening array will be handled later
		}
		else if (lookahead == LOOKAHEAD_MAP)
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("GEN string (6)\n");
#endif
			yajl_gen_string(g,
				(const unsigned char *)next,
				nextSize);

#ifdef ELEKTRA_YAJL_VERBOSE
			printf("GEN map (6)\n");
#endif
			yajl_gen_map_open(g);
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

	size_t equalLevels = elektraKeyCountEqualLevel(cur, next);

	// forward all equal levels
	for (size_t i=0; i < equalLevels+1; ++i)
	{
		pnext=keyNameGetOneLevel(pnext+size,&size);
		pcur=keyNameGetOneLevel(pcur+csize,&csize);
	}

	// always skip first and last level
	const int levelsToSkip = 2;

	// calculate levels which are neither already handled
	// nor the last one
	int levels = nextLevels - equalLevels - levelsToSkip;

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("elektraGenOpen %d: pcur: %s , pnext: %s\n",
		(int) levels, pcur, pnext);
#endif

	// do what needs to be done for first unequal level
	if (equalLevels+1 < nextLevels)
	{
		elektraGenOpenFirst(g, pcur, pnext, size);

		// one level more is done
		pnext=keyNameGetOneLevel(pnext+size,&size);

		// no more iterating afterwards, but we need to open
		// an array here
		if (levels <= 0 && pnext && *pnext == '#')
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN (S1) array after first\n");
#endif
			yajl_gen_array_open(g);
		}
	}

	// now yield everything else in the string but the last value
	elektraGenOpenIterate(g, pnext, levels);
}



/**
 * @brief Close given number of levels of key
 *
 * @pre there is some needed special handling at begin at end,
 * the caller needs to do that
 *
 * For the basename of cur nothing needs to be done
 * (it was either a value or an array entry)
 *
 * Then for every level do:
 *
 *
 * (C1)
 * #/_
 * (lookahead says it is a map in the array)
 * -> close the anonymous map and then the array
 *
 * (C2)
 * _/#
 * (lookahead says it is an array)
 * -> dont do anything
 *
 * (C3)
 * #
 * -> close the map
 *
 * (C4)
 * _
 * -> close the map
 *
 *
 * @param g to yield json information
 * @param cur the key which name is used for closing
 * @param levels the number of levels to close
 */
static void elektraGenCloseIterate(yajl_gen g, const Key *cur,
		int levels)
{
	keyNameReverseIterator curIt =
		elektraKeyNameGetReverseIterator(cur);

	// jump last element
	elektraKeyNameReverseNext(&curIt);

	for (int i=0; i<levels; ++i)
	{
		elektraKeyNameReverseNext(&curIt);

		lookahead_t lookahead = elektraLookahead(curIt.current,
				curIt.size);

		if (curIt.current[0] == '#')
		{
			if (lookahead == LOOKAHEAD_MAP)
			{
#ifdef ELEKTRA_YAJL_VERBOSE
				printf ("GEN (C1) anon map close\n");
#endif
				yajl_gen_map_close(g);

			}
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN (C3) array close\n");
#endif
			yajl_gen_array_close(g);
		}
		else
		{
			if (lookahead == LOOKAHEAD_MAP)
			{
#ifdef ELEKTRA_YAJL_VERBOSE
				printf ("GEN (C3) map close\n");
#endif
				yajl_gen_map_close(g);
			}
			else
			{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("(C2) array name: nothing to do\n");
#endif
			}
		}
	}
}

/**
 * @brief Special handling of cases related to closing in non-final
 * situation
 *
 * Following situations are possible:
 *
 * (X1)
 * cur:  #/#
 * next: #
 * -> closing array (always, because 2 arrays need to be closed)
 *
 * (X2)
 * cur:  #/_
 * next: #
 * -> array iteration, but with anon map
 *
 * (X3)
 * cur:  #
 * next: #
 * -> array iteration, so do not close array
 *
 * (X4)
 * cur:  _/#
 * next: _
 * -> closing map, but only if levels <= 0 (means iteration did nothing)
 *    (otherwise iteration already closed it)
 *
 * (X5)
 * cur:  _/_
 * next: _
 * -> closing map (only if levels <= 0, because iteration did not do it)
 *
 * (X6)
 * cur:  _
 * next: _
 * -> map iteration on same level, doing nothing
 *
 * @param g to generate to
 * @param pcur pointer to current name
 * @param csize size where cur has next level
 * @param pnext pointer to next name
 * @param levels how many levels were handled before (see examples
 * above)
 */
static void elektraGenCloseSpecial(yajl_gen g, const char* pcur,
		size_t csize,
		const char * pnext,
		int levels)
{
	lookahead_t lookahead = elektraLookahead(pcur, csize);
	if (*pcur == '#' && *pnext == '#')
	{
		if (lookahead == LOOKAHEAD_ARRAY)
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("GEN (X1) closing array in array\n");
#endif
			yajl_gen_array_close(g);
		}
		else if(lookahead == LOOKAHEAD_MAP)
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("GEN (X2) next anon-map\n");
#endif
			yajl_gen_map_close(g);
		}
		else
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("(X3) array iteration\n");
#endif
		}
	}
	else if (*pcur != '#')
	{
		if (levels <= 0 && lookahead == LOOKAHEAD_ARRAY)
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("GEN (X4) closing array\n");
#endif
			yajl_gen_array_close(g);
		}
		else if (lookahead == LOOKAHEAD_MAP)
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("GEN (X5) closing map\n");
#endif
			yajl_gen_map_close(g);
		}
		else
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf("(X6) same level iteration\n");
#endif
		}
	}
}

/**
 * @brief Close all levels in cur not needed in next
 *
 * Closing is much simpler then opening because no names need to be
 * yield.
 *
 * @pre keys are not allowed to be below,
 *      except: last run where everything below root/parent key is
 *      closed
 *
 * Then all levels are reverse iterated until the level before the equal
 * level.
 * @see elektraGenCloseIterate
 *
 * In the level before the equal level there is some special handling in
 * regards to the next level.
 * @see elektraGenCloseSpecial
 *
 * @example
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

	// 1 for last level not to iterate, 1 before 1 after equal
	int levels = curLevels - equalLevels - 2;

	const char *pcur = keyName(cur);
	size_t csize = 0;
	const char *pnext = keyName(next);
	size_t nsize = 0;
	for (int i=0; i < equalLevels+1; ++i)
	{
		pcur=keyNameGetOneLevel(pcur+csize,&csize);
		pnext=keyNameGetOneLevel(pnext+nsize, &nsize);
	}

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("elektraGenClose, eq: %d, cur: %s %d, next: %s %d, "
		"levels: %d\n",
			equalLevels,
			pcur, curLevels,
			pnext, nextLevels,
			levels);
#endif

	elektraGenCloseIterate(g, cur, levels);
	elektraGenCloseSpecial(g, pcur, csize, pnext, levels);
}

static void elektraGenCloseFinally(yajl_gen g, const Key *cur, const Key *next)
{
	int curLevels = elektraKeyCountLevel(cur);
#ifdef ELEKTRA_YAJL_VERBOSE
	int nextLevels = elektraKeyCountLevel(next);
#endif
	int equalLevels = elektraKeyCountEqualLevel(cur, next);

	// 1 for last level not to iterate, 1 after equal
	int levels = curLevels - equalLevels - 1;

	const char *pcur = keyName(cur);
	size_t csize = 0;
	const char *pnext = keyName(next);
	size_t nsize = 0;
	for (int i=0; i < equalLevels+1; ++i)
	{
		pcur=keyNameGetOneLevel(pcur+csize,&csize);
		pnext=keyNameGetOneLevel(pnext+nsize, &nsize);
	}

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("elektraGenFinally, eq: %d, cur: %s %d, next: %s %d, "
		"levels: %d\n",
			equalLevels,
			pcur, curLevels,
			pnext, nextLevels,
			levels);
#endif

	elektraGenCloseIterate(g, cur, levels);

	// fixes elektraCloseIterate for the special handling of
	// arrays finally
	keyNameReverseIterator last =
		elektraKeyNameGetReverseIterator(cur);
	elektraKeyNameReverseNext(&last);

#ifdef ELEKTRA_YAJL_VERBOSE
	printf("last startup entry: \"%.*s\"\n",
			(int)last.size, last.current);
#endif

	if (last.current[0] == '#')
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf("GEN array close (finally)\n");
#endif
		yajl_gen_array_close(g);
	}

	// now we look at the first unequal element
	// this is the very last element we are about to close
	if (pcur && *pcur == '#')
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("array close FINAL\n");
#endif
	}
	else
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("GEN map close FINAL\n");
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
	elektraGenOpenLast(g, cur);

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
	else if (!strcmp(keyString(type), "number")) // TODO: distuingish between float and int (parser too)
	{
		yajl_gen_number(g, keyString(cur), keyGetValueSize(cur)-1);
	}
	else { // unknown or unsupported type, render it as string but add warning
		ELEKTRA_ADD_WARNING(78, parentKey, keyString(type));
		yajl_gen_string(g, (const unsigned char *)keyString(cur), keyGetValueSize(cur)-1);
	}
}

int elektraYajlGenEmpty(yajl_gen g, KeySet *returned, Key *parentKey)
{
	int did_something = 0;
	// TODO: do all these situations actually occur?
	if (ksGetSize(returned) == 0) // we got nothing..
	{
#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("GEN empty map (got nothing)\n");
#endif
		yajl_gen_map_open(g);
		yajl_gen_map_close(g);
		did_something = 1;
	}
	else if (ksGetSize(returned) == 1) // maybe just parentKey
	{
		if (!strcmp(keyName(ksTail(returned)), keyName(parentKey)))
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN empty map (got parent)\n");
#endif
			yajl_gen_map_open(g);
			yajl_gen_map_close(g);
			did_something = 1;
		}
	}
	else
	{
		if (!strcmp(keyBaseName(ksTail(returned)), "###empty_array"))
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN empty array (got ###empty_array)\n");
#endif
			yajl_gen_array_open(g);
			yajl_gen_array_close(g);
			did_something = 1;
		}
		else if (!strcmp(keyBaseName(ksTail(returned)), "___empty_map"))
		{
#ifdef ELEKTRA_YAJL_VERBOSE
			printf ("GEN empty map (got ___empty_map)\n");
#endif
			yajl_gen_map_open(g);
			yajl_gen_map_close(g);
			did_something = 1;
		}
	}
	
	return did_something;
}

int elektraGenWriteFile(yajl_gen g, Key *parentKey)
{
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

	fclose (fp);

	return 1; /* success */
}

int elektraYajlSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
#if YAJL_MAJOR == 1
	yajl_gen_config conf = { 1, "  " };
	yajl_gen g = yajl_gen_alloc(&conf, NULL
#else
	// TODO: do this by config
	yajl_gen g = yajl_gen_alloc(NULL);
	yajl_gen_config(g, yajl_gen_beautify, 1);
	yajl_gen_config(g, yajl_gen_validate_utf8, 1);
#endif

	ksRewind (returned);
	Key *cur = elektraNextNotBelow(returned);
	if (!cur)
	{
		// empty config should be handled by resolver
		// (e.g. remove file)
		yajl_gen_free(g);
		return 0;
	}

	if (elektraYajlGenEmpty(g, returned, parentKey))
	{
		int ret = elektraGenWriteFile(g, parentKey);
		yajl_gen_free(g);
		return ret;
	}

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parentKey: %s, cur: %s\n", keyName(parentKey), keyName(cur));
#endif
	elektraGenOpenInitial(g, parentKey, cur);

	Key *next = 0;
	while ((next = elektraNextNotBelow(returned)) != 0)
	{
		elektraGenValue(g, parentKey, cur);
		elektraGenClose(g, cur, next);

#ifdef ELEKTRA_YAJL_VERBOSE
		printf ("\nITERATE: %s next: %s\n", keyName(cur), keyName(next));
#endif
		elektraGenOpen(g, cur, next);

		cur = next;
	}

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("\nleaving loop: %s\n", keyName(cur));
#endif

	elektraGenValue(g, parentKey, cur);

	elektraGenCloseFinally(g, cur, parentKey);

	int ret = elektraGenWriteFile(g, parentKey);
	yajl_gen_free(g);

	return ret;
}
