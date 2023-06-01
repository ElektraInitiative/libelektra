/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./yajl_gen.h"

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
 * @brief fixes elektraGenCloseIterate for the special handling of
 * arrays at very last position.
 *
 * @param g generate array there
 * @param key the key to look at
 */
static void elektraGenCloseLast (yajl_gen g, const Key * key)
{
	keyNameReverseIterator last = elektraKeyNameGetReverseIterator (key);
	elektraKeyNameReverseNext (&last);

	ELEKTRA_LOG_DEBUG ("last startup entry: \"%.*s\"", (int) last.size, last.current);

	if (last.current[0] == '#' && strcmp (last.current, "###empty_array") != 0)
	{
		ELEKTRA_LOG_DEBUG ("GEN array close last");
		yajl_gen_array_close (g);
	}
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
 * _/___empty_map
 * (lookahead says it is not a map)
 * -> don't do anything
 *
 * (C3)
 * #
 * -> close the array
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
static void elektraGenCloseIterate (yajl_gen g, const Key * cur, int levels)
{
	keyNameReverseIterator curIt = elektraKeyNameGetReverseIterator (cur);

	// jump last element
	elektraKeyNameReverseNext (&curIt);

	for (int i = 0; i < levels; ++i)
	{
		elektraKeyNameReverseNext (&curIt);

		lookahead_t lookahead = elektraLookahead (curIt.current, curIt.size);

		if (curIt.current[0] == '#')
		{
			if (lookahead == LOOKAHEAD_MAP)
			{
				ELEKTRA_LOG_DEBUG ("GEN (C1) anon map close");
				yajl_gen_map_close (g);
			}
			ELEKTRA_LOG_DEBUG ("GEN (C3) array close");
			yajl_gen_array_close (g);
		}
		else
		{
			if (lookahead == LOOKAHEAD_MAP)
			{
				ELEKTRA_LOG_DEBUG ("GEN (C4) map close");
				yajl_gen_map_close (g);
			}
			else
			{
				ELEKTRA_LOG_DEBUG ("(C2) lookahead not a map: nothing to do");
			}
		}
	}
}

/**
 * @brief Special handling of cases related to closing in non-final
 * situation.
 *
 * Closes the name in the middle (first unequal), so it must be executed
 * after iterating. (Because closing is in reverse order)
 *
 * Following situations are possible:
 *
 * (X1)
 * cur:  #/#
 * next: #
 * -> closing array (only if levels <0, because it might be outside
 *  array with iterating one level deeper)
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
static void elektraGenCloseFirst (yajl_gen g, const char * pcur, size_t csize, const char * pnext, int levels)
{
	lookahead_t lookahead = elektraLookahead (pcur, csize);
	ELEKTRA_LOG_DEBUG ("%s -> %s, levels: %d, lookahead: %d", pcur, pnext, levels, lookahead);
	if (*pcur == '#' && *pnext == '#')
	{
		if (levels <= 0 && lookahead == LOOKAHEAD_ARRAY)
		{
			ELEKTRA_LOG_DEBUG ("GEN (X1) closing array in array");
			yajl_gen_array_close (g);
		}
		else if (lookahead == LOOKAHEAD_MAP)
		{
			ELEKTRA_LOG_DEBUG ("GEN (X2) next anon-map");
			yajl_gen_map_close (g);
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("(X3) array iteration");
		}
	}
	else if (*pcur != '#')
	{
		if (levels <= 0 && lookahead == LOOKAHEAD_ARRAY)
		{
			ELEKTRA_LOG_DEBUG ("GEN (X4) closing array");
			yajl_gen_array_close (g);
		}
		else if (lookahead == LOOKAHEAD_MAP)
		{
			ELEKTRA_LOG_DEBUG ("GEN (X5) closing map");
			yajl_gen_map_close (g);
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("(X6) same level iteration");
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
 * @see elektraGenCloseFirst
 *
 * @example
 *
 * cur:  user:/sw/org/deeper
 * next: user:/sw/org/other/deeper/below
 * -> nothing will be done ("deeper" is value)
 * [eq: 3, cur: 4, next: 6, gen: 0]
 *
 * cur:  user:/sw/org/other/deeper/below
 * next: user:/no
 * -> "deeper", "other", "org" and "sw" maps will be closed ("below" is value)
 * [eq: 1, cur: 6, next: 2, gen: 4]
 *
 * cur:  user:/no
 * next: user:/oops/it/is/below
 * -> nothing will be done ("no" is value)
 * [eq: 1, cur: 2, next: 5, gen: 0]
 *
 * cur:  user:/oops/it/is/below
 * next: user:/x/t/s/x
 * -> close "is", "it", "oops"
 * [eq: 1, cur: 5, next: 5, gen: 3]
 *
 * last iteration (e.g. close down to root)
 * cur:  user:/x/t/s/x
 * next: user
 * -> close "s", "t" and "x" maps
 * [eq: 1, cur: 5, next: 1, gen: 3]
 *
 * cur:  user:/#0/1/1/1
 * next: user:/#1/1/1/1
 * -> close "1", "1", "1", but not array
 * [eq: 1, cur: 5, next: 5, gen: 3]
 *
 * @param g
 * @param cur
 * @param next
 */
void elektraGenClose (yajl_gen g, const Key * cur, const Key * next)
{
	int curLevels = elektraKeyCountLevel (cur);
#ifdef HAVE_LOGGER
	int nextLevels = elektraKeyCountLevel (next);
#endif
	int equalLevels = elektraKeyCountEqualLevel (cur, next);

	// 1 for last level not to iterate, 1 before 1 after equal
	int levels = curLevels - equalLevels - 2;

	const char * pcur = keyName (cur);
	size_t csize = 0;
	const char * pnext = keyName (next);
	size_t nsize = 0;
	for (int i = 0; i < equalLevels + 1; ++i)
	{
		pcur = keyNameGetOneLevel (pcur + csize, &csize);
		pnext = keyNameGetOneLevel (pnext + nsize, &nsize);
	}


	ELEKTRA_LOG_DEBUG ("eq: %d, cur: %s %d, next: %s %d, levels: %d", equalLevels, pcur, curLevels, pnext, nextLevels, levels);

	if (levels > 0)
	{
		elektraGenCloseLast (g, cur);
	}
	elektraGenCloseIterate (g, cur, levels);
	elektraGenCloseFirst (g, pcur, csize, pnext, levels);
}

/**
 * @brief Close the last element
 *
 * Needs less special handling because cur is fully below next.
 *
 * Will fully iterate over all elements.
 *
 * @param g handle to yield close events
 * @param cur current key
 * @param next the last key (the parentKey)
 */
void elektraGenCloseFinally (yajl_gen g, const Key * cur, const Key * next)
{
	int curLevels = elektraKeyCountLevel (cur);
#ifdef HAVE_LOGGER
	int nextLevels = elektraKeyCountLevel (next);
#endif
	int equalLevels = elektraKeyCountEqualLevel (cur, next);

	// 1 for last level not to iterate, 1 after equal
	int levels = curLevels - equalLevels - 1;

	const char * pcur = keyName (cur);
	size_t csize = 0;
	const char * pnext = keyName (next);
	size_t nsize = 0;
	for (int i = 0; i < equalLevels + 1; ++i)
	{
		pcur = keyNameGetOneLevel (pcur + csize, &csize);
		pnext = keyNameGetOneLevel (pnext + nsize, &nsize);
	}

	ELEKTRA_LOG_DEBUG ("eq: %d, cur: %s %d, next: %s %d, levels: %d", equalLevels, pcur, curLevels, pnext, nextLevels, levels);
	// fixes elektraGenCloseIterate for the special handling of
	// arrays finally
	elektraGenCloseLast (g, cur);

	// now we iterate over the middle part
	elektraGenCloseIterate (g, cur, levels);

	// now we look at the first unequal element
	// this is the very last element we are about to close
	if (pcur && *pcur == '#')
	{
		ELEKTRA_LOG_DEBUG ("array close FINAL");
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("GEN map close FINAL");
		yajl_gen_map_close (g);
	}
}
