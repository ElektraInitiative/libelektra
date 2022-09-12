/**
 * @file
 *
 * @brief Methods for metadata manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <kdbconfig.h>
#include <kdbease.h>
#include <kdbmeta.h>
#include <kdbprivate.h>
#include <kdbtypes.h>
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif


/**
 * @defgroup meta Meta Data proposal+compatibility
 * @brief Meta data proposal+compatibility methods.
 * @ingroup proposal
 *
 * In versions before Elektra 0.8 only limited metadata was
 * available. Now any metadata can be added. These API methods are
 * implementations of the 0.7 API using 0.8 metadata.
 *
 * Additionally, new suggestions can be made here.
 *
 * It is planned that these methods will be generated from doc/METADATA.ini
 * and moved to a separate library.
 * Currently, you should better avoid the methods and directly use  @link keymeta metainfo @endlink
 * instead.
 *
 * @{
 *
 */


/*********************************************
 *    General comment manipulation methods   *
 *********************************************/


/**
 * Return a pointer to the real internal @p key comment.
 *
 * This is a much more efficient version of keyGetComment() and you
 * should use it if you are responsible enough to not mess up things.
 * You are not allowed to change anything in the memory region the
 * returned pointer points to.
 *
 * keyComment() returns "" when there is no keyComment. The reason is
 * @code
 key=keyNew(0);
 keySetComment(key,"");
 keyComment(key); // you would expect "" here
 keyDel(key);
 * @endcode
 *
 * See keySetComment() for more information on comments.
 *
 * @note Note that the Key structure keeps its own size field that is calculated
 * by library internal calls, so to avoid inconsistencies, you
 * must never use the pointer returned by keyComment() method to set a new
 * value. Use keySetComment() instead.
 *
 * @param key the key object to work with
 * @return a pointer to the internal managed comment
 * @retval "" when there is no comment
 * @retval 0 on NULL pointer
 * @see keyGetCommentSize() for size and keyGetComment() as alternative
 */
const char * keyComment (const ElektraKey * key)
{
	const char * comment;

	if (!key) return 0;
	comment = elektraKeyValue (elektraKeyGetMeta (key, "comment"));

	if (!comment)
	{
		return "";
	}

	return comment;
}


/**
 * Calculates number of bytes needed to store a key comment, including
 * final NULL.
 *
 * Use this method to know to size for allocated memory to retrieve
 * a key comment.
 *
 * See keySetComment() for more information on comments.
 *
 * For an empty key name you need one byte to store the ending NULL.
 * For that reason 1 is returned.
 *
 * @code
 char *buffer;
 buffer = elektraMalloc (keyGetCommentSize (key));
// use this buffer to store the comment
// pass keyGetCommentSize (key) for maxSize
 * @endcode
 *
 * @param key the key object to work with
 * @return number of bytes needed
 * @retval 1 if there is no comment
 * @retval -1 on NULL pointer
 * @see keyGetComment(), keySetComment()
 */
ssize_t keyGetCommentSize (const ElektraKey * key)
{
	ssize_t size;
	if (!key) return -1;

	size = elektraKeyGetValueSize (elektraKeyGetMeta (key, "comment"));

	if (!size || size == -1)
	{
		return 1;
	}

	return size;
}


/**
 * Get the key comment.
 *
 * @section comment Comments
 *
 * A Key comment is description for humans what this key is for. It may be a
 * textual explanation of valid values, when and why a user or administrator
 * changed the key or any other text that helps the user or administrator related
 * to that key.
 *
 * Don't depend on a comment in your program. A user is
 * always allowed to remove or change it in any way he wants to. But you are
 * allowed or even encouraged to always show the content of the comment
 * to the user and allow him to change it.
 *
 * @param key the key object to work with
 * @param returnedComment pre-allocated memory to copy the comments to
 * @param maxSize number of bytes that will fit returnedComment
 * @return the number of bytes actually copied to @p returnedString, including
 * 	final NULL
 * @retval 1 if the string is empty
 * @retval -1 on NULL pointer
 * @retval -1 if maxSize is 0, not enough to store the comment or when larger then SSIZE_MAX
 * @see keyGetCommentSize(), keySetComment()
 */
ssize_t keyGetComment (const ElektraKey * key, char * returnedComment, size_t maxSize)
{
	const char * comment;
	size_t commentSize;
	if (!key) return -1;

	if (!maxSize) return -1;
	if (!returnedComment) return -1;
	if (maxSize > SSIZE_MAX) return -1;

	comment = elektraKeyValue (elektraKeyGetMeta (key, "comment"));
	commentSize = elektraKeyGetValueSize (elektraKeyGetMeta (key, "comment"));

	if (!comment)
	{
		returnedComment[0] = 0;
		return 1;
	}

	strncpy (returnedComment, comment, maxSize);
	if (maxSize < commentSize)
	{
		return -1;
	}
	return commentSize;
}


/**
 * Set a comment for a key.
 *
 * A key comment is like a configuration file comment.
 * See keySetComment() for more information.
 *
 * @param key the key object to work with
 * @param newComment the comment, that can be freed after this call.
 * @return the number of bytes actually saved including final NULL
 * @retval 0 when the comment was freed (newComment NULL or empty string)
 * @retval -1 on NULL pointer or memory problems
 * @see keyGetComment()
 */
ssize_t keySetComment (ElektraKey * key, const char * newComment)
{
	if (!key) return -1;
	if (!newComment || *newComment == 0)
	{
		elektraKeySetMeta (key, "comment", 0);
		return 1;
	}

	return elektraKeySetMeta (key, "comment", newComment);
}

/**
 * Compare the order metadata of two keys.
 *
 * @return a number less than, equal to or greater than zero if
 *    the order of k1 is found, respectively, to be less than,
 *    to match, or be greater than the order of k2. If one key is
 *    NULL, but the other isn't, the key which is not NULL is considered
 *    to be greater. If both keys are NULL, they are
 *    considered to be equal. If one key does have an order
 *    metadata but the other has not, the key with the metadata
 *    is considered greater. If no key has metadata,
 *    they are considered to be equal.
 *
 * @param ka key to compare with
 * @param kb other key to compare with
 */
int elektraKeyCmpOrder (const ElektraKey * ka, const ElektraKey * kb)
{

	if (ka == NULL && kb == NULL)
	{
		return 0;
	}

	if (ka != NULL && kb == NULL)
	{
		return 1;
	}

	if (ka == NULL && kb != NULL)
	{
		return -1;
	}

	const ElektraKey * kam = elektraKeyGetMeta (ka, "order");
	const ElektraKey * kbm = elektraKeyGetMeta (kb, "order");

	if (kam == NULL && kbm == NULL)
	{
		return 0;
	}
	if (kam != NULL && kbm == NULL)
	{
		return 1;
	}
	if (kam == NULL && kbm != NULL)
	{
		return -1;
	}

	return atoi (elektraKeyString (kam)) - atoi (elektraKeyString (kbm));
}


/**
 * creates an metadata array or appends another element to an existing metadata array
 * e.g.
 * Key *key = keyNew("user:/test", KEY_END);
 * elektraMetaArrayAdd(key, "test", "val0");
 * key now has "test/#0" with value "val0" as metadata
 * elektraMetaArrayAdd(key, "test", "val1");
 * appends "test/#1" with value "val1" to key
 *
 * @param key the key the metadata should be added to
 * @param metaName the name of the metakey array parent
 * @param value the value of the newly appended metakey
 */

void elektraMetaArrayAdd (ElektraKey * key, const char * metaName, const char * value)
{
	const ElektraKey * meta = elektraKeyGetMeta (key, metaName);
	ElektraKey * arrayKey;
	if (!meta)
	{
		elektraKeySetMeta (key, metaName, "#0");
		arrayKey = elektraKeyDup (elektraKeyGetMeta (key, metaName), ELEKTRA_KEY_CP_NAME);
		elektraKeySetString (arrayKey, 0);
		elektraKeyAddBaseName (arrayKey, "#");
	}
	else
	{
		arrayKey = elektraKeyDup (meta, ELEKTRA_KEY_CP_NAME);
		elektraKeyAddBaseName (arrayKey, elektraKeyString (meta));
	}
	elektraArrayIncName (arrayKey);
	const char * arrayName = elektraKeyName (arrayKey) + sizeof ("meta:/") - 1;
	elektraKeySetMeta (key, arrayName, value);
	elektraKeySetMeta (key, metaName, elektraKeyBaseName (arrayKey));
	elektraKeyDel (arrayKey);
}

/**
 * Create a `KeySet` from a metakey array.
 *
 * For example, the following function call
 *
 * @code
elektraMetaArrayToKS(
	keyNew ("/a", KEY_VALUE, "b, c",
		KEY_META, "dep",    "#1",
		KEY_META, "dep/#0", "/b",
		KEY_META, "dep/#1", "/c", KEY_END),
	"dep");
 * @endcode
 *
 * returns a `KeySet` containing the keys `dep` with value `#1`, `"dep/#0"` with value `"/b"` and
 * `"dep/#1"` with value `"/c"`.
 *
 * If no meta key array is found, null is returned.
 * The returned `KeySet` must be freed with `ksDel`
 *
 * @returns a keyset containing all the metakeys of the metakey array
 * @param key the key containing the metakey array
 * @param metaName the name of the metakey array parent
 */
ElektraKeyset * elektraMetaArrayToKS (ElektraKey * key, const char * metaName)
{
	const ElektraKey * meta = elektraKeyGetMeta (key, metaName);
	if (!meta) return NULL;

	ElektraKeyset * result;
	if (elektraKeyString (meta)[0] != '#')
	{
		result = elektraKeysetNew (1, meta, ELEKTRA_KS_END);
	}
	else
	{
		result = elektraArrayGet (meta, elektraKeyMeta (key));
		elektraKeysetAppendKey (result, (ElektraKey *) meta);
	}

	elektraKeysetRewind (result);
	return result;
}


/**
 * @internal
 *
 * elektraSortTopology helper
 * matrix struct
 */
typedef struct
{
	ElektraKey * key;
	kdb_octet_t isResolved;
	unsigned long * deps;
} _adjMatrix;

/**
 * @internal
 *
 * elektraSortTopology helper
 * ordering function for qsort
 */
static int topCmpOrder (const void * a, const void * b)
{
	const ElektraKey * ka = (*(const ElektraKey **) a);
	const ElektraKey * kb = (*(const ElektraKey **) b);

	if (!ka && !kb) return 0;
	if (ka && !kb) return 1;
	if (!ka && kb) return -1;

	const ElektraKey * kam = elektraKeyGetMeta (ka, "order");
	const ElektraKey * kbm = elektraKeyGetMeta (kb, "order");

	return strcmp (elektraKeyString (kam), elektraKeyString (kbm));
}


/**
 * @internal
 *
 * elektraSortTopology helper
 * returns the index of dependency depKey
 */
static int getArrayIndex (ElektraKey * depKey, _adjMatrix * adjMatrix, size_t size)
{
	for (unsigned int i = 0; i < size; ++i)
	{
		if (!strcmp (elektraKeyName (adjMatrix[i].key), elektraKeyString (depKey))) return i;
	}
	return -1;
}
/**
 * @internal
 *
 * elektraSortTopology helper
 * removes resolved dependency j from our matrix
 */
static int resolveDep (unsigned int j, _adjMatrix * adjMatrix, size_t size)
{
	int removed = 0;
	for (unsigned int i = 0; i < size; ++i)
	{
		if (adjMatrix[i].deps[j])
		{
			++removed;
			adjMatrix[i].deps[j] = 0;
		}
	}
	return removed;
}

/**
 * @internal
 *
 * elektraSortTopology helper
 * checks if the key with index j has unresolved dependencies
 */
static int hasUnresolvedDependencies (unsigned int j, _adjMatrix * adjMatrix, size_t size)
{
	for (unsigned int i = 0; i < size; ++i)
	{
		if (adjMatrix[j].deps[i]) return 1;
	}
	return 0;
}

/**
 * @internal
 *
 * elektraSortTopology helper
 * resolve all dependencies of the key with the index j in our matrix.
 */
static int resolveDeps (unsigned int j, _adjMatrix * adjMatrix, size_t size, ElektraKeyset * done, ElektraKey * orderCounter)
{
	unsigned int loops = 0;
	unsigned int frontier[size];
	unsigned int todo = 0;
	for (unsigned int i = 0; i < size; ++i)
	{
		if (adjMatrix[j].deps[i])
		{
			frontier[i] = 1;
			++todo;
		}
		else
		{
			frontier[i] = 0;
		}
	}
	int found = 1;

	// loop until all dependencies are added to frontier
	while (found)
	{
		found = 0;
		for (unsigned int i = 0; i < size; ++i)
		{
			if (!frontier[i]) continue;
			if (hasUnresolvedDependencies (i, adjMatrix, size))
			{
				for (unsigned int k = 0; k < size; ++k)
				{
					if (adjMatrix[i].deps[k])
					{
						if (!frontier[k])
						{
							found = 1;
							++todo;
							frontier[k] = 1;
						}
					}
				}
			}
		}
	}
	if (todo == 0)
	{
		// all dependencies are already resolved, give key an order number and add it to
		// the our list of resolved keys (done)
		adjMatrix[j].isResolved = 1;
		resolveDep (j, adjMatrix, size);
		elektraKeySetMeta (adjMatrix[j].key, "order", elektraKeyBaseName (orderCounter));
		elektraArrayIncName (orderCounter);
		elektraKeysetAppendKey (done, elektraKeyDup (adjMatrix[j].key, ELEKTRA_KEY_CP_ALL));
		return 1;
	}
	unsigned int max_loops = todo;
	for (unsigned int i = 0; todo; ++i)
	{
		if (i == size)
		{
			++loops;
			i = 0;
		}
		if (loops > max_loops) return -1; // more loops than we had unresolved keys -> cycle
		if (!frontier[i]) continue;
		if (!hasUnresolvedDependencies (i, adjMatrix, size))
		{
			resolveDep (i, adjMatrix, size);
			frontier[i] = 0;
			--todo;
			adjMatrix[i].isResolved = 1;
			resolveDep (i, adjMatrix, size);
			elektraKeySetMeta (adjMatrix[i].key, "order", elektraKeyBaseName (orderCounter));
			elektraArrayIncName (orderCounter);
			elektraKeysetAppendKey (done, elektraKeyDup (adjMatrix[i].key, ELEKTRA_KEY_CP_ALL));
		}
	}
	return 1;
}

/**
 * elektraSortTopology helper
 * tests if name is a valid keyname
 */
static int isValidKeyName (const char * testName)
{
	int retVal = 0;
	ElektraKey * testKey = elektraKeyNew (testName, ELEKTRA_KEY_END);
	if (testKey && !strcmp (elektraKeyName (testKey), testName)) retVal = 1;
	elektraKeyDel (testKey);
	return retVal;
}

/**
 * @brief topological sorting
 *
 * @param array the array where the sorted keys will be stored in topological order.
 *        Nothing will be written into an array if
 * @param ks is the keyset that should be sorted.
 *        Dependencies and order is defined by metakeys.
 *
 * - the "dep/#" metakeys
 *  e.g. the Key *k = keyNew ("/a", KEY_VALUE, "b, c",
 *  KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END), "dep");
 *  depends on Key "/b" and Key "/c".
 * - if "order" metakeys are defined for the keys the algorithm tries to resolves them by that
 *  order using lexical comparison. You should prefer `#0` array syntax.
 *
 * Duplicated and reflexive dep entries are ignored.
 *
 * The algorithm used is a mixture of Kahn and BFS.
 * Furthermore the algorithm does not use recursion.
 *
 * First a BFS with the keys sorted by "order" is used.
 * Then all dependencies (recursively) of every key is collected.
 *
 * @retval 1 on success
 * @retval 0 for cycles
 * @retval -1 for invalid dependencies
 */

int elektraSortTopology (ElektraKeyset * ks, ElektraKey ** array)
{
	if (ks == NULL || array == NULL) return -1;
	ElektraKeyset * done = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetRewind (ks);
	ElektraKey * cur;
	ssize_t size = elektraKeysetGetSize (ks);
	ElektraKey * orderCounter = elektraKeyNew ("/#", ELEKTRA_KEY_END);
	elektraArrayIncName (orderCounter);
	_adjMatrix adjMatrix[size];
	int i = 0;
	int retVal = 1;
	int depCount = 0;
	ElektraKey ** localArray = elektraMalloc (size * sizeof (ElektraKey *));
	elektraKsToMemArray (ks, localArray);
	qsort (localArray, size, sizeof (ElektraKey *), topCmpOrder);
	for (long j = 0; j < size; ++j)
	{
		adjMatrix[j].key = localArray[j];
		adjMatrix[j].isResolved = 0;
		adjMatrix[j].deps = elektraCalloc (sizeof (unsigned long) * size);
	}
	kdb_octet_t hasOrder = 0;
	if (elektraKeyGetMeta (localArray[0], "order")) hasOrder = 1;
	unsigned int unresolved = 0;
	for (int j = 0; j < size; ++j)
	{
		cur = localArray[j];
		ElektraKeyset * deps = elektraMetaArrayToKS (cur, "dep");
		elektraKeysetLookupByName (deps, "meta:/dep", ELEKTRA_KDB_O_POP);
		ElektraKey * tmpDep;
		switch (elektraKeysetGetSize (deps))
		{
		case -1: {
			// key has no dependencies, give it an order number and add it to list of resolved dependencies
			elektraKeySetMeta (cur, "order", elektraKeyBaseName (orderCounter));
			elektraArrayIncName (orderCounter);
			elektraKeysetAppendKey (done, elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL));
			adjMatrix[j].isResolved = 1;
			elektraKeysetDel (deps);
			break;
		}
		case 1: {
			// only 1 dependency:
			// test if it's reflexive
			tmpDep = elektraKeysetHead (deps);
			if (!strcmp (elektraKeyName (cur), elektraKeyString (tmpDep)))
			{
				elektraKeySetMeta (cur, "order", elektraKeyBaseName (orderCounter));
				elektraArrayIncName (orderCounter);
				elektraKeysetAppendKey (done, elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL));
				adjMatrix[j].isResolved = 1;
				elektraKeysetDel (deps);
				break;
			}
			// if not, fallthrough to normal dependency handling
		}
		// FALLTHROUGH
		default: {
			int gotUnresolved = 0;
			while ((tmpDep = elektraKeysetNext (deps)) != NULL)
			{
				if (!isValidKeyName (elektraKeyString (tmpDep)))
				{
					// invalid keyname -> ERROR
					retVal = -1;
					break;
				}
				i = getArrayIndex (tmpDep, adjMatrix, size);
				if (i == -1)
				{
					// key doesn't exist yet but has valid name, ignore it.
					continue;
				}
				else if (i == j)
				{
					// reflexiv dependency, do nothing
				}
				else
				{
					if (!adjMatrix[i].isResolved)
					{
						// unresolved dependency
						adjMatrix[j].deps[i] = 1;
						++gotUnresolved;
						// simple cycle detection
						if (adjMatrix[i].deps[j])
						{
							retVal = 0;
							break;
						}
					}
				}
			}
			if (gotUnresolved)
			{
				adjMatrix[j].isResolved = 0;
				++unresolved;
				// count unresolved dependencies
				depCount += gotUnresolved;
			}
			elektraKeysetDel (deps);
			break;
		}
		}
		if (retVal <= 0) break;
	}
	if (retVal <= 0)
	{
		// error or cycle: goto cleanup
		goto TopSortCleanup;
	}

	// resolve all dependencies that can be resolved immediately
	for (int j = 0; j < size; ++j)
	{
		if (adjMatrix[j].isResolved) depCount -= resolveDep (j, adjMatrix, size);
	}

	ssize_t resolved = elektraKeysetGetSize (done);
	if (((depCount + resolved) >= size) && (unresolved))
	{
		// more dependencies dependencies than keys:
		//  cycle found !
		retVal = 0;
		goto TopSortCleanup;
	}

	if (unresolved)
	{
		int found = 1;
		// we have unresolved dependencies
		for (int j = 0; j < size + 1; ++j)
		{
			// loop until no dependency can be resolved anymore
			if (j == size)
			{
				if (found)
				{
					found = 0;
					j = -1;
					unresolved = 0;
					continue;
				}
				else
					break;
			}
			if (adjMatrix[j].isResolved) continue;
			++unresolved;
			if (hasOrder)
			{
				// resolve by order
				int ret = resolveDeps (j, adjMatrix, size, done, orderCounter);
				if (ret == -1) break;
				j = -1;
				found = 1;
				continue;
			}
			else
			{
				// resolve next possible dependency in keyset
				if (!hasUnresolvedDependencies (j, adjMatrix, size))
				{
					adjMatrix[j].isResolved = 1;
					resolveDep (j, adjMatrix, size);
					elektraKeySetMeta (localArray[j], "order", elektraKeyBaseName (orderCounter));
					elektraArrayIncName (orderCounter);
					elektraKeysetAppendKey (done, elektraKeyDup (localArray[j], ELEKTRA_KEY_CP_ALL));
					found = 1;
				}
			}
		}
	}
	if (unresolved == 0)
	{
		// everything resolved
		// add dependencies in topological order to array
		elektraKsToMemArray (ks, array);
		qsort (array, size, sizeof (ElektraKey *), topCmpOrder);
		retVal = 1;
	}
	else
	{
		// still unresolved dependencies left:
		// there must be a cycle somewhere
		retVal = 0;
	}
TopSortCleanup:
	elektraKeysetDel (done);
	elektraKeyDel (orderCounter);
	elektraFree (localArray);
	for (ssize_t j = 0; j < size; ++j)
	{
		elektraFree (adjMatrix[j].deps);
	}
	return retVal;
}

/**
 * returns the metakey array as a string separated by delim
 *
 * @param key the key containing the metakey array
 * @param metaName the name of the metakey array parent
 * @param delim delimiter for the records in the returned string
 *
 * @returns a string containing all metakey values separated by "delim"
 */

char * elektraMetaArrayToString (const ElektraKey * key, const char * metaName, const char * delim)
{
	char * result = NULL;
	ElektraKey * lookupElem = elektraKeyDup (elektraKeyGetMeta (key, metaName), ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (lookupElem, "#0");
	ElektraKey * elem = (ElektraKey *) elektraKeyGetMeta (key, elektraKeyName (lookupElem));
	if (elem != NULL)
	{
		elektraRealloc ((void **) &result, elektraKeyGetValueSize (elem));
		snprintf (result, elektraKeyGetValueSize (elem), "%s", elektraKeyString (elem));
	}
	elektraArrayIncName (lookupElem);
	elem = (ElektraKey *) elektraKeyGetMeta (key, elektraKeyName (lookupElem));
	while (elem != NULL)
	{
		elektraRealloc ((void **) &result,
				elektraStrLen (result) + elektraKeyGetValueSize (elem) + 1); // String (incl. +2 times \0) + delimiter + whitespace
		strcat (result, delim);
		strcat (result, elektraKeyString (elem));
		elektraArrayIncName (lookupElem);
		elem = (ElektraKey *) elektraKeyGetMeta (key, elektraKeyName (lookupElem));
	}
	elektraKeyDel (lookupElem);
	return result;
}

/**
 * @}
 */
