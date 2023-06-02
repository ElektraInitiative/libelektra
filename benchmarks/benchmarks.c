/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#define _GNU_SOURCE
#include "./benchmarks.h"
#ifdef HAVE_HSEARCHR
#include <search.h>
#endif
#include <sys/time.h>

#include <internal/utility/alloc.h>

struct timeval start;
int num_dir = NUM_DIR;
int num_key = NUM_KEY;
KeySet * large;

void timeInit (void)
{
	gettimeofday (&start, 0);
}

void timePrint (char * msg)
{
	struct timeval measure;
	time_t diff;

	gettimeofday (&measure, 0);

	diff = (measure.tv_sec - start.tv_sec) * 1000000 + (measure.tv_usec - start.tv_usec);
	fprintf (stdout, "%20s: %20d Microseconds\n", msg, (int) diff);

	gettimeofday (&start, 0);
}

int timeGetDiffMicroseconds (void)
{
	struct timeval measure;
	time_t diff;

	gettimeofday (&measure, 0);
	diff = (measure.tv_sec - start.tv_sec) * 1000000 + (measure.tv_usec - start.tv_usec);
	gettimeofday (&start, 0);

	return (int) diff;
}

void benchmarkCreate (void)
{
	large = ksNew (num_key * num_dir, KS_END);
}

void benchmarkFillup (void)
{
	int i, j;
	char name[KEY_NAME_LENGTH + 1];
	char value[] = "data";

	for (i = 0; i < num_dir; i++)
	{
		snprintf (name, KEY_NAME_LENGTH, "%s/%s%d", KEY_ROOT, "dir", i);
		ksAppendKey (large, keyNew (name, KEY_VALUE, value, KEY_END));
		for (j = 0; j < num_key; j++)
		{
			snprintf (name, KEY_NAME_LENGTH, "%s/%s%d/%s%d", KEY_ROOT, "dir", i, "key", j);
			ksAppendKey (large, keyNew (name, KEY_VALUE, value, KEY_END));
		}
	}
}


/**
 * Arbitrary Key Set Generator
 */


/**
 * Internal representation of the KsTree
 */
typedef struct _KsTreeVertex KsTreeVertex;
struct _KsTreeVertex
{
	char * name;			  /*!< name of the vertex, root has no name */
	uint8_t isKey;			  /*!< when true the path from root to vertex is a Key in the resulting KeySet */
	uint8_t isLink;			  /*!< determines if vertex is link, used at recFreeKsTree (...) */
	struct _KsTreeVertex ** children; /*!< stores the children */
#ifdef HAVE_HSEARCHR
	struct hsearch_data * htab; /*!< stores the Hash Map, containing the children names */
#endif
	size_t numberofChildren; /*!< number of the stored children */
	size_t mallocSize;	 /*!< size malloced for the children */
};

/**
 * Declares the label storage and some config
 */
const uint8_t maxNameGenerationTries = 99;
#define NUMBER_OF_LABELS 10
static KsTreeVertex * labels[NUMBER_OF_LABELS];

static void * shapeDefaultInit (void);
static void shapeDefaultDel (void * data);
static void shapefDefault (const size_t initSize, size_t size, size_t level, int32_t * seed, KsShapeFunctionReturn * ret, void * data);

const char * const alphabetnumbers = "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ0123456789";
const char * const alphabetspecial = "^!\"$`%&/{([)]=} %?\\+*~#';,:§.-_|<>¸¬½¼³²¹ł€¶øæßðđł˝«»¢“”nµ─·";


#ifdef HAVE_HSEARCHR
/**
 * @brief Creates the Hash Map for a given vertex.
 *
 * vertex->mallocSize must be set.
 *
 * @param vertex the vertex
 */
static void createHashMap (KsTreeVertex * vertex)
{
	vertex->htab = elektraCalloc (sizeof (struct hsearch_data));
	if (!vertex->htab || !hcreate_r (vertex->mallocSize, vertex->htab))
	{
		printExit ("recGenerateKsTree: can not create Hash Map");
	}
}

/**
 * @brief Deletes the Hash Map for a given vertex.
 *
 * @param vertex the vertex
 */
static void deleteHashMap (KsTreeVertex * vertex)
{
	hdestroy_r (vertex->htab);
	elektraFree (vertex->htab);
}
/**
 * @brief Searches in the Hash Map of vertex for the name.
 *
 * If not in vertex insert it.
 *
 * @param vertex the vertex
 * @param name the name to search
 *
 * @retval 1 if not in vertex, unique
 * @retval 0 if in vertex, not unique
 */
static int searchHashMap (KsTreeVertex * vertex, char * name)
{
	ENTRY e;
	ENTRY * ep;
	e.key = name;
	e.data = NULL;
	if (!hsearch_r (e, FIND, &ep, vertex->htab))
	{
		// not in Hash Map, insert
		if (!hsearch_r (e, ENTER, &ep, vertex->htab))
		{
			printExit ("recGenerateKsTree: can not insert in Hash Map");
		}
		return 1;
	}
	else
	{
		return 0;
	}
}
#else
// no hsearch use dummys and linear search
static void createHashMap (KsTreeVertex * vertex ELEKTRA_UNUSED)
{
}
static void deleteHashMap (KsTreeVertex * vertex ELEKTRA_UNUSED)
{
}
static int searchHashMap (KsTreeVertex * vertex, char * name)
{
	for (size_t i = 0; i < vertex->numberofChildren; ++i)
	{
		if (!strcmp (vertex->children[i]->name, name))
		{
			return 0;
		}
	}
	return 1;
}
#endif

/**
 * @brief Fills a string up with random chars and null terminates it.
 *
 * @param name the string
 * @param length the length of the string (excluding the '\0')
 * @param seed to generate random data
 * @param shape the shape of the KeySet
 */
static void fillUpWithRandomChars (char * name, size_t length, int32_t * seed, KeySetShape * shape)
{
	// throw the dice and use 8 bit of each byte from seed for one char, determination if special or not and which
	elektraRand (seed);
	size_t i = 0;
	size_t s = i;
	char * c = name;
	bool done = false;
	while (!done)
	{
		uint8_t ds = (*seed >> (8 * s * 2)) & 0xFF;
		uint8_t dc = (*seed >> (8 * (s * 2 + 1))) & 0xFF;
		// special or not
		bool special = shape->special && (ds % shape->special) == 0;
		// choose
		if (special)
		{
			*c = alphabetspecial[dc % strlen (alphabetspecial)];
		}
		else
		{
			*c = alphabetnumbers[dc % strlen (alphabetnumbers)];
		}
		++c;
		++i;
		++s;
		if (i == length)
		{
			done = true;
		}
		else if (s == 2)
		{
			elektraRand (seed);
			s = 0;
		}
	}
	*c = '\0';
}

/**
 * @brief Recursively calculate the number of vertices with isKey set to 1 starting from vertex.
 *
 * @param vertex the staring
 */
static size_t getSizeOfVertex (KsTreeVertex * vertex)
{
	size_t size = 0;
	if (vertex->isKey)
	{
		++size;
	}
	for (size_t i = 0; i < vertex->numberofChildren; ++i)
	{
		size += getSizeOfVertex (vertex->children[i]);
	}
	return size;
}

/**
 * @brief Generate a name for a vertex.
 *
 * Generates a name and looks in the parent if the name is unique, if not try until maxNameGenerationTries * (maxLength - length + 1)
 * is exceeded. The length is extended every maxNameGenerationTries by one until maxWordLength is reached.
 *
 * @param parent the parent vertex
 * @param seed the seed for random actions, already randomized
 * @param shape the KeySetShape
 *
 * @retval char * the generated name
 */
static char * generateName (KsTreeVertex * parent, int32_t * seed, KeySetShape * shape)
{
	char * out = elektraMalloc ((shape->maxWordLength + 1) * sizeof (char));
	if (!out)
	{
		printExit ("recGenerateKsTree: malloc vertex->name");
	}
	int32_t dl = *seed & 0xFFFFFF;
	size_t length = (dl % (shape->maxWordLength - shape->minWordLength + 1)) + shape->minWordLength;
	uint8_t uniqueName;
	uint8_t nameGenerationTries = 0;
	do
	{
		// generate name and see if name is unique
		fillUpWithRandomChars (out, length, seed, shape);
		uniqueName = searchHashMap (parent, out);
		++nameGenerationTries;
		if (nameGenerationTries > maxNameGenerationTries && !uniqueName)
		{
			// make word longer if possible
			if (length < shape->maxWordLength)
			{

				++length;
				// start new
				nameGenerationTries = 0;
			}
			else
			{
				printExit ("recGenerateKsTree: max name generation tries exceeded");
			}
		}
	} while (!uniqueName);
	return out;
}

/**
 * @brief Recursively generates a KsTree.
 *
 * Every invocation generates a vertex. First the name and after the KeySetShape->shapef invocation the children by recursion.
 *
 * @param parent the parent vertex
 * @param size the target number of vertices where isKey is 1
 * @param actualSize the number of vertices where isKey is 1 so far
 * @param level the actual level
 * @param seed the seed for random actions
 * @param shape the KeySetShape
 * @param data the data passed to the KeySetShape->shapef function
 *
 * @retval KsTreeVertex * the generated vertex
 */
static KsTreeVertex * recGenerateKsTree (KsTreeVertex * parent, const size_t size, size_t * actualSize, size_t level, int32_t * seed,
					 KeySetShape * shape, void * data)
{
	// create actual vertex
	KsTreeVertex * vertex = elektraMalloc (sizeof (KsTreeVertex));
	if (!vertex)
	{
		printExit ("recGenerateKsTree: malloc vertex");
	}
	// default not a key
	vertex->isKey = 0;
	elektraRand (seed);
	// used for parent determination
	int8_t dp = *seed >> 24;
	// vertex name generation
	vertex->name = generateName (parent, seed, shape);
	// determine subKeys and label
	KsShapeFunctionReturn ret;
	shape->shapef (size, *actualSize, level, seed, &ret, data);
	// if too many set max
	if (ret.subKeys > (ssize_t) *actualSize + 1)
	{
		ret.subKeys = *actualSize;
	}
	if (ret.subKeys >= 0)
	{
		vertex->isLink = 0;
		// vertex children generation
		if (ret.subKeys > 0)
		{
			// create branch
			// remove costs for subkeys
			*actualSize -= (ret.subKeys - 1); // the cost for one is included in the size from the parent call
			// see if parent
			if (*actualSize && shape->parent && (dp % shape->parent) == 0)
			{
				// counts extra so costs need to be removed
				--*actualSize;
				vertex->isKey = 1;
			}
			// prepare children
			vertex->numberofChildren = 0;
			vertex->mallocSize = ret.subKeys;
			createHashMap (vertex);
			vertex->children = elektraMalloc (vertex->mallocSize * sizeof (KsTreeVertex *));
			if (!vertex->children)
			{
				printExit ("recGenerateKsTree: malloc children");
			}
			++level;
			// make children
			for (size_t i = 0; i < vertex->mallocSize; ++i)
			{
				vertex->children[i] = recGenerateKsTree (vertex, size, actualSize, level, seed, shape, data);
				++vertex->numberofChildren;
			}
		}
		else
		{
			// terminate branch
			vertex->isKey = 1;
			vertex->numberofChildren = 0;
		}
		if (ret.label)
		{
			// set label at vertex
			if (ret.label > NUMBER_OF_LABELS)
			{
				printExit ("recGenerateKsTree: label > NUMBER_OF_LABELS");
			}
			--ret.label;
			labels[ret.label] = vertex;
		}
	}
	else
	{
		// links will not be followed by recFreeKsTree (...)
		vertex->isLink = 1;
		if (!ret.label || ret.label > NUMBER_OF_LABELS)
		{
			printExit ("recGenerateKsTree: subKeys < 0 but no label set or label > NUMBER_OF_LABELS");
		}
		// take children from label vertex
		--ret.label;
		KsTreeVertex * linkVertex = labels[ret.label];
		// copy children, if space actualSize includes the costs for the actual key
		size_t linkTreeSize = getSizeOfVertex (linkVertex) - 1;
		// linkVertex->isKey will not be copied, so remove it from size if set
		if (linkVertex->isKey)
		{
			--linkTreeSize;
		}
		if (*actualSize >= linkTreeSize)
		{
			vertex->mallocSize = linkVertex->mallocSize;
			vertex->numberofChildren = linkVertex->numberofChildren;
			vertex->children = linkVertex->children;
			*actualSize -= linkTreeSize;
			// link has children so it can have a parent
			if (*actualSize && shape->parent && (dp % shape->parent) == 0)
			{
				// counts extra so costs need to be removed
				--*actualSize;
				vertex->isKey = 1;
			}
		}
		else
		{
			// if no space terminate branch
			vertex->numberofChildren = 0;
			vertex->isKey = 1;
		}
	}
	return vertex;
}

/**
 * @brief Transforms a given branch of the KsTree to a KeySet.
 *
 * The KeySet and Key must be initialized. Every vertex with isKey set will be a Key in the resulting KeySet.
 * The resulting Key name is every KsTreeVertex->name in the path from root to vertex.
 *
 * @param ks the KeySet
 * @param key the actual Key
 * @param vertex starting point
 *
 * @retval KeySet * the resulting KeySet
 */
static void recGenerateKeySet (KeySet * ks, Key * key, KsTreeVertex * vertex)
{
	// add name to key
	if (keyAddBaseName (key, vertex->name) < 0)
	{
		printExit ("recGenerateKeySet: Can not add KeyBaseName ");
	}
	// add if Key
	if (vertex->isKey)
	{
		Key * dupKey = keyDup (key, KEY_CP_ALL);
		if (!dupKey)
		{
			printExit ("recGenerateKeySet: Can not dup Key");
		}
		ssize_t sizeBefore = ksGetSize (ks);
		if (ksAppendKey (ks, dupKey) < 0)
		{
			printExit ("recGenerateKeySet: Can not add Key");
		}
		if (sizeBefore == ksGetSize (ks))
		{
			printExit ("recGenerateKeySet: Add Key with on effect");
		}
	}
	// go to children
	for (size_t i = 0; i < vertex->numberofChildren; ++i)
	{
		Key * dupKey = keyDup (key, KEY_CP_ALL);
		if (!dupKey)
		{
			printExit ("recGenerateKeySet: Can not dup Key");
		}
		recGenerateKeySet (ks, dupKey, vertex->children[i]);
	}
	keyDel (key);
}

/**
 * @brief Frees recursively to whole KsTree under the passed vertex.
 *
 * @param vertex the start vertex
 */
static void recFreeKsTree (KsTreeVertex * vertex)
{
	if (!vertex->isLink)
	{
		for (size_t i = 0; i < vertex->numberofChildren; ++i)
		{
			recFreeKsTree (vertex->children[i]);
		}
	}
	if (vertex->name) elektraFree (vertex->name);
	if (vertex->numberofChildren && !vertex->isLink)
	{
		elektraFree (vertex->children);
		deleteHashMap (vertex);
	}
	elektraFree (vertex);
}

/**
 * @brief Generates a KeySet.
 *
 * Generates a KsTree and transforms the KsTree to the resulting KeySet.
 *
 * @param size the desired KeySet size
 * @param seed the seed for the random generation
 * @param shape the KeySetShape
 *
 * @retval KeySet * the resulting KeySet
 */
KeySet * generateKeySet (const size_t size, int32_t * seed, KeySetShape * shape)
{
	ELEKTRA_ASSERT (size > 4, "size < 5");
	int32_t defaultSeed = 1;
	if (!seed)
	{
		seed = &defaultSeed;
	}
	KeySetShape shapeDefault;
	if (!shape)
	{
		/**
		 * Default KeySetShape
		 */
		shapeDefault.parent = 3;
		shapeDefault.special = 50;
		shapeDefault.minWordLength = 4;
		shapeDefault.maxWordLength = 7;
		shapeDefault.shapeInit = shapeDefaultInit;
		shapeDefault.shapef = shapefDefault;
		shapeDefault.shapeDel = shapeDefaultDel;

		shape = &shapeDefault;
	}
	ELEKTRA_ASSERT (shape->minWordLength <= shape->maxWordLength, "minWordLength > maxWordLength");
	ELEKTRA_ASSERT (shape->maxWordLength - shape->minWordLength <= 16777215, "max world length variation exceeded 16777215");
	ELEKTRA_ASSERT (shape->parent <= 127, "parent > 127");
	ELEKTRA_ASSERT (shape->special <= 127, "parent > 127");
	ELEKTRA_ASSERT (shape->minWordLength != 0, "minWordLength is 0");
	ELEKTRA_ASSERT (shape->maxWordLength != 0, "maxWordLength is 0");
	ELEKTRA_ASSERT (shape->shapef, "shape->shapef");
	ELEKTRA_ASSERT ((shape->shapeInit && shape->shapeDel) || (!shape->shapeInit && !shape->shapeDel),
			"shape->shapeInit or shape->shapeDel not set");
	// init data
	void * data = NULL;
	if (shape->shapeInit)
	{
		data = shape->shapeInit ();
		if (!data)
		{
			printExit ("generateKeySet: shapeInit returned NULL");
		}
	}
	// create root and init root
	KsTreeVertex * root = elektraMalloc (sizeof (KsTreeVertex));
	if (!root)
	{
		printExit ("generateKeySet: malloc root vertex");
	}
	root->name = NULL;
	root->isKey = 0;
	root->isLink = 0;
	root->mallocSize = size;
	createHashMap (root);
	root->numberofChildren = 0;
	root->children = elektraMalloc (root->mallocSize * sizeof (KsTreeVertex *));
	if (!root->children)
	{
		printExit ("generateKeySet: root children malloc");
	}
	// generate ksTree
	size_t actualSize = size;
	while (actualSize)
	{
		--actualSize;
		root->children[root->numberofChildren] = recGenerateKsTree (root, size, &actualSize, 1, seed, shape, data);
		++root->numberofChildren;
	}
	// del data
	if (shape->shapeDel)
	{
		shape->shapeDel (data);
	}
	// generate KeySet out of KsTree
	KeySet * ks = ksNew (size, KS_END);
	if (!ks)
	{
		printExit ("generateKeySet: Can not create KeySet");
	}
	for (size_t i = 0; i < root->numberofChildren; ++i)
	{
		Key * key = keyNew ("/", KEY_END);
		if (!key)
		{
			printExit ("generateKeySet: Can not create Key");
		}
		recGenerateKeySet (ks, key, root->children[i]);
	}
	// delete KsTree
	recFreeKsTree (root);
	return ks;
}

/**
 * Default KeySetShape
 *
 * Create two labels at second level and use them constantly.
 * The KeySet with default seed and 20 elements looks like:
 *
 * /6iyrg67
 * /6iyrg67/CFQK5/t24RHQJ/mvh*Hr
 * /6iyrg67/CFQK5/wxaP1Ar
 * /6iyrg67/CFQK5/wxaP1Ar/7'FE
 * /6iyrg67/jdEm/EYFex
 * /6iyrg67/jdEm/EYFex/nGH5z
 * /6iyrg67/jdEm/c5oY8cj/nd3C5L
 * /KZUr/TWNK/EYFex
 * /KZUr/TWNK/EYFex/nGH5z
 * /KZUr/TWNK/c5oY8cj/nd3C5L
 * /KZUr/csj9t/t24RHQJ/mvh*Hr
 * /KZUr/csj9t/wxaP1Ar
 * /KZUr/csj9t/wxaP1Ar/7'FE
 * /u002/sACrr
 * /u002/sACrr/EYFex
 * /u002/sACrr/EYFex/nGH5z
 * /u002/sACrr/c5oY8cj/nd3C5L
 * /u002/y10bqcj/t24RHQJ/mvh*Hr
 * /u002/y10bqcj/wxaP1Ar
 * /u002/y10bqcj/wxaP1Ar/7'FE
 */
static void * shapeDefaultInit (void)
{
	void * data = elektraMalloc (3 * sizeof (uint8_t));
	if (!data)
	{
		return NULL;
	}
	uint8_t * b = data;
	// three boolean flags
	b[0] = 0; // if first label was set
	b[1] = 0; // if second label was set
	b[2] = 0; // alternation bit, to use the labels
	return data;
}
static void shapeDefaultDel (void * data)
{
	elektraFree (data);
}
static void shapefDefault (const size_t initSize ELEKTRA_UNUSED, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
			   KsShapeFunctionReturn * ret, void * data)
{
	uint8_t * labelSet = data;
	if (level == 1)
	{
		// create 2 keys
		ret->subKeys = 2;
		ret->label = 0;
	}
	else if (level == 2)
	{
		if (!labelSet[0] && !labelSet[1])
		{
			// no label set, so set the first one
			ret->subKeys = 2;
			ret->label = 1;
			labelSet[0] = 1;
		}
		else if (labelSet[0] && !labelSet[1])
		{
			// first one set, so set the second
			ret->subKeys = 2;
			ret->label = 2;
			labelSet[1] = 1;
		}
		else
		{
			// both set, alternation to assign
			ret->subKeys = -1;
			if (labelSet[2])
			{
				ret->label = 1;
				labelSet[2] = 0;
			}
			else
			{
				ret->label = 2;
				labelSet[2] = 1;
			}
		}
	}
	else if (level == 3)
	{
		// some names after labels
		ret->subKeys = 1;
		ret->label = 0;
	}
	else
	{
		// terminate branch
		ret->subKeys = 0;
		ret->label = 0;
	}
}

/**
 * @brief Print message to stderr and exit with failure code.
 *
 * @param msg the message
 */
void printExit (const char * msg)
{
	fprintf (stderr, "FATAL: %s\n", msg);
	exit (EXIT_FAILURE);
}
