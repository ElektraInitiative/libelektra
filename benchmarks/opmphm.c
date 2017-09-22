/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map C benchmark.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#define KDBRAND_BENCHMARK
#include "../src/libs/elektra/opmphm.c"
#include "../src/libs/elektra/rand.c"
#include "benchmarks.h"
#include <sys/time.h>

int32_t elektraRandBenchmarkInitSeed;

// benchmarks helpers
static int32_t * getRandomSeed (int32_t * seed);
static FILE * openOutFileWithRPartitePostfix (const char * name);
static const char * elektraGetString (void * data);
static size_t getPower (size_t p, size_t q);
// shapes helpers
static void showShape (void);
// generate KeySets
static KeySetShape * getKeySetShapes (void);
const size_t numberOfShapes = 90;


/**
 * General structure of a benchmark
 *
 * `name` is a unique name of the benchmark and `benchmarkF` is the independent function executing the benchmark.
 * Execute a benchmark with benchmark_opmphm `name`.
 */
typedef struct
{
	char * name;
	void (*benchmarkF) (void);
} Benchmark;


/**
 * START ======================================= Measures the Opmphm Hash Function time ============================================== START
 *
 * This benchmark measures the time for hashing a whole KeySet, variegating in the size. Executed multiple times.
 *
 * The output has the following header: n;n;n;n;... (for each KeySetShape)
 *
 * This benchmark takes numberOfShapes * nCount seeds
 */
static void benchmarkHashFunctionTime (void)
{
	const size_t nCount = 4;
	const size_t n[] = { 10, 100, 1000, 10000 };
	const size_t runs = 11;
	// init results
	size_t * results = elektraMalloc (nCount * numberOfShapes * runs * sizeof (size_t));
	if (!results)
	{
		printExit ("malloc");
	}
	// benchmark
	KeySetShape * keySetShapes = getKeySetShapes ();
	for (size_t i = 0; i < nCount; ++i)
	{
		for (size_t s = 0; s < numberOfShapes; ++s)
		{
			printf ("now at n: %lu/%lu shape: %lu/%lu\r", i, nCount, s, numberOfShapes);
			int32_t seed;
			if (getRandomSeed (&seed) != &seed) printExit ("Seed Parsing Error or feed me more seeds");
			KeySet * ks = generateKeySet (n[i], &seed, &keySetShapes[s]);
			for (size_t r = 0; r < runs; ++r)
			{
				Key * key;
				ksRewind (ks);
				struct timeval start;
				struct timeval end;
				__asm__("");
				gettimeofday (&start, 0);
				__asm__("");
				// measure
				while ((key = ksNext (ks)))
				{
					__asm__("");
					opmphmHashfunction (keyName (key), strlen (keyName (key)), 1337);
					__asm__("");
				}
				__asm__("");
				gettimeofday (&end, 0);
				__asm__("");
				results[i * (numberOfShapes * runs) + s * runs + r] =
					(end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
			}
			ksDel (ks);
		}
	}
	elektraFree (keySetShapes);
	// wirte out results
	FILE * out = openOutFileWithRPartitePostfix ("benchmark_opmphm_hashfunctiontime");
	if (!out)
	{
		printExit ("open out file");
	}
	// print header
	for (size_t i = 0; i < nCount; ++i)
	{
		for (size_t s = 0; s < numberOfShapes; ++s)
		{
			if (!s && !i)
			{
				fprintf (out, "%lu", n[i]);
			}
			else
			{
				fprintf (out, ";%lu", n[i]);
			}
		}
	}
	fprintf (out, "\n");
	// print data
	for (size_t r = 0; r < runs; ++r)
	{
		for (size_t i = 0; i < nCount; ++i)
		{
			for (size_t s = 0; s < numberOfShapes; ++s)
			{
				if (!s && !i)
				{
					fprintf (out, "%lu", results[i * (numberOfShapes * runs) + s * runs + r]);
				}
				else
				{
					fprintf (out, ";%lu", results[i * (numberOfShapes * runs) + s * runs + r]);
				}
			}
		}
		fprintf (out, "\n");
	}
	fclose (out);
	elektraFree (results);
}
/**
 * END ========================================= Measures the Opmphm Hash Function time ================================================ END
 */

/**
 * START ======================= Compares the Opmphm Hash Function vs Hash Function suggested by Fox et al. ========================== START
 *
 * This comparison counts how often the mapping process needs to be done. The benchmark tries different n and c (all c above the minimum c),
 * for one n and c multiple seeds are used.
 *
 * The output format is: n/c;n/c;... (each n and c are unique)
 *
 * This benchmark takes (numberOfShapes * nCount) + (different c * seeds) seeds
 */

/**
 * Benchmark the suggested hash function by
 * An O(n log n) Algorithm for Finding Minimal Perfect Hash Functions
 * Fox et al.
 */
typedef struct
{
	size_t maxLength;
	int32_t * randomData;
} FoxHash;

/**
 * @brief Creates the FoxHash
 *
 * Finds the longest Key name in the KeySet and generates with initSeed a random Table, such that there is a random number
 * for each char at every possible position in the string.
 *
 * @param initSeed initialization seed
 * @param ks the KeySet
 *
 * @retval FoxHash * success
 * @retval NULL memory error
 */
static FoxHash * createFoxHash (int32_t * initSeed, KeySet * ks)
{
	ELEKTRA_ASSERT (initSeed, "seed is NULL");
	ELEKTRA_ASSERT (ks, "KeySet is NULL");
	FoxHash * out = elektraMalloc (sizeof (FoxHash));
	if (!out)
	{
		return NULL;
	}
	// find maximum length
	out->maxLength = 0;
	Key * key;
	ksRewind (ks);
	while ((key = ksNext (ks)))
	{
		if (strlen (keyName (key)) > out->maxLength)
		{
			out->maxLength = strlen (keyName (key));
		}
	}
	size_t alphabetLength = strlen (alphabetnumbers) + strlen (alphabetspecial);
	out->randomData = malloc (sizeof (int32_t) * alphabetLength * out->maxLength);
	if (!out->randomData)
	{
		elektraFree (out);
		return NULL;
	}
	for (size_t i = 0; i < alphabetLength * out->maxLength; ++i)
	{
		elektraRand (initSeed);
		out->randomData[i] = *initSeed;
	}
	return out;
}

/**
 * @brief Destroy the FoxHash
 *
 * Frees all memory.
 *
 * @param f the fox hash
 */
static void destroyFoxHash (FoxHash * f)
{
	ELEKTRA_ASSERT (f, "FoxHash is NULL");
	elektraFree (f->randomData);
	elektraFree (f);
}

/**
 * @brief The FoxHash hash function
 *
 * Searches for every char in key the position of the respective char in the alphabets.
 * The position in the alphabets and the position in the key of a char is used to add up the hash function value.
 *
 * @param f the fox hash
 * @param key a char * to the key
 *
 * @retval int32_t hash function value.
 */
static int32_t foxHash (FoxHash * f, const void * key)
{
	int32_t h = 0;
	char * c = (char *)key;
	size_t i = 0;
	while (*c)
	{
		// find index of *c in alphabets
		size_t pos = 0;
		char * an = (char *)alphabetnumbers;
		char * as = (char *)alphabetspecial;
		while (*an && *c != *an)
		{
			++pos;
			++an;
		}
		if (!*an)
		{
			// not found in alphabetnumbers
			pos = strlen (alphabetnumbers);
			while (*as && *c != *as)
			{
				++pos;
				++as;
			}
			ELEKTRA_ASSERT (*as, "char not found");
		}
		h += f->randomData[pos * f->maxLength + i];
		++c;
		++i;
	}
	return h;
}

static size_t benchmarkHashFunctionVsFox (KeySet * ks, size_t n, double c, int32_t seed, const size_t maxMappingCalls)
{
	size_t calls = 0;
	FoxHash * fHash[OPMPHMR_PARTITE];
	Opmphm * opmphm = opmphmNew ();
	if (!opmphm) printExit ("opmphm");
	OpmphmGraph * graph = opmphmGraphNew (opmphm, n, c);
	if (!graph) printExit ("graph");
	int cyclic;
	do
	{
		// init fox hash
		for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
		{
			fHash[r] = createFoxHash (&seed, ks);
			if (!fHash[r]) printExit ("foxhash");
		}
		size_t i = 0;
		Key * key;
		ksRewind (ks);
		while ((key = ksNext (ks)))
		{
			for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
			{
				// set edge.h[]
				graph->edges[i].h[r] = foxHash (fHash[r], elektraGetString (key)) % opmphm->componentSize;
				// add edge to graph
				// set edge.nextEdge[r]
				size_t v = r * opmphm->componentSize + graph->edges[i].h[r];
				graph->edges[i].nextEdge[r] = graph->vertices[v].firstEdge;
				// set vertex.firstEdge
				graph->vertices[v].firstEdge = i;
				// increment degree
				++graph->vertices[v].degree;
			}
			++i;
		}
		for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
		{
			destroyFoxHash (fHash[r]);
		}
		cyclic = hasCycle (opmphm, graph, n);
		if (cyclic)
		{
			memset (graph->vertices, 0, opmphm->componentSize * OPMPHMR_PARTITE * sizeof (OpmphmVertex));
		}
		++calls;
	} while (cyclic && calls < maxMappingCalls);
	opmphmDel (opmphm);
	opmphmGraphDel (graph);
	return calls;
}

static size_t benchmarkHashFunctionVsOpmphm (KeySet * ks, size_t n, double c, int32_t seed, const size_t maxMappingCalls)
{
	size_t calls = 0;
	Opmphm * opmphm = opmphmNew ();
	if (!opmphm) printExit ("opmphm");
	OpmphmGraph * graph = opmphmGraphNew (opmphm, n, c);
	if (!graph) printExit ("graph");
	OpmphmInit init;
	init.getString = elektraGetString;
	init.data = (void **)ks->array;
	init.initSeed = seed;
	int ret;
	do
	{
		ret = opmphmMapping (opmphm, graph, &init, n);
		++calls;
	} while (ret && calls < maxMappingCalls);
	opmphmDel (opmphm);
	opmphmGraphDel (graph);
	return calls;
}

static void benchmarkHashFunctionVs (void)
{
	const size_t nCount = 1;
	const size_t n[] = { 10, 100, 1000, 10000 };
	const size_t seeds = 11;
	const size_t cCount = 6;
	const double c[] = { 0.0, 0.1, 0.2, 0.3, 0.4, 0.5 };
	const size_t maxMappingCalls = 5;
	// init results
	size_t * opmphmResults = elektraMalloc (nCount * numberOfShapes * cCount * seeds * sizeof (size_t));
	if (!opmphmResults)
	{
		printExit ("malloc");
	}
	size_t * foxResults = elektraMalloc (nCount * numberOfShapes * cCount * seeds * sizeof (size_t));
	if (!foxResults)
	{
		printExit ("malloc");
	}
	// benchmarks
	KeySetShape * keySetShapes = getKeySetShapes ();
	for (size_t i = 0; i < nCount; ++i)
	{
		for (size_t s = 0; s < numberOfShapes; ++s)
		{
			printf ("now at n: %lu/%lu shape: %lu/%lu\r", i, nCount, s, numberOfShapes);
			int32_t genSeed;
			if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
			KeySet * ks = generateKeySet (n[i], &genSeed, &keySetShapes[s]);
			for (size_t cI = 0; cI < cCount; ++cI)
			{
				for (size_t sI = 0; sI < seeds; ++sI)
				{
					// get seed for run
					int32_t runSeed;
					if (getRandomSeed (&runSeed) != &runSeed) printExit ("Seed Parsing Error or feed me more seeds");
					// opmphm
					opmphmResults[i * (numberOfShapes * cCount * seeds) + s * (cCount * seeds) + cI * seeds + sI] =
						benchmarkHashFunctionVsOpmphm (ks, n[i], opmphmMinC () + c[cI], runSeed, maxMappingCalls);
					// fox
					foxResults[i * (numberOfShapes * cCount * seeds) + s * (cCount * seeds) + cI * seeds + sI] =
						benchmarkHashFunctionVsFox (ks, n[i], opmphmMinC () + c[cI], runSeed, maxMappingCalls);
				}
			}
			ksDel (ks);
		}
	}
	elektraFree (keySetShapes);
	// write out
	FILE * opmphmOut = openOutFileWithRPartitePostfix ("benchmark_opmphm_hashfunction_opmphm");
	if (!opmphmOut)
	{
		printExit ("open out file");
	}
	FILE * foxOut = openOutFileWithRPartitePostfix ("benchmark_opmphm_hashfunction_fox");
	if (!foxOut)
	{
		printExit ("open out file");
	}
	// print header
	for (size_t i = 0; i < nCount; ++i)
	{
		for (size_t cI = 0; cI < cCount; ++cI)
		{
			if (!cI && !i)
			{
				fprintf (opmphmOut, "%lu/%f", n[i], opmphmMinC () + c[cI]);
				fprintf (foxOut, "%lu/%f", n[i], opmphmMinC () + c[cI]);
			}
			else
			{
				fprintf (opmphmOut, ";%lu/%f", n[i], opmphmMinC () + c[cI]);
				fprintf (foxOut, ";%lu/%f", n[i], opmphmMinC () + c[cI]);
			}
		}
	}
	fprintf (opmphmOut, "\n");
	fprintf (foxOut, "\n");
	// print data
	for (size_t s = 0; s < numberOfShapes; ++s)
	{
		for (size_t sI = 0; sI < seeds; ++sI)
		{
			for (size_t i = 0; i < nCount; ++i)
			{
				for (size_t cI = 0; cI < cCount; ++cI)
				{
					if (!cI && !i)
					{
						fprintf (opmphmOut, "%lu", opmphmResults[i * (numberOfShapes * cCount * seeds) +
											 s * (cCount * seeds) + cI * seeds + sI]);
						fprintf (foxOut, "%lu", foxResults[i * (numberOfShapes * cCount * seeds) +
										   s * (cCount * seeds) + cI * seeds + sI]);
					}
					else
					{
						fprintf (opmphmOut, ";%lu", opmphmResults[i * (numberOfShapes * cCount * seeds) +
											  s * (cCount * seeds) + cI * seeds + sI]);
						fprintf (foxOut, ";%lu", foxResults[i * (numberOfShapes * cCount * seeds) +
										    s * (cCount * seeds) + cI * seeds + sI]);
					}
				}
			}
			fprintf (opmphmOut, "\n");
			fprintf (foxOut, "\n");
		}
	}
	fclose (opmphmOut);
	fclose (foxOut);
	elektraFree (opmphmResults);
	elektraFree (foxResults);
}
/**
 * END ========================= Compares the Opmphm Hash Function vs Hash Function suggested by Fox at al. ============================ END
 */

/**
 * START ====================================== Tests all possible seeds for one KeySet ============================================== START
 * 
 * not finished.
 */

void benchmarkHashFunctionAllSeeds (void)
{
	const size_t n = 2000;
	const double c = opmphmMinC () + 0.1;
	const size_t shapeId = 40;
	const size_t maxMappingCalls = 5;
	const int32_t startSeed = 25698431;
	const int32_t maxSeed = 25698431 + 10000;
	if (shapeId >= numberOfShapes)
	{
		printExit ("shapeId higher than numberOfShapes");
	}
	size_t * result = elektraMalloc (maxMappingCalls * sizeof (size_t));
	if (!result)
	{
		printExit ("malloc");
	}
	for (size_t i = 0; i < maxMappingCalls; ++i)
	{
		result[i] = 0;
	}
	KeySetShape * keySetShapes = getKeySetShapes ();
	int32_t genSeed;
	if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
	KeySet * ks = generateKeySet (n, &genSeed, &keySetShapes[shapeId]);
	for (int32_t seed = startSeed; seed != maxSeed; ++seed)
	{
		++result[benchmarkHashFunctionVsOpmphm (ks, n, c, seed, maxMappingCalls - 1)];
		if (seed & 10000)
		{
			printf ("now at seed: %i/%i\r", seed, maxSeed - 2);
		}
	}
	ksDel (ks);
	elektraFree (keySetShapes);
	for (size_t i = 0; i < maxMappingCalls; ++i)
	{
		printf ("%lu: %lu of %i\n", i, result[i], maxSeed - 2 - startSeed);
	}
	elektraFree (result);
}

/**
 * END ========================= Compares the Opmphm Hash Function vs Hash Function suggested by Fox at al. ============================ END
 */

int main (int argc, char ** argv)
{
	// define all benchmarks
	const size_t benchmarksCount = 3;
	Benchmark benchmarks[benchmarksCount];
	// hashfunctiontime
	char * benchmarkNameHashFunctionTime = "hashfunctiontime";
	benchmarks[0].name = benchmarkNameHashFunctionTime;
	benchmarks[0].benchmarkF = benchmarkHashFunctionTime;
	// hashfunctionvs
	char * benchmarkNameHashFunctionVs = "hashfunctionvs";
	benchmarks[1].name = benchmarkNameHashFunctionVs;
	benchmarks[1].benchmarkF = benchmarkHashFunctionVs;
	// hashfunctionvs
	char * benchmarkNameHashFunctionAllSeeds = "hashfunctionallseeds";
	benchmarks[1].name = benchmarkNameHashFunctionAllSeeds;
	benchmarks[1].benchmarkF = benchmarkHashFunctionAllSeeds;

	// run benchmark
	if (argc == 1)
	{
		fprintf (stderr, "Usage: %s <benchmark>\n", argv[0]);
		fprintf (stderr, "Available benchmarks:\n");
		for (size_t i = 0; i < benchmarksCount; ++i)
		{
			fprintf (stderr, "* %s\n", benchmarks[i].name);
		}
		return EXIT_FAILURE;
	}
	for (size_t i = 0; i < benchmarksCount; ++i)
	{
		if (!strncmp (benchmarks[i].name, argv[1], strlen (benchmarks[i].name)))
		{
			benchmarks[i].benchmarkF ();
			return EXIT_SUCCESS;
		}
	}
	fprintf (stderr, "Error: %s is not a benchmark\n", argv[1]);
	fprintf (stderr, "Available benchmarks:\n");
	for (size_t i = 0; i < benchmarksCount; ++i)
	{
		fprintf (stderr, "* %s\n", benchmarks[i].name);
	}
	return EXIT_FAILURE;
}

/**
 * Benchmark helpers
 */

/**
 * @brief Read a seed from STDIN.
 *
 * @param seed storage for the read in seed
 *
 * @retval int32_t * on success
 * @retval NULL on read or parse error
 */
static int32_t * getRandomSeed (int32_t * seed)
{
	// read from stdin
	char data[10 + 2]; // min = 0, max = 2^32 - 1, len(2^32 - 1) = 10 + '\n' + '\0'
	if (fgets (data, 12, stdin) != data)
	{
		return NULL;
	}
	// eliminate newline
	char * c;
	for (c = data; *c != '\n'; ++c)
		;
	*c = '\0';
	// convert to int
	char * pEnd;
	*seed = strtol (data, &pEnd, 10);
	if (*pEnd != '\0')
	{
		return NULL;
	}
	return seed;
}

/**
 * @brief Opens file with OPMPHMR_PARTITE postfix.
 *
 * supports OPMPHMTUPLE < 100
 *
 * @param name name of the file
 *
 * @retval FILE * on success
 * @retval NULL on error
 */
static FILE * openOutFileWithRPartitePostfix (const char * name)
{
	const char * const format = "%u.csv";
	char formatData[strlen (name) + strlen (format) + 1];
	char filename[strlen (name) + strlen (format) + 1];
	strcpy (formatData, name);
	strcpy (&formatData[strlen (name)], format);
	sprintf (filename, formatData, OPMPHMR_PARTITE);
	FILE * out = fopen (filename, "w");
	if (!out)
	{
		return NULL;
	}
	return out;
}

static const char * elektraGetString (void * data)
{
	return keyName ((Key *)data);
}

/**
 * @brief Power function.
 *
 * @param p basis
 * @param q exponent
 *
 * @retval size_t p^q
 */
static size_t getPower (size_t p, size_t q)
{
	size_t result = 1;
	for (size_t t = 0; t < q; ++t)
	{
		result *= p;
	}
	return result;
}

/**
 * The Key Set shapes
 */
// ** regular shaped **
// binary: only 0 or 1 as return
/**
 * every key name is unique and goes 5 level deep
 */
static void shapefConstBinary (const size_t initSize ELEKTRA_UNUSED, size_t size ELEKTRA_UNUSED, size_t level,
			       int32_t * seed ELEKTRA_UNUSED, KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	size_t maxLevel = 5;
	if (level > maxLevel)
	{
		ret->subKeys = 0;
	}
	else
	{
		ret->subKeys = 1;
	}
	ret->label = 0;
}
/**
 * binary tree
 */
// non binary: 1 > as return
static void shapefBinaryBranch (const size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED,
				KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	size_t subKeys = 2;
	size_t vertices = 0;
	for (size_t i = 1; i < level; ++i)
	{
		vertices += getPower (subKeys, i);
	}
	if (subKeys > size || vertices > initSize)
	{
		ret->subKeys = 0;
	}
	else
	{
		ret->subKeys = subKeys;
	}
	ret->label = 0;
}
/**
 * every parent has n/branchfactor children
 */
static void shapefDynamicBranch (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
				 KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	size_t branchFactor = 9;
	size_t subKeys = (initSize / branchFactor);
	if (subKeys < 2)
	{
		subKeys = 2;
	}
	size_t vertices = 0;
	for (size_t i = 1; i < level; ++i)
	{
		vertices += getPower (subKeys, i);
	}
	if (vertices > initSize)
	{
		ret->subKeys = 0;
	}
	else
	{
		ret->subKeys = subKeys;
	}
	ret->label = 0;
}
// ** abnormal shaped **
/**
 * all key names have a common start, startLevel length
 */
static void shapefLateDynamicBranch (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
				     KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	ret->label = 0;
	size_t startLevel = 5;
	if (level <= startLevel)
	{
		ret->subKeys = 1;
		return;
	}
	level -= startLevel;
	size_t branchFactor = 9;
	size_t subKeys = (initSize / branchFactor);
	if (subKeys < 2)
	{
		subKeys = 2;
	}
	size_t vertices = 0;
	for (size_t i = 1; i < level; ++i)
	{
		vertices += getPower (subKeys, i);
	}
	if (vertices > initSize)
	{
		ret->subKeys = 0;
	}
	else
	{
		ret->subKeys = subKeys;
	}
}
/**
* all key names have a common start and end
*/
static void * shapefCommonStartEndInit (void)
{
	uint8_t * data = elektraMalloc (sizeof (uint8_t));
	if (!data)
	{
		return NULL;
	}
	*data = 0;
	return data;
}

static void shapefCommonStartEndDel (void * data)
{
	elektraFree (data);
}


static void shapefCommonStartEnd (const size_t initSize ELEKTRA_UNUSED, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED,
				  KsShapeFunctionReturn * ret, void * data)
{
	size_t notCommonLevel = 4;
	size_t maxLevel = 10;
	if (level < notCommonLevel)
	{
		// creates common start
		ret->subKeys = 1;
		ret->label = 0;
	}
	else if (notCommonLevel == level)
	{
		// creates level with different names
		ret->subKeys = size + 1;
		ret->label = 0;
	}
	else if (level > notCommonLevel)
	{
		uint8_t * isLabelSet = data;
		if (!*isLabelSet)
		{
			// creates common end
			if (level == notCommonLevel + 1)
			{
				// set label
				ret->label = 1;
				ret->subKeys = 1;
			}
			else if (level == maxLevel)
			{
				// end of deep key
				ret->label = 0;
				ret->subKeys = 0;
				*isLabelSet = 1;
			}
			else
			{
				// create deep key
				ret->label = 0;
				ret->subKeys = 1;
			}
		}
		else
		{
			// use common end
			ret->subKeys = -1;
			ret->label = 1;
		}
	}
}


/**
 * @brief Generates all possible KeySetShapes.
 *
 * Uses a predefined set of parameters and KsShapeFunctions to generate all possible KeySetShape.
 *
 * @retval KeySetShape * on success
 */
static KeySetShape * getKeySetShapes (void)
{
	KeySetShape * out = elektraMalloc (sizeof (KeySetShape) * numberOfShapes);
	if (!out) printExit ("malloc KeySetShapes");
	const unsigned int wordLength[3] = { 1, 25, 50 };
	const unsigned int special[2] = { 5, 10 };
	const unsigned int parent[5] = { 0, 1, 5, 15, 30 };
	const KsShapeFunction shapeFunctions[4] = { shapefConstBinary, shapefBinaryBranch, shapefDynamicBranch, shapefLateDynamicBranch };
	// numberOfShapes = 3 * 2 * 5 * shapefCount
	size_t shapeCount = 0;
	for (int w0 = 0; w0 < 3; ++w0)
	{
		for (int w1 = w0 + 1; w1 < 3; ++w1)
		{
			for (int s = 0; s < 2; ++s)
			{
				for (int p = 0; p < 5; ++p)
				{
					for (int sf = 0; sf < 4; ++sf)
					{
						out[shapeCount].minWordLength = wordLength[w0];
						out[shapeCount].maxWordLength = wordLength[w1];
						out[shapeCount].special = special[s];
						out[shapeCount].parent = parent[p];
						out[shapeCount].shapef = shapeFunctions[sf];
						++shapeCount;
					}
				}
			}
		}
	}
	if (shapeCount != numberOfShapes) printExit ("shapeCount != numberOfShapes");
	return out;
}

static void showShape (void)
{
	size_t n = 20;
	// SHAPE
	KeySetShape shape;
	shape.minWordLength = 20;
	shape.maxWordLength = 30;
	shape.special = 0;
	shape.parent = 0;
	shape.shapeInit = shapefCommonStartEndInit;
	shape.shapef = shapefCommonStartEnd;
	shape.shapeDel = shapefCommonStartEndDel;
	// SHAPE
	// GENERATE
	KeySet * ks = generateKeySet (n, NULL, &shape);
	// GENERATE
	if (1)
	{
		// print KS
		printf ("print\n");
		Key * key;
		ksRewind (ks);
		while ((key = ksNext (ks)))
		{
			printf ("%s\n", keyName (key));
		}
		// print KS
	}
	printf ("%li\n", ksGetSize (ks));
	ksDel (ks);
}
