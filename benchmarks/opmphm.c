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

int32_t elektraRandBenchmarkInitSeed;

// benchmarks helpers
static int32_t * getRandomSeed (int32_t * seed);
static FILE * openOutFileWithRPartitepostfix (const char * name);
static const char * elektraGetString (void * data);
static size_t getPower (size_t p, size_t q);
size_t getNCount (void);
size_t getCCount (void);
void printStderrExit (const char * msg);
// shapes helpers
void showShape (void);
// generate KeySets
KeySetShape * getKeySetShapes (void);
const size_t numberOfShapes = 660;
// config
const size_t keySetsPerShape = 1;
const size_t minN = 1000;
const size_t maxN = 1000;
const size_t stepN = 1000;
const double minC = 1.2;
const double maxC = 1.4;
const double stepC = 0.1;

/**
 * General structure of a benchmark
 *
 * The generateKeySet () is not so fast, thus each KeySet will be only generated once and fed to all benchmarks.
 * The initBenchmarks is called before the generation starts, the runBenchmarks runs for each KeySet and the deinitBenchmarks run at the end.
 * The results store the benchmarks results.
 *
 */
typedef void (*initBenchmarkf) (size_t **);
typedef void (*deinitBenchmarkf) (size_t *);
typedef void (*runBenchmarkf) (KeySet *, size_t *, size_t, size_t, size_t, size_t);
typedef struct
{
	size_t * results;
	initBenchmarkf initBenchmark;
	runBenchmarkf runBenchmark;
	deinitBenchmarkf deinitBenchmark;
} Benchmark;

/**
 * START ======================= Compares the Opmphm Hash Function vs Hash Function suggested by Fox et al. ========================== START
 *
 * This comparison counts how often the mapping process needs to be done for one KeySet. The Opmphm Hash Function uses one seed
 * for each KeySet, the Hash Function suggested by Fox et al. uses for each mapping OPMPHMTUPLE seeds.
 */

const size_t benchmarkAllHashFunctionMaxCalls = 3;
/**
 * Common functions
 */

/**
 * @brief Init the result
 *
 * One result field has the following attributes:
 *
 * * n
 * * r
 * * shapes
 * * keySetsPerShape
 *
 * Malloc and init.
 *
 * @param results the results not initialized
 */
void benchmarkAllHashFunctionInit (size_t ** results)
{
	const size_t nCount = getNCount ();
	const size_t cCount = getCCount ();
	*results = elektraMalloc (sizeof (size_t) * nCount * cCount * numberOfShapes * keySetsPerShape);
	if (!*results) printStderrExit ("malloc");
	memset (*results, 0, sizeof (size_t) * nCount * cCount * numberOfShapes * keySetsPerShape);
}

/**
 * @brief Write result to filename and deinit
 *
 * One result field has the following attributes:
 *
 * * n
 * * r
 * * shapes
 * * keySetsPerShape
 *
 * Write out the data in csv format. First row contains n/r/w, the second the shape Index (which shape was used) and
 * the following rows contains the number of mapping calls for each KeySet respective to the shape n and r.
 *
 * @param results the results not initialized
 * @param filename output filename
 */
void benchmarkAllHashFunctionDeinit (size_t * results, const char * filename)
{
	FILE * out = openOutFileWithRPartitepostfix (filename);
	if (!out) printStderrExit ("can not open out file");
	const size_t cCount = getCCount ();
	size_t n;
	size_t nIndex;
	fprintf (out, "n/r/size;shapeIndex;data\n");
	for (n = minN, nIndex = 0; n <= maxN; n += stepN, ++nIndex)
	{
		double c;
		size_t cIndex;
		for (c = minC, cIndex = 0; c <= maxC; c += stepC, ++cIndex)
		{
			for (size_t s = 0; s < numberOfShapes; ++s)
			{
				fprintf (out, "%lu/%f/%lu;%lu", n, c, (size_t) ((n * c / OPMPHMR_PARTITE) + 1) * OPMPHMR_PARTITE, s);
				for (size_t i = 0; i < keySetsPerShape; ++i)
				{
					fprintf (out, ";%lu",
						 results[nIndex * (cCount * numberOfShapes * keySetsPerShape) +
							 cIndex * (numberOfShapes * keySetsPerShape) + s * (keySetsPerShape) + i]);
				}
				fprintf (out, "\n");
			}
		}
	}
	fclose (out);
	elektraFree (results);
}

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
FoxHash * createFoxHash (int32_t * initSeed, KeySet * ks)
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
void destroyFoxHash (FoxHash * f)
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
int32_t foxHash (FoxHash * f, const void * key)
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

void benchmarkFoxHashFunctionDeinit (size_t * results)
{
	benchmarkAllHashFunctionDeinit (results, "opmphmFoxHashFuction");
}

void benchmarkFoxHashFunctionRun (KeySet * ks, size_t * results, size_t n, size_t nIndex, size_t shapeIndex, size_t keysetIndex)
{
	const size_t cCount = getCCount ();
	size_t cIndex;
	double c;
	for (cIndex = 0, c = minC; c <= maxC; c += stepC, ++cIndex)
	{
		FoxHash * fHash[OPMPHMR_PARTITE];
		Opmphm * opmphm = opmphmNew ();
		if (!opmphm) printStderrExit ("opmphm");
		OpmphmGraph * graph = opmphmGraphNew (opmphm, n, c);
		if (!graph) printStderrExit ("graph");
		size_t calls = 0;
		int cyclic;
		do
		{
			// init fox hash
			for (uint8_t r = 0; r < OPMPHMR_PARTITE; ++r)
			{
				int32_t hashseed;
				if (getRandomSeed (&hashseed) != &hashseed) printStderrExit ("Seed Parsing Error or feed me more seeds");

				fHash[r] = createFoxHash (&hashseed, ks);
				if (!fHash[r]) printStderrExit ("foxhash");
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
		} while (cyclic && calls < benchmarkAllHashFunctionMaxCalls);
		results[nIndex * (cCount * numberOfShapes * keySetsPerShape) + cIndex * (numberOfShapes * keySetsPerShape) +
			shapeIndex * (keySetsPerShape) + keysetIndex] = calls;

		opmphmDel (opmphm);
		opmphmGraphDel (graph);
	}
}

/**
 * Benchmark the opmphmMapping with the opmphmHashFunction
 */
void benchmarkOpmphmHashFunctionDeinit (size_t * results)
{
	benchmarkAllHashFunctionDeinit (results, "opmphmHashFuction");
}

void benchmarkOpmphmHashFunctionRun (KeySet * ks, size_t * results, size_t n, size_t nIndex, size_t shapeIndex, size_t keysetIndex)
{
	const size_t cCount = getCCount ();
	size_t cIndex = 0;
	double c;
	for (c = minC, cIndex = 0; c <= maxC; c += stepC, ++cIndex)
	{
		// benchmark
		Opmphm * opmphm = opmphmNew ();
		if (!opmphm) printStderrExit ("opmphm");
		OpmphmGraph * graph = opmphmGraphNew (opmphm, n, c);
		if (!graph) printStderrExit ("graph");
		OpmphmInit init;
		init.getString = elektraGetString;
		init.data = (void **)ks->array;
		int32_t hashseed;
		if (getRandomSeed (&hashseed) != &hashseed) printStderrExit ("Seed Parsing Error or feed me more seeds");
		init.initSeed = hashseed;
		int ret;
		size_t calls = 0;
		do
		{
			ret = opmphmMapping (opmphm, graph, &init, n);
			++calls;
		} while (ret && calls < benchmarkAllHashFunctionMaxCalls);
		opmphmDel (opmphm);
		opmphmGraphDel (graph);
		results[nIndex * (cCount * numberOfShapes * keySetsPerShape) + cIndex * (numberOfShapes * keySetsPerShape) +
			shapeIndex * (keySetsPerShape) + keysetIndex] = calls;
	}
}

/**
 * END ========================= Compares the Opmphm Hash Function vs Hash Function suggested by Fox at al. ============================ END
 */


int main (int argc ELEKTRA_UNUSED, char ** argv ELEKTRA_UNUSED)
{
	// define benchmarks
	Benchmark opmphmHashFunction;
	opmphmHashFunction.initBenchmark = benchmarkAllHashFunctionInit;
	opmphmHashFunction.deinitBenchmark = benchmarkOpmphmHashFunctionDeinit;
	opmphmHashFunction.runBenchmark = benchmarkOpmphmHashFunctionRun;
	Benchmark foxHashFunction;
	foxHashFunction.initBenchmark = benchmarkAllHashFunctionInit;
	foxHashFunction.deinitBenchmark = benchmarkFoxHashFunctionDeinit;
	foxHashFunction.runBenchmark = benchmarkFoxHashFunctionRun;
	// add to benchmarks array
	Benchmark benchmarks[2] = { opmphmHashFunction, foxHashFunction };
	const int benchmarksCount = 2;
	// init
	for (int b = 0; b < benchmarksCount; ++b)
	{
		benchmarks[b].initBenchmark (&benchmarks[b].results);
	}
	KeySetShape * keySetShapes = getKeySetShapes ();
	size_t n;
	size_t nIndex;
	for (n = minN, nIndex = 0; n <= maxN; n += stepN, ++nIndex)
	{
		for (size_t s = 0; s < numberOfShapes; ++s)
		{
			for (size_t i = 0; i < keySetsPerShape; ++i)
			{
				printf ("now at n: %lu/%lu shape: %lu/%lu\r", nIndex, getNCount (), s, numberOfShapes);
				int32_t seed;
				if (getRandomSeed (&seed) != &seed) printStderrExit ("Seed Parsing Error or feed me more seeds");
				KeySet * ks = generateKeySet (n, &seed, &keySetShapes[s]);
				// run
				for (int b = 0; b < benchmarksCount; ++b)
				{
					benchmarks[b].runBenchmark (ks, benchmarks[b].results, n, nIndex, s, i);
				}
				ksDel (ks);
			}
		}
	}
	elektraFree (keySetShapes);
	// deinit
	for (int b = 0; b < benchmarksCount; ++b)
	{
		benchmarks[b].deinitBenchmark (benchmarks[b].results);
	}
	return EXIT_SUCCESS;
}

/**
 * Benchmark helpers
 */

size_t getNCount (void)
{
	size_t nCount = 0;
	for (size_t n = minN; n <= maxN; n += stepN)
	{
		++nCount;
	}
	return nCount;
}

size_t getCCount (void)
{
	size_t cCount = 0;
	for (double c = minC; c <= maxC; c += stepC)
	{
		++cCount;
	}
	return cCount;
}

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
// supports OPMPHMTUPLE < 100
static FILE * openOutFileWithRPartitepostfix (const char * name)
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
static size_t getPower (size_t p, size_t q)
{
	size_t result = 1;
	for (size_t t = 0; t < q; ++t)
	{
		result *= p;
	}
	return result;
}

void printStderrExit (const char * msg)
{
	fprintf (stderr, "FATAL: %s\n", msg);
	exit (EXIT_FAILURE);
}

/**
 * The Key Set shapes
 */
// binary: only 0 or 1 as return
static size_t shapefConstBinary5 (size_t initSize ELEKTRA_UNUSED, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t maxLevel = 5;
	if (level > maxLevel) return 0;
	return 1;
}
static size_t shapefConstBinary10 (size_t initSize ELEKTRA_UNUSED, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t maxLevel = 10;
	if (level > maxLevel) return 0;
	return 1;
}
static size_t shapefConstBinary20 (size_t initSize ELEKTRA_UNUSED, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t maxLevel = 20;
	if (level > maxLevel) return 0;
	return 1;
}
static size_t shapefBinaryRand (size_t initSize ELEKTRA_UNUSED, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed)
{
	size_t maxLevel = 100;
	if (level > maxLevel) return 0;
	elektraRand (seed);
	if ((*seed % 5) == 0)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}
// non binary: 1 > as return
static size_t shapefConstBranch2 (size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t subKeys = 2;
	size_t vertices = 0;
	for (size_t i = 1; i < level; ++i)
	{
		vertices += getPower (subKeys, i);
	}
	if (subKeys > size || vertices > initSize)
	{
		return 0;
	}
	else
	{
		return subKeys;
	}
}
static size_t shapefConstBranch3 (size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t subKeys = 3;
	size_t vertices = 0;
	for (size_t i = 1; i < level; ++i)
	{
		vertices += getPower (subKeys, i);
	}
	if (subKeys > size || vertices > initSize)
	{
		return 0;
	}
	else
	{
		return subKeys;
	}
}
static size_t shapefConstBranch5 (size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t subKeys = 5;
	size_t vertices = 0;
	for (size_t i = 1; i < level; ++i)
	{
		vertices += getPower (subKeys, i);
	}
	if (subKeys > size || vertices > initSize)
	{
		return 0;
	}
	else
	{
		return subKeys;
	}
}
//~ static size_t shapefConstBranch11 (size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED)
//~ {
//~ size_t subKeys = 11;
//~ size_t vertices = 0;
//~ for (size_t i = 1; i < level; ++i)
//~ {
//~ vertices += getPower (subKeys, i);
//~ }
//~ if (subKeys > size || vertices > initSize)
//~ {
//~ return 0;
//~ }
//~ else
//~ {
//~ return subKeys;
//~ }
//~ }
static size_t shapefDynamicBranch5 (size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t branchFactor = 5;
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
	if (subKeys > size || vertices > initSize)
	{
		return 0;
	}
	else
	{
		return subKeys;
	}
}
static size_t shapefDynamicBranch10 (size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t branchFactor = 10;
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
	if (subKeys > size || vertices > initSize)
	{
		return 0;
	}
	else
	{
		return subKeys;
	}
}
static size_t shapefDynamicBranch15 (size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t branchFactor = 15;
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
	if (subKeys > size || vertices > initSize)
	{
		return 0;
	}
	else
	{
		return subKeys;
	}
}
static size_t shapefDynamicBranch20 (size_t initSize, size_t size, size_t level, int32_t * seed ELEKTRA_UNUSED)
{
	size_t branchFactor = 20;
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
	if (subKeys > size || vertices > initSize)
	{
		return 0;
	}
	else
	{
		return subKeys;
	}
}
// too random choosen random functions
//~ static size_t shapefBranchRand0 (size_t initSize ELEKTRA_UNUSED, size_t size, size_t level, int32_t * seed)
//~ {
//~ size_t maxLevel = 100;
//~ if (level > maxLevel) return 0;
//~ elektraRand (seed);
//~ if (!size) return 0;
//~ return *seed % size;
//~ }
//~ static size_t shapefBranchRand1 (size_t initSize ELEKTRA_UNUSED, size_t size, size_t level, int32_t * seed)
//~ {
//~ size_t maxLevel = 100;
//~ if (level > maxLevel) return 0;
//~ elektraRand (seed);
//~ size_t result = *seed % initSize;
//~ if (result > size)
//~ {
//~ return 0;
//~ }
//~ else
//~ {
//~ return result;
//~ }
//~ }


KeySetShape * getKeySetShapes (void)
{
	KeySetShape * out = elektraMalloc (sizeof (KeySetShape) * numberOfShapes);
	if (!out) printStderrExit ("malloc KeySetShapes");
	const unsigned int wordLength[4] = { 1, 5, 20, 50 };
	const unsigned int special[2] = { 5, 10 };
	const unsigned int parent[5] = { 0, 1, 5, 15, 30 };
	const KsShapeFunction shapeFunctions[11] = { shapefConstBinary5,    shapefConstBinary10,  shapefConstBinary20,
						     shapefBinaryRand,      shapefConstBranch2,   shapefConstBranch3,
						     shapefConstBranch5,    shapefDynamicBranch5, shapefDynamicBranch10,
						     shapefDynamicBranch15, shapefDynamicBranch20 };
	// numberOfShapes = 6 * 2 * 5 * shapefCount
	size_t shapeCount = 0;
	for (int w0 = 0; w0 < 4; ++w0)
	{
		for (int w1 = w0 + 1; w1 < 4; ++w1)
		{
			for (int s = 0; s < 2; ++s)
			{
				for (int p = 0; p < 5; ++p)
				{
					for (int sf = 0; sf < 11; ++sf)
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
	if (shapeCount != numberOfShapes) printStderrExit ("shapeCount != numberOfShapes");
	return out;
}

void showShape (void)
{
	int n = 10000;
	// SHAPE
	KeySetShape shape;
	shape.minWordLength = 5;
	shape.maxWordLength = 20;
	shape.special = 10;
	shape.parent = 1;
	shape.shapef = shapefDynamicBranch5;
	// SHAPE
	// GENERATE
	int32_t seed;
	if (getRandomSeed (&seed) != &seed) printStderrExit ("Seed Parsing Error or feed me more seeds");
	__asm__("");
	timeInit ();
	KeySet * ks = generateKeySet (n, &seed, &shape);
	timePrint ("generate");
	__asm__("");
	// GENERATE
	if (0)
	{
		// print KS
		Key * key;
		ksRewind (ks);
		while ((key = ksNext (ks)))
		{
			printf ("%s\n", keyName (key));
		}
		// print KS
	}
	ksDel (ks);
}
