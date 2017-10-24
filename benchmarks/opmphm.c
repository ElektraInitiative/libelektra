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
// generate KeySets
static KeySetShape * getKeySetShapes (void);
const size_t numberOfShapes = 8;


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
						fprintf (opmphmOut, "%lu",
							 opmphmResults[i * (numberOfShapes * cCount * seeds) + s * (cCount * seeds) +
								       cI * seeds + sI]);
						fprintf (foxOut, "%lu",
							 foxResults[i * (numberOfShapes * cCount * seeds) + s * (cCount * seeds) +
								    cI * seeds + sI]);
					}
					else
					{
						fprintf (opmphmOut, ";%lu",
							 opmphmResults[i * (numberOfShapes * cCount * seeds) + s * (cCount * seeds) +
								       cI * seeds + sI]);
						fprintf (foxOut, ";%lu",
							 foxResults[i * (numberOfShapes * cCount * seeds) + s * (cCount * seeds) +
								    cI * seeds + sI]);
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
 * START ======================================================= Mapping ============================================================= START
 *
 * This benchmark counts the opmphmMapping (...)  invocations until success, for each KeySet size (n) and space influencing parameter (c).
 * First the KeySets are build, for every KeySet size (n) there are numberOfShapes * keySetsPerShape KeySets.
 * Then the benchmarking for every KeySet size (n) and space influencing parameter (c) takes place, with a fixed set of seeds for
 * the opmphmMapping (...) invocations.
 * At the end the results are written out in the following format:
 *
 * trials;n_%luc_%lu;... (each n and c are unique)
 *
 * The number of needed seeds for this benchmarks is: nCount * numberOfShapes * keySetsPerShape (KeySets generation) + numberOfSeeds (tested
 * seeds)
 */

static void benchmarkMapping (void)
{
	const size_t nCount = 11;
	const size_t n[] = { 10, 20, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240 }; // 11
	const size_t cCount = 21;
	const double c[] = {
		0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0
	}; // 21
	const size_t keySetsPerShape = 125;
	const size_t numberOfKeySets = nCount * numberOfShapes * keySetsPerShape;

	const size_t numberOfSeeds = 10000;
	const size_t maxMappings = 10; // the maximum trials for one opmphmMapping (...) invocation series.

	// init seed population, used for opmphmMapping (...) invocation.
	int32_t * seeds = elektraMalloc (numberOfSeeds * sizeof (int32_t));
	if (!seeds)
	{
		printExit ("malloc");
	}
	// get seeds
	for (size_t i = 0; i < numberOfSeeds; ++i)
	{
		if (getRandomSeed (&seeds[i]) != &seeds[i]) printExit ("Seed Parsing Error or feed me more seeds");
	}

	// init results
	size_t * results = elektraMalloc (nCount * cCount * maxMappings * sizeof (size_t));
	if (!results)
	{
		printExit ("malloc");
	}
	memset (results, 0, nCount * cCount * maxMappings * sizeof (size_t));

	// Generate all KeySets
	KeySetShape * keySetShapes = getKeySetShapes ();
	KeySet ** keySetsCache = elektraMalloc (numberOfKeySets * sizeof (KeySet *));
	if (!keySetsCache)
	{
		printExit ("malloc");
	}
	printf ("KeySet Cache Build:\n");
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		printf ("now at: %lu/%lu\r", nI + 1, nCount);
		for (size_t shapeI = 0; shapeI < numberOfShapes; ++shapeI)
		{
			for (size_t ksPshapeI = 0; ksPshapeI < keySetsPerShape; ++ksPshapeI)
			{
				int32_t genSeed;
				if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
				keySetsCache[nI * (numberOfShapes * keySetsPerShape) + shapeI * keySetsPerShape + ksPshapeI] =
					generateKeySet (n[nI], &genSeed, &keySetShapes[shapeI]);
			}
		}
	}

	printf ("\nRun Benchmark:\n");
	// for all nCount
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		// and cCount
		for (size_t cI = 0; cI < cCount; ++cI)
		{
			// OPMPHM
			Opmphm * opmphm = opmphmNew ();
			if (!opmphm) printExit ("opmphm");
			OpmphmGraph * graph = opmphmGraphNew (opmphm, n[nI], opmphmMinC () + c[cI]);
			if (!graph) printExit ("graph");
			OpmphmInit init;
			init.getString = elektraGetString;
			// OPMPHM

			// go through all KeySets from n
			for (size_t ksCacheI = 0; ksCacheI < numberOfShapes * keySetsPerShape; ++ksCacheI)
			{
				printf ("now at: %lu/%lu\r",
					nI * (cCount * numberOfShapes * keySetsPerShape) + cI * (numberOfShapes * keySetsPerShape) +
						ksCacheI + 1,
					nCount * cCount * numberOfShapes * keySetsPerShape);
				// OPMPHM
				init.data = (void **)(keySetsCache[nI * (numberOfShapes * keySetsPerShape) + ksCacheI]->array);
				// OPMPHM

				// try each seed
				for (size_t seedI = 0; seedI < numberOfSeeds; ++seedI)
				{
					size_t mappings = 0; // counts mapping invocations
					// OPMPHM
					init.initSeed = seeds[seedI];
					// fresh OpmphmGraph
					memset (graph->vertices, 0, opmphm->componentSize * OPMPHMR_PARTITE * sizeof (OpmphmVertex));
					// do benchmark
					int ret;
					do
					{
						ret = opmphmMapping (opmphm, graph, &init, n[nI]);
						++mappings;
					} while (ret && mappings < maxMappings);
					// OPMPHM
					if (mappings < 1 || mappings > maxMappings)
					{
						printExit ("benchmarkSeedRangeMappingCount: mappings out of range");
					}
					// shift, because 0 not used
					--mappings;
					++results[nI * (cCount * maxMappings) + cI * maxMappings + mappings];
				}
			}
			// OPMPHM
			opmphmDel (opmphm);
			opmphmGraphDel (graph);
			// OPMPHM
		}
	}
	printf ("\n");

	/*
	 * results sanity check
	 *
	 * each n and c should have in sum (numberOfShapes * keySetsPerShape) for each KeySet times (numberOfSeeds) seeds trials
	 */
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		for (size_t cI = 0; cI < cCount; ++cI)
		{
			size_t sum = 0;
			for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
			{
				sum += results[nI * (cCount * maxMappings) + cI * maxMappings + mappingI];
			}
			if (sum != numberOfShapes * keySetsPerShape * numberOfSeeds)
			{
				printExit ("benchmarkSeedRangeMappingCount: results sanity check failed");
			}
		}
	}

	// write out
	FILE * out = openOutFileWithRPartitePostfix ("benchmark_opmphm_mapping");
	if (!out)
	{
		printExit ("open out file");
	}
	// print header
	fprintf (out, "trials");
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		for (size_t cI = 0; cI < cCount; ++cI)
		{
			fprintf (out, ";n_%luc_%f", n[nI], opmphmMinC () + c[cI]);
		}
	}
	fprintf (out, "\n");
	// print data
	for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
	{
		fprintf (out, "%lu", mappingI + 1); // unshift, because 0 is not a result
		for (size_t nI = 0; nI < nCount; ++nI)
		{
			for (size_t cI = 0; cI < cCount; ++cI)
			{
				fprintf (out, ";%lu", results[nI * (cCount * maxMappings) + cI * maxMappings + mappingI]);
			}
		}
		fprintf (out, "\n");
	}

	// cleanup
	for (size_t i = 0; i < numberOfKeySets; ++i)
	{
		ksDel (keySetsCache[i]);
	}
	elektraFree (keySetsCache);
	fclose (out);
	elektraFree (keySetShapes);
	elektraFree (seeds);
	elektraFree (results);
}

/**
 * END ========================================================= Mapping =============================================================== END
 */

/**
 * START ================================================= Prints all KeySetShapes =================================================== START
 */

static void benchmarkPrintAllKeySetShapes (void)
{
	const size_t n = 30;
	int32_t seed = 47658589;
	KeySetShape * keySetShapes = getKeySetShapes ();
	for (size_t shapeId = 0; shapeId < numberOfShapes; ++shapeId)
	{
		int32_t s = seed;
		//~ timeInit ();
		KeySet * ks = generateKeySet (n, &s, &keySetShapes[shapeId]);
		//~ timePrint ("generateKeySet:");
		// print KS
		if (1)
		{
			printf (" ======================= shapeId %lu =======================\n\n", shapeId);
			Key * key;
			ksRewind (ks);
			while ((key = ksNext (ks)))
			{
				printf ("%s\n", keyName (key));
			}
			printf ("\n ======================== size %li ========================\n\n", ksGetSize (ks));
		}
		ksDel (ks);
	}
	elektraFree (keySetShapes);
}

/**
 * END =================================================== Prints all KeySetShapes ===================================================== END
 */

int main (int argc, char ** argv)
{
	// define all benchmarks
	const size_t benchmarksCount = 4;
	Benchmark benchmarks[benchmarksCount];
	// hashfunctiontime
	char * benchmarkNameHashFunctionTime = "hashfunctiontime";
	benchmarks[0].name = benchmarkNameHashFunctionTime;
	benchmarks[0].benchmarkF = benchmarkHashFunctionTime;
	// hashfunctionvs
	char * benchmarkNameHashFunctionVs = "hashfunctionvs";
	benchmarks[1].name = benchmarkNameHashFunctionVs;
	benchmarks[1].benchmarkF = benchmarkHashFunctionVs;
	// mapping
	char * benchmarkNameMapping = "mapping";
	benchmarks[2].name = benchmarkNameMapping;
	benchmarks[2].benchmarkF = benchmarkMapping;
	// printallkeysetshapes
	char * benchmarkNamePrintAllKeySetShapes = "printallkeysetshapes";
	benchmarks[3].name = benchmarkNamePrintAllKeySetShapes;
	benchmarks[3].benchmarkF = benchmarkPrintAllKeySetShapes;

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


/**
 * every key name is unique and goes 1 level deep
 */
static void shapefConstBinary (const size_t initSize ELEKTRA_UNUSED, size_t size ELEKTRA_UNUSED, size_t level ELEKTRA_UNUSED,
			       int32_t * seed ELEKTRA_UNUSED, KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	ret->label = 0;
	ret->subKeys = 0;
}
/**
 * binary tree
 */
static void shapefBinaryBranch (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
				KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	size_t subKeys = 2;
	ret->label = 0;
	if (getPower (subKeys, level) > initSize)
	{
		ret->subKeys = 0;
	}
	else
	{
		ret->subKeys = subKeys;
	}
}
/**
 * every parent has n/branchfactor children
 */
static void shapefDynamicBranch (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
				 KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	size_t branchRatio = 9;
	ret->label = 0;
	size_t subKeys = (initSize / branchRatio);
	if (subKeys < 2)
	{
		subKeys = 2;
	}
	if (getPower (subKeys, level) > initSize)
	{
		ret->subKeys = 0;
	}
	else
	{
		ret->subKeys = subKeys;
	}
}
/**
 * all key names have a common start, startLevel length
 */
static void shapefLateDynamicBranch (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
				     KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	size_t startLevel = 5;
	size_t branchRatio = 9;
	ret->label = 0;
	if (level < startLevel)
	{
		ret->subKeys = 1;
		return;
	}
	level -= startLevel;
	size_t subKeys = (initSize / branchRatio);
	if (subKeys < 2)
	{
		subKeys = 2;
	}
	if (getPower (subKeys, level) > initSize)
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
static void * shapeCommonStartEndInit (void)
{
	uint8_t * data = elektraMalloc (sizeof (uint8_t));
	if (!data)
	{
		return NULL;
	}
	*data = 0;
	return data;
}
static void shapeCommonStartEndDel (void * data)
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
 * modules, level 1 keys same, one level 2 key stores the modules. Like system/elektra.
 */
static void * shapeModulesInit (void)
{
	// three boolean flags if the respective label where set, the fourth counts from 1 to 3 for label assignment
	void * data = elektraMalloc (4 * sizeof (uint8_t));
	if (!data)
	{
		return NULL;
	}
	uint8_t * d = data;
	d[0] = 0;
	d[1] = 0;
	d[2] = 0;
	d[3] = 1;
	return data;
}
static void shapeModulesDel (void * data)
{
	elektraFree (data);
}
static void shapefModules (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
			   KsShapeFunctionReturn * ret, void * data)
{
	// label 1 5 subKeys
	// label 2 10 subKeys
	// label 3 20 subKeys
	ssize_t modulesKeys[3] = { 5, 10, 15 };
	uint8_t * d = data;
	uint8_t * firstSet = &d[0];
	uint8_t * secondSet = &d[1];
	uint8_t * thirdSet = &d[2];
	uint8_t * assign = &d[3];
	if (level == 1)
	{
		// common start, simulates elektra in system/elektra
		ret->subKeys = 1;
		ret->label = 0;
	}
	else if (level == 2)
	{
		// common name, simulates modules in system/elektra/modules
		// calculates how many modules have space
		ret->subKeys = 0;
		ssize_t remainingSize = initSize;
		uint8_t isSpace = 1;
		uint8_t l = 0;
		while (isSpace)
		{
			if (remainingSize - modulesKeys[l] < 0)
			{
				isSpace = 0;
			}
			else
			{
				remainingSize -= modulesKeys[l];
				l = (l + 1) % 3;
				++ret->subKeys;
			}
		}
		// add solo keys
		ret->subKeys += remainingSize;
		ret->label = 0;
	}
	else if (level == 3)
	{
		// give each modules ret->subKeys * 5 subKeys
		if (!*firstSet)
		{
			ret->subKeys = 1;
			ret->label = 1;
			*firstSet = 1;
		}
		else if (!*secondSet)
		{
			ret->subKeys = 2;
			ret->label = 2;
			*secondSet = 1;
		}
		else if (!*thirdSet)
		{
			ret->subKeys = 3;
			ret->label = 3;
			*thirdSet = 1;
		}
		else
		{
			// assign
			ret->subKeys = -1;
			ret->label = *assign;
			*assign = (*assign % 3) + 1;
		}
	}
	else if (level == 4)
	{
		// the 5 in ret->subKeys * 5
		ret->subKeys = 5;
		ret->label = 0;
	}
	else
	{
		// terminate keys
		ret->subKeys = 0;
		ret->label = 0;
	}
}
/**
 * always wider, subKeys are incremented by one every level
 */
static void shapefWide (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
			KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	ret->label = 0;
	size_t startSubKeys = 2;
	// determine to which level it is possible to go
	size_t l = 0; // level 0 should have 2 subs
	size_t keysOnLevel = startSubKeys;
	while (keysOnLevel <= initSize)
	{
		++l;
		keysOnLevel *= startSubKeys + l;
	}
	if (level < l)
	{
		ret->subKeys = startSubKeys + level;
	}
	else
	{
		ret->subKeys = 0;
	}
}
/**
 * always tighter, subKeys are decrementing by one every level till two is reached
 */
static void shapefTight (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
			 KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	ret->label = 0;
	size_t startSubKeys = 2;
	// determine to which level it is possible to go
	size_t l = 0; // level 0 should have 2 subs
	size_t keysOnLevel = startSubKeys;
	while (keysOnLevel <= initSize)
	{
		++l;
		keysOnLevel *= startSubKeys + l;
	}
	if (level < l)
	{
		ret->subKeys = startSubKeys + l - level - 1;
	}
	else
	{
		ret->subKeys = 0;
	}
}

/**
 * @brief Set the shape functions and parameters together to get the KeySetShape population.
 *
 * @retval KeySetShape * on success
 */
static KeySetShape * getKeySetShapes (void)
{
	KeySetShape * out = elektraMalloc (sizeof (KeySetShape) * numberOfShapes);
	if (!out) printExit ("malloc KeySetShapes");
	size_t shapeCount = 0;
	// shapefConstBinary
	out[shapeCount].minWordLength = 1;
	out[shapeCount].maxWordLength = 21;
	out[shapeCount].special = 127;
	out[shapeCount].parent = 0;
	out[shapeCount].shapeInit = NULL;
	out[shapeCount].shapef = shapefConstBinary;
	out[shapeCount].shapeDel = NULL;
	++shapeCount;

	// shapefBinaryBranch
	out[shapeCount].minWordLength = 1;
	out[shapeCount].maxWordLength = 1;
	out[shapeCount].special = 50;
	out[shapeCount].parent = 7;
	out[shapeCount].shapeInit = NULL;
	out[shapeCount].shapef = shapefBinaryBranch;
	out[shapeCount].shapeDel = NULL;
	++shapeCount;

	// shapefDynamicBranch
	out[shapeCount].minWordLength = 1;
	out[shapeCount].maxWordLength = 11;
	out[shapeCount].special = 50;
	out[shapeCount].parent = 7;
	out[shapeCount].shapeInit = NULL;
	out[shapeCount].shapef = shapefDynamicBranch;
	out[shapeCount].shapeDel = NULL;
	++shapeCount;

	// shapefLateDynamicBranch
	out[shapeCount].minWordLength = 1;
	out[shapeCount].maxWordLength = 11;
	out[shapeCount].special = 50;
	out[shapeCount].parent = 7;
	out[shapeCount].shapeInit = NULL;
	out[shapeCount].shapef = shapefLateDynamicBranch;
	out[shapeCount].shapeDel = NULL;
	++shapeCount;

	// shapefWide
	out[shapeCount].minWordLength = 1;
	out[shapeCount].maxWordLength = 11;
	out[shapeCount].special = 50;
	out[shapeCount].parent = 7;
	out[shapeCount].shapeInit = NULL;
	out[shapeCount].shapef = shapefWide;
	out[shapeCount].shapeDel = NULL;
	++shapeCount;

	// shapefTight
	out[shapeCount].minWordLength = 1;
	out[shapeCount].maxWordLength = 11;
	out[shapeCount].special = 50;
	out[shapeCount].parent = 7;
	out[shapeCount].shapeInit = NULL;
	out[shapeCount].shapef = shapefTight;
	out[shapeCount].shapeDel = NULL;
	++shapeCount;

	// shapefCommonStartEnd
	out[shapeCount].minWordLength = 1;
	out[shapeCount].maxWordLength = 21;
	out[shapeCount].special = 50;
	out[shapeCount].parent = 0;
	out[shapeCount].shapeInit = shapeCommonStartEndInit;
	out[shapeCount].shapef = shapefCommonStartEnd;
	out[shapeCount].shapeDel = shapeCommonStartEndDel; // remove f
	++shapeCount;

	// shapefModules
	out[shapeCount].minWordLength = 1;
	out[shapeCount].maxWordLength = 11;
	out[shapeCount].special = 50;
	out[shapeCount].parent = 7;
	out[shapeCount].shapeInit = shapeModulesInit;
	out[shapeCount].shapef = shapefModules;
	out[shapeCount].shapeDel = shapeModulesDel;
	++shapeCount;

	if (shapeCount != numberOfShapes) printExit ("shapeCount != numberOfShapes");
	return out;
}
