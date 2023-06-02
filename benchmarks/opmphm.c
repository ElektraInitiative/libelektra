/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map C benchmark.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// ==== DEFINE SECTION ====
#define _GNU_SOURCE
#define KDBRAND_BENCHMARK // allows the seed injection into Elektra
// uncomment to use OPENMP and set USE_OPENMP in CMakeLists.txt
//~ #define USE_OPENMP

#ifdef USE_OPENMP
// set here you number of threads
#define NUMBEROFTHREADS 8
#else
#define NUMBEROFTHREADS 1
#endif

// ==== INCLUDE SECTION ====
#include "./benchmarks.h"
#ifdef HAVE_HSEARCHR
#include <search.h>
#endif

#ifdef USE_OPENMP
#include <omp.h>
#endif

#include "../src/libs/core/opmphm.c"
#include "../src/libs/core/opmphmpredictor.c"
#include "../src/libs/core/rand.c"
#include <sys/time.h>

#include <internal/core/keyset.h>
#include <internal/core/lookup.h>
#include <internal/macros/attributes.h>

int32_t elektraRandBenchmarkInitSeed;

// benchmarks helpers
static int32_t * getRandomSeed (int32_t * seed);
static FILE * openOutFileWithRPartitePostfix (const char * name, uint8_t r);
static const char * getString (void * data);
static size_t getPower (size_t p, size_t q);
static int cmpInteger (const void * a, const void * b);
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
	size_t numberOfSeedsNeeded;
	void (*benchmarkF) (char *);
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
static void benchmarkHashFunctionTime (char * name)
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
	printf ("Run Benchmark %s:\n", name);
	KeySetShape * keySetShapes = getKeySetShapes ();
	for (size_t i = 0; i < nCount; ++i)
	{
		for (size_t s = 0; s < numberOfShapes; ++s)
		{
			printf ("now at n: %zu/%zu shape: %zu/%zu\r", i, nCount, s, numberOfShapes);
			fflush (stdout);
			int32_t seed;
			if (getRandomSeed (&seed) != &seed) printExit ("Seed Parsing Error or feed me more seeds");
			KeySet * ks = generateKeySet (n[i], &seed, &keySetShapes[s]);
			ssize_t ksSize = ksGetSize (ks);
			for (size_t r = 0; r < runs; ++r)
			{
				Key * key;

				struct timeval start;
				struct timeval end;
				__asm__ ("");
				gettimeofday (&start, 0);
				__asm__ ("");
				// measure
				for (elektraCursor it = 0; it < ksSize; ++it)
				{
					key = ksAtCursor (ks, it);
					__asm__ ("");
					opmphmHashfunction (keyName (key), strlen (keyName (key)), 1337);
					__asm__ ("");
				}

				__asm__ ("");
				gettimeofday (&end, 0);
				__asm__ ("");
				results[i * (numberOfShapes * runs) + s * runs + r] =
					(end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
			}
			ksDel (ks);
		}
	}
	elektraFree (keySetShapes);
	// wirte out results
	FILE * out = openOutFileWithRPartitePostfix ("benchmark_opmphm_hashfunctiontime", 0);
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
				fprintf (out, "%zu", n[i]);
			}
			else
			{
				fprintf (out, ";%zu", n[i]);
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
					fprintf (out, "%zu", results[i * (numberOfShapes * runs) + s * runs + r]);
				}
				else
				{
					fprintf (out, ";%zu", results[i * (numberOfShapes * runs) + s * runs + r]);
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
 * START ======================================================= Mapping ============================================================= START
 *
 * This benchmark counts the opmphmMapping (...)  invocations until success, for each KeySet size (n) and space influencing parameter (c).
 * First the KeySets are build, for every KeySet size (n) there are numberOfShapes * keySetsPerShape KeySets.
 * Then the benchmarking for every KeySet size (n) and space influencing parameter (c) takes place, with a fixed set of seeds for
 * the opmphmMapping (...) invocations.
 * At the end the results are written out in the following format:
 *
 * trials;n_%zuc_%f;... (each n and c are unique)
 *
 * The number of needed seeds for this benchmarks is: nCount * numberOfShapes * keySetsPerShape (KeySets generation) + numberOfSeeds (tested
 * seeds)
 */

static void benchmarkMappingCheckOpmphm (Opmphm * opmphm, OpmphmGraph * graph, size_t n, OpmphmInit * init, size_t mappings,
					 size_t maxMappings)
{
	if (n < 5 && mappings != maxMappings)
	{
		// assign
		if (opmphmAssignment (opmphm, graph, n, 1))
		{
			printExit ("check assignment failed");
		}
		for (size_t i = 0; i < n; ++i)
		{
			if (i != opmphmLookup (opmphm, n, init->getName (init->data[i])))
			{
				printExit ("check assignment failed");
			}
		}
		opmphmClear (opmphm);
	}
}

static void benchmarkMapping (char * name)
{
	size_t rUniPar = 3;
	const size_t nCount = 15;
	const size_t n[] = { 10, 15, 20, 30, 40, 60, 80, 120, 160, 240, 320, 480, 640, 960, 1280 }; // 15
	const size_t cCount = 15;
	const double c[] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5 }; // 15
	const size_t keySetsPerShape = 20;
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
		printf ("now at: %zu/%zu\r", nI + 1, nCount);
		fflush (stdout);
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

	printf ("\nRun Benchmark %s:\n", name);

#ifdef USE_OPENMP
	omp_set_num_threads (NUMBEROFTHREADS);
	// lock
	omp_lock_t writeLock;
	omp_init_lock (&writeLock);
#endif
	// split
	if (numberOfSeeds % NUMBEROFTHREADS != 0) printExit ("seeds % NUMBEROFTHREADS != 0");
	size_t partSize = numberOfSeeds / NUMBEROFTHREADS;

	// init threads local results
	size_t * localResults[NUMBEROFTHREADS];
	for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
	{
		localResults[i] = elektraMalloc (nCount * cCount * maxMappings * sizeof (size_t));
		if (!localResults[i])
		{
			printExit ("malloc");
		}
	}

	// for all nCount
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		// and cCount
		for (size_t cI = 0; cI < cCount; ++cI)
		{
			printf ("now at: n = %zu/%zu c = %zu/%zu\r", nI + 1, nCount, cI + 1, cCount);
			fflush (stdout);
			// OPMPHM for all threads
			Opmphm * opmphms[NUMBEROFTHREADS];
			OpmphmGraph * graphs[NUMBEROFTHREADS];
			for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
			{
				opmphms[i] = opmphmNew ();
				if (!opmphms[i]) printExit ("opmphm");
				graphs[i] = opmphmGraphNew (opmphms[i], rUniPar, n[nI], opmphmMinC (rUniPar) + c[cI]);
				if (!graphs[i]) printExit ("graph");
			}
			// OPMPHM

			// go through all KeySets from n
			for (size_t ksCacheI = 0; ksCacheI < numberOfShapes * keySetsPerShape; ++ksCacheI)
			{
				KeySet * ks = keySetsCache[nI * (numberOfShapes * keySetsPerShape) + ksCacheI];
#ifdef USE_OPENMP
#pragma omp parallel
#endif
				{
					size_t threadI = 0;
					// OPMPHM
					OpmphmInit init;
					init.getName = getString;
					init.data = (void **) (ks->data->array);
// OPMPHM
#ifdef USE_OPENMP
					threadI = omp_get_thread_num ();
#endif
					// reset local result
					memset (localResults[threadI], 0, nCount * cCount * maxMappings * sizeof (size_t));

					// try each seed part
					for (size_t seedI = threadI * partSize; seedI < (threadI + 1) * partSize; ++seedI)
					{
						size_t mappings = 0; // counts mapping invocations
						// OPMPHM
						init.initSeed = seeds[seedI];
						// fresh OpmphmGraph
						opmphmGraphClear (opmphms[threadI], graphs[threadI]);
						// do benchmark
						int ret;
						do
						{
							ret = opmphmMapping (opmphms[threadI], graphs[threadI], &init, n[nI]);
							++mappings;
						} while (ret && mappings < maxMappings);
						// OPMPHM
						if (mappings < 1 || mappings > maxMappings)
						{
							printExit ("benchmarkSeedRangeMappingCount: mappings out of range");
						}
						// check opmphm
						benchmarkMappingCheckOpmphm (opmphms[threadI], graphs[threadI], n[nI], &init, mappings,
									     maxMappings);
						// save result
						// shift, because 0 not used
						--mappings;
						++localResults[threadI][nI * (cCount * maxMappings) + cI * maxMappings + mappings];
					}
#ifdef USE_OPENMP
					// write local to global
					omp_set_lock (&writeLock);
#endif
					for (size_t i = 0; i < nCount * cCount * maxMappings; ++i)
					{
						results[i] += localResults[threadI][i];
					}
#ifdef USE_OPENMP
					omp_unset_lock (&writeLock);
#endif
				}
			}
			for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
			{
				// OPMPHM
				opmphmDel (opmphms[i]);
				opmphmGraphDel (graphs[i]);
				// OPMPHM
			}
		}
		// end for all nCount
	}
#ifdef USE_OPENMP
	omp_destroy_lock (&writeLock);
#endif
	for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
	{
		free (localResults[i]);
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
	FILE * out = openOutFileWithRPartitePostfix ("benchmark_opmphm_mapping", rUniPar);
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
			fprintf (out, ";n_%zuc_%f", n[nI], opmphmMinC (rUniPar) + c[cI]);
		}
	}
	fprintf (out, "\n");
	// print data
	for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
	{
		fprintf (out, "%zu", mappingI + 1); // unshift, because 0 is not a result
		for (size_t nI = 0; nI < nCount; ++nI)
		{
			for (size_t cI = 0; cI < cCount; ++cI)
			{
				fprintf (out, ";%zu", results[nI * (cCount * maxMappings) + cI * maxMappings + mappingI]);
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
 * START ============================================== Mapping with Optimization ==================================================== START
 *
 * This benchmark counts the opmphmMapping (...)  invocations until success, for each KeySet size.
 * First the KeySets are build, for every KeySet size (n) there are numberOfShapes * keySetsPerShape KeySets.
 * Then the benchmarking for every KeySet size (n) takes place, with a fixed set of seeds for the opmphmMapping (...) invocations.
 * At the end the results are written out in the following format:
 *
 * trials;n_%zur_%uc_%f;... (each n is unique)
 *
 * The number of needed seeds for this benchmarks is: nCount * numberOfShapes * keySetsPerShape (KeySets generation) + numberOfSeeds (tested
 * seeds)
 */

static void benchmarkMappingOpt (char * name)
{
	// create the n array
	const size_t nCount = 132;
	size_t * n = elektraMalloc (nCount * sizeof (size_t));
	if (!n)
	{
		printExit ("malloc");
	}
	size_t controlCount = 0;
	for (size_t i = 2; i <= 38; ++i)
	{
		n[controlCount] = i;
		++controlCount;
	}
	for (size_t i = 39; i <= 239; i = i + 5)
	{
		n[controlCount] = i;
		++controlCount;
	}
	n[controlCount] = 240;
	++controlCount;
	for (size_t i = 259; i <= 1279; i = i + 20)
	{
		n[controlCount] = i;
		++controlCount;
	}
	n[controlCount] = 1280;
	++controlCount;
	if (controlCount != nCount)
	{
		printExit ("controlCount != nCount");
	}

	const size_t keySetsPerShape = 70;
	const size_t numberOfKeySets = nCount * numberOfShapes * keySetsPerShape;

	const size_t numberOfSeeds = 20000;
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
	size_t * results = elektraMalloc (nCount * maxMappings * sizeof (size_t));
	if (!results)
	{
		printExit ("malloc");
	}
	memset (results, 0, nCount * maxMappings * sizeof (size_t));

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
		printf ("now at: %zu/%zu\r", nI + 1, nCount);
		fflush (stdout);
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

	printf ("\nRun Benchmark %s:\n", name);

#ifdef USE_OPENMP
	omp_set_num_threads (NUMBEROFTHREADS);
	// lock
	omp_lock_t writeLock;
	omp_init_lock (&writeLock);
#endif
	// split
	if (numberOfSeeds % NUMBEROFTHREADS != 0) printExit ("seeds % NUMBEROFTHREADS != 0");
	size_t partSize = numberOfSeeds / NUMBEROFTHREADS;

	// init threads local results
	size_t * localResults[NUMBEROFTHREADS];
	for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
	{
		localResults[i] = elektraMalloc (nCount * maxMappings * sizeof (size_t));
		if (!localResults[i])
		{
			printExit ("malloc");
		}
	}

	// for all nCount
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		printf ("now at: n = %zu/%zu\r", nI + 1, nCount);
		fflush (stdout);
		// OPMPHM for all threads
		Opmphm * opmphms[NUMBEROFTHREADS];
		OpmphmGraph * graphs[NUMBEROFTHREADS];
		for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
		{
			opmphms[i] = opmphmNew ();
			if (!opmphms[i]) printExit ("opmphm");
			uint8_t r = opmphmOptR (n[nI]);
			graphs[i] = opmphmGraphNew (opmphms[i], r, n[nI], opmphmMinC (r) + opmphmOptC (n[nI]));
			if (!graphs[i]) printExit ("graph");
		}
		// OPMPHM

		// go through all KeySets from n
		for (size_t ksCacheI = 0; ksCacheI < numberOfShapes * keySetsPerShape; ++ksCacheI)
		{
			KeySet * ks = keySetsCache[nI * (numberOfShapes * keySetsPerShape) + ksCacheI];
#ifdef USE_OPENMP
#pragma omp parallel
#endif
			{
				size_t threadI = 0;
				// OPMPHM
				OpmphmInit init;
				init.getName = getString;
				init.data = (void **) (ks->data->array);
// OPMPHM
#ifdef USE_OPENMP
				threadI = omp_get_thread_num ();
#endif
				// reset local result
				memset (localResults[threadI], 0, nCount * maxMappings * sizeof (size_t));

				// try each seed part
				for (size_t seedI = threadI * partSize; seedI < (threadI + 1) * partSize; ++seedI)
				{
					size_t mappings = 0; // counts mapping invocations
					// OPMPHM
					init.initSeed = seeds[seedI];
					// fresh OpmphmGraph
					opmphmGraphClear (opmphms[threadI], graphs[threadI]);
					// do benchmark
					int ret;
					do
					{
						ret = opmphmMapping (opmphms[threadI], graphs[threadI], &init, n[nI]);
						++mappings;
					} while (ret && mappings < maxMappings);
					// OPMPHM
					if (mappings < 1 || mappings > maxMappings)
					{
						printExit ("benchmarkSeedRangeMappingCount: mappings out of range");
					}
					// check assignment
					if (nI < 5 && mappings != maxMappings)
					{
						// assign
						if (opmphmAssignment (opmphms[threadI], graphs[threadI], n[nI], 1))
						{
							printExit ("check assignment failed");
						}
						for (size_t i = 0; i < n[nI]; ++i)
						{
							if (i != opmphmLookup (opmphms[threadI], n[nI], init.getName (init.data[i])))
							{
								printExit ("check assignment failed");
							}
						}
						opmphmClear (opmphms[threadI]);
					}
					// save result
					// shift, because 0 not used
					--mappings;
					++localResults[threadI][nI * maxMappings + mappings];
				}
#ifdef USE_OPENMP
				// write local to global
				omp_set_lock (&writeLock);
#endif
				for (size_t i = 0; i < nCount * maxMappings; ++i)
				{
					results[i] += localResults[threadI][i];
				}
#ifdef USE_OPENMP
				omp_unset_lock (&writeLock);
#endif
			}
		}
		for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
		{
			// OPMPHM
			opmphmDel (opmphms[i]);
			opmphmGraphDel (graphs[i]);
			// OPMPHM
		}
		// end for all nCount
	}
#ifdef USE_OPENMP
	omp_destroy_lock (&writeLock);
#endif
	for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
	{
		free (localResults[i]);
	}
	printf ("\n");

	/*
	 * results sanity check
	 *
	 * each n should have in sum (numberOfShapes * keySetsPerShape) for each KeySet times (numberOfSeeds) seeds trials
	 */
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		size_t sum = 0;
		for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
		{
			sum += results[nI * maxMappings + mappingI];
		}
		if (sum != numberOfShapes * keySetsPerShape * numberOfSeeds)
		{
			printExit ("benchmarkSeedRangeMappingCount: results sanity check failed");
		}
	}

	// write out
	FILE * out = fopen ("benchmark_opmphm_mapping_opt.csv", "w");
	if (!out)
	{
		printExit ("open out file");
	}
	// print header
	fprintf (out, "trials");
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		fprintf (out, ";n_%zur_%uc_%f", n[nI], opmphmOptR (n[nI]), opmphmMinC (opmphmOptR (n[nI])) + opmphmOptC (n[nI]));
	}
	fprintf (out, "\n");
	// print data
	for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
	{
		fprintf (out, "%zu", mappingI + 1); // unshift, because 0 is not a result
		for (size_t nI = 0; nI < nCount; ++nI)
		{
			fprintf (out, ";%zu", results[nI * maxMappings + mappingI]);
		}
		fprintf (out, "\n");
	}

	// cleanup
	for (size_t i = 0; i < numberOfKeySets; ++i)
	{
		ksDel (keySetsCache[i]);
	}
	elektraFree (n);
	elektraFree (keySetsCache);
	fclose (out);
	elektraFree (keySetShapes);
	elektraFree (seeds);
	elektraFree (results);
}

/**
 * END ================================================ Mapping with Optimization ====================================================== END
 */

/**
 * START ================================================== Mapping All Seeds ======================================================== START
 *
 * This benchmark counts the opmphmMapping (...)  invocations until success, for each KeySet size and all seeds.
 * First the KeySets are build, for every KeySet size (n). Then the benchmarking for every KeySet size (n) takes place,
 * the seeds start at 1 and go to ELEKTRARANDMAX - 1 = 2147483646.
 * At the end the results are written out in the following format:
 *
 * trials;n_%zur_%uc_%f;... (each n is unique)
 *
 * The number of needed seeds for this benchmarks is: nCount (KeySets generation)
 */

static void benchmarkMappingAllSeeds (char * name)
{
	// create the n array
	const size_t nCount = 7;
	size_t * n = elektraMalloc (nCount * sizeof (size_t));
	if (!n)
	{
		printExit ("malloc");
	}
	n[0] = 9;
	n[1] = 29;
	n[2] = 49;
	n[3] = 69;
	n[4] = 89;
	n[5] = 109;
	n[6] = 129;

	// seeds limits
	const int32_t startSeed = 1;
	const int32_t endSeed = ELEKTRARANDMAX - 1; // = ELEKTRARANDMAX;

	const size_t maxMappings = 10; // the maximum trials for one opmphmMapping (...) invocation series.

	// init results
	size_t * results = elektraMalloc (nCount * maxMappings * sizeof (size_t));
	if (!results)
	{
		printExit ("malloc");
	}
	memset (results, 0, nCount * maxMappings * sizeof (size_t));

	// Generate all KeySets
	KeySetShape * keySetShapes = getKeySetShapes ();
	KeySet ** keySetsCache = elektraMalloc (nCount * sizeof (KeySet *));
	if (!keySetsCache)
	{
		printExit ("malloc");
	}
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		int32_t genSeed;
		if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
		keySetsCache[nI] = generateKeySet (n[nI], &genSeed, &keySetShapes[0]); // shape 0 is shapefConstBinary with 0 parents
	}

	printf ("\nRun Benchmark %s:\n", name);

#ifdef USE_OPENMP
	omp_set_num_threads (NUMBEROFTHREADS);
	// lock
	omp_lock_t writeLock;
	omp_init_lock (&writeLock);
#endif

	// split the job
	int32_t partIntervals[NUMBEROFTHREADS * 2];
	int32_t onePart = (endSeed - startSeed) / NUMBEROFTHREADS;
	int32_t iterateIntervals = startSeed;
	for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
	{
		if (i == NUMBEROFTHREADS - 1)
		{
			// give last thread the remaining seeds
			partIntervals[i * 2] = iterateIntervals;
			partIntervals[(i * 2) + 1] = endSeed;
		}
		else
		{
			partIntervals[i * 2] = iterateIntervals;
			partIntervals[(i * 2) + 1] = iterateIntervals + onePart - 1;
			iterateIntervals += onePart;
		}
	}

	// init threads local results
	size_t * localResults[NUMBEROFTHREADS];
	for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
	{
		localResults[i] = elektraMalloc (nCount * maxMappings * sizeof (size_t));
		if (!localResults[i])
		{
			printExit ("malloc");
		}
	}

	// for all nCount
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		// OPMPHM for all threads
		Opmphm * opmphms[NUMBEROFTHREADS];
		OpmphmGraph * graphs[NUMBEROFTHREADS];
		for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
		{
			opmphms[i] = opmphmNew ();
			if (!opmphms[i]) printExit ("opmphm");
			uint8_t r = opmphmOptR (n[nI]);
			graphs[i] = opmphmGraphNew (opmphms[i], r, n[nI], opmphmMinC (r) + opmphmOptC (n[nI]));
			if (!graphs[i]) printExit ("graph");
		}
		// OPMPHM

		KeySet * ks = keySetsCache[nI];
#ifdef USE_OPENMP
#pragma omp parallel
#endif
		{
			size_t threadI = 0;
			// OPMPHM
			OpmphmInit init;
			init.getName = getString;
			init.data = (void **) (ks->data->array);
			// OPMPHM

#ifdef USE_OPENMP
			threadI = omp_get_thread_num ();
#endif
			// reset local result
			memset (localResults[threadI], 0, nCount * maxMappings * sizeof (size_t));

			// try each seed part
			for (int32_t seed = partIntervals[threadI * 2];
			     partIntervals[threadI * 2] <= seed && seed <= partIntervals[(threadI * 2) + 1]; ++seed)
			{
				if (threadI == 0 && (seed % 1000) == 0)
				{
					printf ("now at: n = %zu/%zu and seed %i from %i\r", nI + 1, nCount, seed, partIntervals[1]);
					fflush (stdout);
				}
				size_t mappings = 0; // counts mapping invocations
				// OPMPHM
				init.initSeed = seed;
				// fresh OpmphmGraph
				opmphmGraphClear (opmphms[threadI], graphs[threadI]);
				// do benchmark
				int ret;
				do
				{
					ret = opmphmMapping (opmphms[threadI], graphs[threadI], &init, n[nI]);
					++mappings;
				} while (ret && mappings < maxMappings);
				// OPMPHM
				if (mappings < 1 || mappings > maxMappings)
				{
					printExit ("benchmarkSeedRangeMappingCount: mappings out of range");
				}
				// save result
				// shift, because 0 not used
				--mappings;
				++localResults[threadI][nI * maxMappings + mappings];
			}
#ifdef USE_OPENMP
			// write local to global
			omp_set_lock (&writeLock);
#endif
			for (size_t i = 0; i < nCount * maxMappings; ++i)
			{
				results[i] += localResults[threadI][i];
			}
#ifdef USE_OPENMP
			omp_unset_lock (&writeLock);
#endif
		}
		for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
		{
			// OPMPHM
			opmphmDel (opmphms[i]);
			opmphmGraphDel (graphs[i]);
			// OPMPHM
		}
		// end for all nCount
	}
#ifdef USE_OPENMP
	omp_destroy_lock (&writeLock);
#endif
	for (size_t i = 0; i < NUMBEROFTHREADS; ++i)
	{
		free (localResults[i]);
	}
	printf ("\n");

	/*
	 * results sanity check
	 *
	 * each n should have in sum endSeed - startSeed + 1 trials
	 */
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		size_t sum = 0;
		for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
		{
			sum += results[nI * maxMappings + mappingI];
		}
		if (sum != (size_t) endSeed - startSeed + 1)
		{
			printExit ("benchmarkSeedRangeMappingCount: results sanity check failed");
		}
	}

	// write out
	FILE * out = fopen ("benchmark_opmphm_mapping_allSeeds.csv", "w");
	if (!out)
	{
		printExit ("open out file");
	}
	// print header
	fprintf (out, "trials");
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		fprintf (out, ";n_%zur_%uc_%f", n[nI], opmphmOptR (n[nI]), opmphmMinC (opmphmOptR (n[nI])) + opmphmOptC (n[nI]));
	}
	fprintf (out, "\n");
	// print data
	for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
	{
		fprintf (out, "%zu", mappingI + 1); // unshift, because 0 is not a result
		for (size_t nI = 0; nI < nCount; ++nI)
		{
			fprintf (out, ";%zu", results[nI * maxMappings + mappingI]);
		}
		fprintf (out, "\n");
	}

	// cleanup
	for (size_t i = 0; i < nCount; ++i)
	{
		ksDel (keySetsCache[i]);
	}
	elektraFree (n);
	elektraFree (keySetsCache);
	fclose (out);
	elektraFree (keySetShapes);
	elektraFree (results);
}

/**
 * END ==================================================== Mapping All Seeds ========================================================== END
 */

/**
 * START ================================================== OPMPHM Build Time ======================================================== START
 *
 * This benchmark measures the time of the OPMPHM build.
 * Uses all KeySet shapes except 6, for all n (KeySet size) a fixed set of seeds is used to build the OPMPHM.
 * The keyset shape 6 is excluded, because previous evaluation had show that the results with that keyset shape
 * where unusable, due to the unnatural long key names.
 * For one n (KeySet size) ksPerN KeySets are used.
 * The results are written out in the following format:
 *
 * n;ks;time
 *
 * The number of needed seeds for this benchmarks is: (numberOfShapes - 1) * ( numberOfSeeds + nCount * ksPerN )
 */

/**
 * @brief Measures the OPMPHM build numberOfRepeats time and returns median
 *
 * @param ks the KeySet
 * @param repeats array to store repeated measurements
 * @param numberOfRepeats fields in repeats
 *
 * @retval median time
 */
static size_t benchmarkOPMPHMBuildTimeMeasure (KeySet * ks, size_t * repeats, size_t numberOfRepeats)
{
	for (size_t repeatsI = 0; repeatsI < numberOfRepeats; ++repeatsI)
	{
		// preparation for measurement
		struct timeval start;
		struct timeval end;
		Key * keySearchFor = ks->data->array[0]; // just some key
		Key * keyFound;
		// fresh OPMPHM
		if (ks->data->opmphm)
		{
			opmphmDel (ks->data->opmphm);
			ks->data->opmphm = NULL;
		}

		// START MEASUREMENT
		__asm__ ("");
		gettimeofday (&start, 0);
		__asm__ ("");

		keyFound = ksLookup (ks, keySearchFor, KDB_O_OPMPHM | KDB_O_NOCASCADING);

		__asm__ ("");
		gettimeofday (&end, 0);
		__asm__ ("");
		// END MEASUREMENT

		// save result
		repeats[repeatsI] = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);

		// sanity check
		if (!opmphmIsBuild (ks->data->opmphm))
		{
			printExit ("Sanity Check Failed: OPMPHM not used");
		}
		if (keyFound != keySearchFor)
		{
			printExit ("Sanity Check Failed: found wrong Key");
		}
	}
	// sort repeats
	qsort (repeats, numberOfRepeats, sizeof (size_t), cmpInteger);
	return repeats[numberOfRepeats / 2]; // take median
}

void benchmarkOPMPHMBuildTime (char * name)
{
	const size_t startN = 50;
	const size_t stepN = 500;
	const size_t endN = 20000;
	const size_t ksPerN = 5;
	const size_t numberOfSeeds = 51;
	const size_t numberOfRepeats = 7;

	// check config
	if (startN >= endN || startN == 0)
	{
		printExit ("startN >= endN || startN == 0");
	}
	if (numberOfRepeats % 2 == 0)
	{
		printExit ("numberOfRepeats is even");
	}
	if (numberOfSeeds % 2 == 0)
	{
		printExit ("numberOfSeeds is even");
	}
	if (ksPerN % 2 == 0)
	{
		printExit ("ksPerN is even");
	}

	// calculate counts
	size_t nCount = 0;
	for (size_t nI = startN; nI <= endN; nI += stepN)
	{
		++nCount;
	}

	// memory allocation and initialization
	// init seeds for mapping step in ksLookup (...)
	int32_t * seeds = elektraMalloc (numberOfSeeds * sizeof (int32_t));
	if (!seeds)
	{
		printExit ("malloc");
	}
	// init results
	size_t * results = elektraMalloc (nCount * ksPerN * numberOfSeeds * sizeof (size_t));
	if (!results)
	{
		printExit ("malloc");
	}
	// init repeats
	size_t * repeats = elektraMalloc (numberOfRepeats * sizeof (size_t));
	if (!repeats)
	{
		printExit ("malloc");
	}
	// init KeySetStorage
	KeySet ** keySetStorage = elektraMalloc (ksPerN * sizeof (KeySet *));
	if (!keySetStorage)
	{
		printExit ("malloc");
	}

	// get KeySet shapes
	KeySetShape * keySetShapes = getKeySetShapes ();

	printf ("Run Benchmark %s:\n", name);

	// for all KeySet shapes except 6
	for (size_t shapeI = 0; shapeI < numberOfShapes; ++shapeI)
	{
		if (shapeI == 6)
		{
			continue;
		}
		// get seeds for mapping step in ksLookup (...)
		for (size_t i = 0; i < numberOfSeeds; ++i)
		{
			if (getRandomSeed (&seeds[i]) != &seeds[i]) printExit ("Seed Parsing Error or feed me more seeds");
		}

		KeySetShape * usedKeySetShape = &keySetShapes[shapeI];

		// for all Ns
		for (size_t nI = startN; nI <= endN; nI += stepN)
		{
			printf ("now at: shape = %zu/%zu n = %zu/%zu\r", shapeI + 1, numberOfShapes, nI, endN);
			fflush (stdout);

			// generate KeySets
			int32_t genSeed;
			for (size_t ksI = 0; ksI < ksPerN; ++ksI)
			{
				if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
				keySetStorage[ksI] = generateKeySet (nI, &genSeed, usedKeySetShape);
			}

			// for all seeds
			for (size_t seedI = 0; seedI < numberOfSeeds; ++seedI)
			{
				// set seed to return by elektraRandGetInitSeed () in the lookup
				elektraRandBenchmarkInitSeed = seeds[seedI];

				// for all KeySets in the storage
				for (size_t ksI = 0; ksI < ksPerN; ++ksI)
				{
					// measure
					size_t res = benchmarkOPMPHMBuildTimeMeasure (keySetStorage[ksI], repeats, numberOfRepeats);

					// store res
					results[((nI - startN) / stepN) * ksPerN * numberOfSeeds + ksI * numberOfSeeds + seedI] = res;
				}
			}

			// free ks
			for (size_t ksI = 0; ksI < ksPerN; ++ksI)
			{
				ksDel (keySetStorage[ksI]);
			}
		}

		// write out
		FILE * out = openOutFileWithRPartitePostfix ("benchmark_opmphm_build_time", shapeI);
		if (!out)
		{
			printExit ("open out file");
		}
		// print header
		fprintf (out, "n;ks;time\n");
		// print data
		for (size_t nI = startN; nI <= endN; nI += stepN)
		{
			for (size_t ksI = 0; ksI < ksPerN; ++ksI)
			{
				for (size_t seedI = 0; seedI < numberOfSeeds; ++seedI)
				{
					fprintf (out, "%zu;%zu;%zu\n", nI, ksI,
						 results[((nI - startN) / stepN) * ksPerN * numberOfSeeds + ksI * numberOfSeeds + seedI]);
				}
			}
		}

		fclose (out);
	}
	printf ("\n");

	elektraFree (repeats);
	elektraFree (keySetStorage);
	elektraFree (keySetShapes);
	elektraFree (results);
	elektraFree (seeds);
}

/**
 * END ==================================================== OPMPHM Build Time ========================================================== END
 */

/**
 * START ================================================== OPMPHM Search Time ======================================================= START
 *
 * This benchmark measures the time of the OPMPHM search.
 * Uses all KeySet shapes except 6, for one n (KeySet size) ksPerN KeySets are used.
 * The keyset shape 6 is excluded, because previous evaluation had show that the results with that keyset shape
 * where unusable, due to the unnatural long key names.
 * Each measurement done with one KeySet is repeated numberOfRepeats time and summarized with the median.
 * For one n (KeySet size) the ksPerN results are also summarized with the median.
 * The results are written out in the following format:
 *
 * n;search_1;search_2;...;search_(numberOfSearches)
 *
 * The number of needed seeds for this benchmarks is: (numberOfShapes - 1) * nCount * ksPerN * (1  + searchesCount )
 */

/**
 * @brief Measures the OPMPHM search time, for searches random Keys, repeats the measurement numberOfRepeats time and returns the media.
 *
 * The OPMPHM build will be triggerd if KDB_OPMPHM is set!
 *
 * @param ks the KeySet
 * @param searches the number of searches to make
 * @param searchSeed the random seed used to determine the Keys to search
 * @param option the options passed to the ksLookup (...)
 * @param repeats array to store repeated measurements
 * @param numberOfRepeats fields in repeats
 *
 * @retval median time
 */
static size_t benchmarkSearchTimeMeasure (KeySet * ks, size_t searches, int32_t searchSeed, elektraLookupFlags option, size_t * repeats,
					  size_t numberOfRepeats)
{
	if (option & KDB_O_OPMPHM)
	{
		// trigger OPMPHM build if not build
		if (!opmphmIsBuild (ks->data->opmphm))
		{
			// set seed to return by elektraRandGetInitSeed () in the lookup
			elektraRandBenchmarkInitSeed = searchSeed;
			(void) ksLookup (ks, ks->data->array[0], KDB_O_OPMPHM | KDB_O_NOCASCADING);
			if (!opmphmIsBuild (ks->data->opmphm))
			{
				printExit ("trigger OPMPHM build");
			}
		}
	}
	for (size_t repeatsI = 0; repeatsI < numberOfRepeats; ++repeatsI)
	{
		// sanity checks
		if (option & KDB_O_OPMPHM)
		{
			if (!opmphmIsBuild (ks->data->opmphm))
			{
				printExit ("Sanity Check Failed: OPMPHM not here");
			}
		}
		else
		{
			if (ks->data->opmphm)
			{
				printExit ("Sanity Check Failed: OPMPHM here");
			}
		}

		// preparation for measurement
		struct timeval start;
		struct timeval end;
		Key * keyFound;
		int32_t actualSearchSeed = searchSeed;

		// START MEASUREMENT
		__asm__ ("");
		gettimeofday (&start, 0);
		__asm__ ("");

		for (size_t s = 1; s <= searches; ++s)
		{
			keyFound = ksLookup (ks, ks->data->array[actualSearchSeed % ks->data->size], option);
			if (!keyFound || keyFound != ks->data->array[actualSearchSeed % ks->data->size])
			{
				printExit ("Sanity Check Failed: found wrong Key");
			}
			elektraRand (&actualSearchSeed);
		}

		__asm__ ("");
		gettimeofday (&end, 0);
		__asm__ ("");
		// END MEASUREMENT

		// sanity checks
		if (option & KDB_O_OPMPHM)
		{
			if (!opmphmIsBuild (ks->data->opmphm))
			{
				printExit ("Sanity Check Failed: OPMPHM not here");
			}
		}
		else
		{
			if (ks->data->opmphm)
			{
				printExit ("Sanity Check Failed: OPMPHM here");
			}
		}

		// save result
		repeats[repeatsI] = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
	}
	// sort repeats
	qsort (repeats, numberOfRepeats, sizeof (size_t), cmpInteger);
	return repeats[numberOfRepeats / 2]; // take median
}

/**
 * @brief Common part of search time benchmarks, used by benchmarkOPMPHMSearchTime and benchmarkBinarySearchTime.
 *
 * @param outFileName the output file name
 * @param option the option to pass to the ksLookup (...)
 */
static void benchmarkSearchTime (char * name, char * outFileName, elektraLookupFlags option)
{
	const size_t startN = 50;
	const size_t stepN = 500;
	const size_t endN = 20000;
	const size_t ksPerN = 3;
	const size_t numberOfRepeats = 7;
	const size_t startSearches = 500;
	const size_t stepSearches = 500;
	const size_t endSearches = 32000;

	// check config
	if (startN >= endN || startN == 0)
	{
		printExit ("startN >= endN || startN == 0");
	}
	if (numberOfRepeats % 2 == 0)
	{
		printExit ("numberOfRepeats is even");
	}
	if (ksPerN % 2 == 0)
	{
		printExit ("ksPerN is even");
	}

	// calculate counts
	size_t nCount = 0;
	for (size_t nI = startN; nI <= endN; nI += stepN)
	{
		++nCount;
	}
	size_t searchesCount = 0;
	for (size_t searchesI = startSearches; searchesI <= endSearches; searchesI += stepSearches)
	{
		++searchesCount;
	}

	// memory allocation and initialization
	// init results
	size_t * results = elektraMalloc (nCount * searchesCount * sizeof (size_t));
	if (!results)
	{
		printExit ("malloc");
	}
	// init repeats
	size_t * repeats = elektraMalloc (numberOfRepeats * sizeof (size_t));
	if (!repeats)
	{
		printExit ("malloc");
	}
	// init partialResult
	size_t * partialResult = elektraMalloc (ksPerN * searchesCount * sizeof (size_t));
	if (!partialResult)
	{
		printExit ("malloc");
	}
	// init KeySetStorage
	KeySet ** keySetStorage = elektraMalloc (ksPerN * sizeof (KeySet *));
	if (!keySetStorage)
	{
		printExit ("malloc");
	}

	// get KeySet shapes
	KeySetShape * keySetShapes = getKeySetShapes ();

	printf ("Run Benchmark %s:\n", name);

	// for all KeySet shapes except 6
	for (size_t shapeI = 0; shapeI < numberOfShapes; ++shapeI)
	{
		if (shapeI == 6)
		{
			continue;
		}
		KeySetShape * usedKeySetShape = &keySetShapes[shapeI];

		// for all Ns
		for (size_t nI = startN; nI <= endN; nI += stepN)
		{
			printf ("now at: shape = %zu/%zu n = %zu/%zu\r", shapeI + 1, numberOfShapes, nI, endN);
			fflush (stdout);

			// generate KeySets
			int32_t genSeed;
			for (size_t ksI = 0; ksI < ksPerN; ++ksI)
			{
				if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
				keySetStorage[ksI] = generateKeySet (nI, &genSeed, usedKeySetShape);
			}

			// for all number of searches
			for (size_t searchesI = startSearches; searchesI <= endSearches; searchesI += stepSearches)
			{
				int32_t searchSeed = 1;
				if (getRandomSeed (&searchSeed) != &searchSeed) printExit ("Seed Parsing Error or feed me more seeds");

				// for all KeySets in the storage
				for (size_t ksI = 0; ksI < ksPerN; ++ksI)
				{
					// measure
					size_t res = benchmarkSearchTimeMeasure (keySetStorage[ksI], searchesI, searchSeed, option, repeats,
										 numberOfRepeats);
					// save partial result to summarize it with median
					partialResult[((searchesI - startSearches) / stepSearches) * ksPerN + ksI] = res;
				}
			}
			// sort partialResult and take median as final result
			for (size_t searchesI = 0; searchesI < searchesCount; ++searchesI)
			{
				qsort (&partialResult[searchesI * ksPerN], ksPerN, sizeof (size_t), cmpInteger);
				results[((nI - startN) / stepN) * searchesCount + searchesI] =
					partialResult[searchesI * ksPerN + (ksPerN / 2)];
			}

			// free ks
			for (size_t ksI = 0; ksI < ksPerN; ++ksI)
			{
				ksDel (keySetStorage[ksI]);
			}
		}

		// write out
		FILE * out = openOutFileWithRPartitePostfix (outFileName, shapeI);
		if (!out)
		{
			printExit ("open out file");
		}
		// print header
		fprintf (out, "n");
		for (size_t searchesI = startSearches; searchesI <= endSearches; searchesI += stepSearches)
		{
			fprintf (out, ";search_%zu", searchesI);
		}
		fprintf (out, "\n");
		// print data
		for (size_t nI = startN; nI <= endN; nI += stepN)
		{
			fprintf (out, "%zu", nI);
			for (size_t searchesI = startSearches; searchesI <= endSearches; searchesI += stepSearches)
			{
				fprintf (out, ";%zu",
					 results[((nI - startN) / stepN) * searchesCount + ((searchesI - startSearches) / stepSearches)]);
			}
			fprintf (out, "\n");
		}

		fclose (out);
	}
	printf ("\n");

	elektraFree (repeats);
	elektraFree (partialResult);
	elektraFree (keySetStorage);
	elektraFree (keySetShapes);
	elektraFree (results);
}

void benchmarkOPMPHMSearchTime (char * name)
{
	benchmarkSearchTime (name, "benchmark_opmphm_search_time", KDB_O_OPMPHM | KDB_O_NOCASCADING);
}

/**
 * END ==================================================== OPMPHM Search Time ========================================================= END
 */

/**
 * START ================================================= Binary search Time ======================================================== START
 *
 * This benchmark measures the time of the binary search.
 * Uses all KeySet shapes except 6, for one n (KeySet size) ksPerN KeySets are used.
 * The keyset shape 6 is excluded, because previous evaluation had show that the results with that keyset shape
 * where unusable, due to the unnatural long key names.
 * Each measurement done with one KeySet is repeated numberOfRepeats time and summarized with the median.
 * For one n (KeySet size) the ksPerN results are also summarized with the median.
 * The results are written out in the following format:
 *
 * n;search_1;search_2;...;search_(numberOfSearches)
 *
 * The number of needed seeds for this benchmarks is: (numberOfShapes - 1) * nCount * ksPerN * (1  + searchesCount )
 */

static void benchmarkBinarySearchTime (char * name)
{
	benchmarkSearchTime (name, "benchmark_binary_search_time", KDB_O_NOCASCADING);
}

/**
 * END =================================================== Binary search Time ========================================================== END
 */

/**
 * START ================================================= hsearch Build Time ======================================================== START
 *
 * This benchmark measures the time of the hsearch build.
 * For one n (KeySet size) ksPerN KeySets are used, with different loads.
 * This benchmark has a 10 strike policy, when 10 time the measured time is over 10000 the next KeySet shape is handled.
 * The results are written out in the following format:
 *
 * n;ks;load;time
 *
 * The number of needed seeds for this benchmarks is: (numberOfShapes - 1) * nCount * ksPerN
 */

// clang-format off
// format bug
#ifdef HAVE_HSEARCHR
// clang-format on

/**
 * @brief Measures the hsearch build numberOfRepeats time and returns median
 *
 * @param ks the KeySet
 * @param nI the KeySet size
 * @param load the load
 * @param repeats array to store repeated measurements
 * @param numberOfRepeats fields in repeats
 *
 * @retval median time
 */
static size_t benchmarkHsearchBuildTimeMeasure (KeySet * ks, size_t nI, double load, size_t * repeats, size_t numberOfRepeats)
{
	for (size_t repeatsI = 0; repeatsI < numberOfRepeats; ++repeatsI)
	{
		// preparation for measurement
		struct timeval start;
		struct timeval end;
		Key * key;

		elektraCursor it;
		ssize_t ksSize = ksGetSize (ks);

		ENTRY e;
		ENTRY * ep;
		// fresh htab
		struct hsearch_data * htab = elektraCalloc (sizeof (struct hsearch_data));
		if (!htab)
		{
			printExit ("calloc");
		}

		// START MEASUREMENT
		__asm__ ("");
		gettimeofday (&start, 0);
		__asm__ ("");

		if (!hcreate_r (nI / load, htab))
		{
			printExit ("hcreate_r");
		}

		for (it = 0; it < ksSize; ++it)
		{
			key = ksAtCursor (ks, it);

			e.key = (char *) keyName (key);
			e.data = key;
			if (!hsearch_r (e, ENTER, &ep, htab))
			{
				printExit ("hsearch_r");
			}
		}

		__asm__ ("");
		gettimeofday (&end, 0);
		__asm__ ("");
		// END MEASUREMENT

		// save result
		repeats[repeatsI] = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);

		// sanity check
		for (it = 0; it < ksSize; ++it)
		{
			key = ksAtCursor (ks, it);
			e.key = (char *) keyName (key);
			if (!hsearch_r (e, FIND, &ep, htab))
			{
				printExit ("Sanity Check Failed: hsearch can not find element");
			}
		}

		hdestroy_r (htab);
		elektraFree (htab);
	}
	// sort repeats
	qsort (repeats, numberOfRepeats, sizeof (size_t), cmpInteger);
	return repeats[numberOfRepeats / 2]; // take median
}

void benchmarkHsearchBuildTime (char * name)
{
	const size_t startN = 50;
	const size_t stepN = 500;
	const size_t endN = 20000;
	const size_t ksPerN = 5;
	const size_t numberOfRepeats = 7;
	const size_t maxStrikes = 10;
	const size_t strikeLimit = 10000;
	const size_t numberOfLoads = 4;
	double * loads = malloc (sizeof (double) * numberOfLoads);
	if (!loads)
	{
		printExit ("malloc");
	}
	loads[0] = 1;
	loads[1] = 0.75;
	loads[2] = 0.5;
	loads[3] = 0.25;

	// check config
	if (startN >= endN || startN == 0)
	{
		printExit ("startN >= endN || startN == 0");
	}
	if (numberOfRepeats % 2 == 0)
	{
		printExit ("numberOfRepeats is even");
	}
	if (ksPerN % 2 == 0)
	{
		printExit ("ksPerN is even");
	}

	// calculate counts
	size_t nCount = 0;
	for (size_t nI = startN; nI <= endN; nI += stepN)
	{
		++nCount;
	}

	// memory allocation and initialization
	// init results
	size_t * results = elektraMalloc (nCount * ksPerN * numberOfLoads * sizeof (size_t));
	if (!results)
	{
		printExit ("malloc");
	}
	// init repeats
	size_t * repeats = elektraMalloc (numberOfRepeats * sizeof (size_t));
	if (!repeats)
	{
		printExit ("malloc");
	}
	// init KeySetStorage
	KeySet ** keySetStorage = elektraMalloc (ksPerN * sizeof (KeySet *));
	if (!keySetStorage)
	{
		printExit ("malloc");
	}

	// get KeySet shapes
	KeySetShape * keySetShapes = getKeySetShapes ();

	printf ("Run Benchmark %s:\n", name);

	// for all KeySet shapes except 6
	for (size_t shapeI = 0; shapeI < numberOfShapes; ++shapeI)
	{
		if (shapeI == 6)
		{
			continue;
		}
		KeySetShape * usedKeySetShape = &keySetShapes[shapeI];
		size_t strikes = 0;

		// for all Ns
		for (size_t nI = startN; nI <= endN; nI += stepN)
		{
			printf ("now at: shape = %zu/%zu n = %zu/%zu\r", shapeI + 1, numberOfShapes, nI, endN);
			fflush (stdout);

			// generate KeySets
			int32_t genSeed;
			for (size_t ksI = 0; ksI < ksPerN; ++ksI)
			{
				if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
				keySetStorage[ksI] = generateKeySet (nI, &genSeed, usedKeySetShape);
			}

			// for all loads
			for (size_t loadI = 0; loadI < numberOfLoads; ++loadI)
			{
				// for all KeySets in the storage
				for (size_t ksI = 0; ksI < ksPerN; ++ksI)
				{
					// measure
					size_t res = benchmarkHsearchBuildTimeMeasure (keySetStorage[ksI], nI, loads[loadI], repeats,
										       numberOfRepeats);
					// strike policy
					if (res > strikeLimit)
					{
						++strikes;
						if (strikes >= maxStrikes)
						{
							ksI = ksPerN;
							loadI = numberOfLoads;
							nI = endN + 1;
							printf ("shape %zu is out!\n", shapeI);
						}
					}
					else
					{
						strikes = 0;
						// save only non strike values
						results[((nI - startN) / stepN) * ksPerN * numberOfLoads + ksI * numberOfLoads + loadI] =
							res;
					}
				}
			}

			// free ks
			for (size_t ksI = 0; ksI < ksPerN; ++ksI)
			{
				ksDel (keySetStorage[ksI]);
			}
		}

		// write out
		FILE * out = openOutFileWithRPartitePostfix ("benchmark_hsearch_build_time", shapeI);
		if (!out)
		{
			printExit ("open out file");
		}
		// print header
		fprintf (out, "n;ks;load;time\n");
		// print data
		for (size_t nI = startN; nI <= endN; nI += stepN)
		{
			for (size_t ksI = 0; ksI < ksPerN; ++ksI)
			{
				for (size_t loadI = 0; loadI < numberOfLoads; ++loadI)
				{
					fprintf (out, "%zu;%zu;%f;%zu\n", nI, ksI, loads[loadI],
						 results[((nI - startN) / stepN) * ksPerN * numberOfLoads + ksI * numberOfLoads + loadI]);
				}
			}
		}

		fclose (out);
	}
	printf ("\n");

	elektraFree (repeats);
	elektraFree (keySetStorage);
	elektraFree (loads);
	elektraFree (keySetShapes);
	elektraFree (results);
}

#endif

/**
 * END =================================================== hsearch Build Time ========================================================== END
 */

/**
 * START =================================================== Prediction Time ========================================================= START
 *
 * This benchmark measures the time from `numberOfSequences` lookup sequences, with the modified branch predictor and the binary search.
 * All KeySet shapes except 6 where used.
 * The keyset shape 6 is excluded, because previous evaluation had show that the results with that keyset shape
 * where unusable, due to the unnatural long key names.
 * For all `n` `patternsPerN` lookup patterns are created. With a length of `numberOfSequences`.
 * The KeySet shapes rotate through the lookup patterns.
 * Two entries of the pattern entries use one seed (31 bit), this works because max n  is 10000.
 * log_2 (opmphmPredictorWorthOpmphm(10000)) * 2) < 15 bit
 * log_2 (15000) * 2) < 15 bit
 *
 * n;predictiontime;binarysearchtime
 *
 * The number of needed seeds for this benchmarks is: nCount * patternsPerN * ( numberOfSequences/2 + 1 + numberOfSequences )
 */

static void benchmarkPredictionTime (char * name)
{
	const size_t numberOfRepeats = 5;
	const size_t numberOfSequences = 66;
	const size_t patternsPerN = 999;
	// create the n array
	const size_t nCount = 35;
	size_t * n = elektraMalloc (nCount * sizeof (size_t));
	if (!n)
	{
		printExit ("malloc");
	}
	size_t controlCount = 0;
	for (size_t i = 100; i < 1000; i += 100)
	{
		n[controlCount] = i;
		++controlCount;
	}
	for (size_t i = 1000; i < 5000; i += 200)
	{
		n[controlCount] = i;
		++controlCount;
	}
	for (size_t i = 5000; i <= 10000; i += 1000)
	{
		n[controlCount] = i;
		++controlCount;
	}

	// check config
	if (controlCount != nCount)
	{
		printExit ("controlCount != nCount");
	}
	if (numberOfRepeats % 2 == 0)
	{
		printExit ("numberOfRepeats is even");
	}
	if (patternsPerN % (numberOfShapes - 1) == 0)
	{
		printExit ("not all shapes used equally");
	}

	// memory allocation and initialization
	// init results
	size_t * results = elektraMalloc (nCount * patternsPerN * 2 * sizeof (size_t)); // 2 prediction and binary search
	if (!results)
	{
		printExit ("malloc");
	}
	// init repeats
	size_t * repeats = elektraMalloc (numberOfRepeats * sizeof (size_t));
	if (!repeats)
	{
		printExit ("malloc");
	}
	// init seeds
	int32_t * seeds = elektraMalloc (numberOfSequences * sizeof (int32_t));
	if (!seeds)
	{
		printExit ("malloc");
	}
	// init pattern
	size_t * pattern = elektraMalloc (numberOfSequences * sizeof (size_t));
	if (!pattern)
	{
		printExit ("malloc");
	}

	// get KeySet shapes
	KeySetShape * keySetShapes = getKeySetShapes ();

	printf ("Run Benchmark %s:\n", name);

	// for all n
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		// for all pattern per n
		for (size_t pI = 0; pI < patternsPerN; ++pI)
		{
			printf ("now at: n = %zu/%zu pattern = %zu/%zu \r", nI + 1, nCount, pI + 1, patternsPerN);
			fflush (stdout);
			// create pattern, always two entries with one seed
			for (size_t s = 0; s < numberOfSequences; s += 2)
			{
				int32_t genSeed = 0;
				if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
				// 15 bit each of the 31 bit seed
				size_t sequnceLength1 = (genSeed >> 15) & 0x7FFF;
				size_t sequnceLength0 = genSeed & 0x7FFF;
				sequnceLength1 = sequnceLength1 % (opmphmPredictorWorthOpmphm (n[nI]) * 2 - 1);
				sequnceLength0 = sequnceLength0 % (opmphmPredictorWorthOpmphm (n[nI]) * 2 - 1);
				pattern[s + 1] = sequnceLength1 + 1;
				pattern[s] = sequnceLength0 + 1;
			}
			// rotate through all KeySet shapes, except 6
			size_t shapeI = patternsPerN % (numberOfShapes - 1);
			if (shapeI == 6)
			{
				++shapeI;
			}
			KeySetShape * usedKeySetShape = &keySetShapes[shapeI];

			// generate KeySet
			int32_t genSeed = 0;
			if (getRandomSeed (&genSeed) != &genSeed) printExit ("Seed Parsing Error or feed me more seeds");
			KeySet * ks = generateKeySet (n[nI], &genSeed, usedKeySetShape);

			// get seeds for OPMPHM
			for (size_t s = 0; s < numberOfSequences; ++s)
			{
				if (getRandomSeed (&seeds[s]) != &seeds[s]) printExit ("Seed Parsing Error or feed me more seeds");
			}

			size_t resultPredition;
			size_t resultBinarySearch;

			// benchmark prediction

			// repeat measurement numberOfRepeats time
			for (size_t repeatsI = 0; repeatsI < numberOfRepeats; ++repeatsI)
			{
				// preparation for measurement
				struct timeval start;
				struct timeval end;
				Key * keyFound;

				// START MEASUREMENT
				__asm__ ("");
				gettimeofday (&start, 0);
				__asm__ ("");

				// for all sequences
				for (size_t s = 0; s < numberOfSequences; ++s)
				{
					// seed used for key to lookup and OPMPHM
					int32_t searchHashSeed = seeds[s];
					// set seed to return by elektraRandGetInitSeed () in the lookup, in case of hashing
					elektraRandBenchmarkInitSeed = searchHashSeed;

					// do the lookups
					for (size_t lookups = 0; lookups < pattern[s]; ++lookups)
					{
						keyFound =
							ksLookup (ks, ks->data->array[searchHashSeed % ks->data->size], KDB_O_NOCASCADING);
						if (!keyFound || keyFound != ks->data->array[searchHashSeed % ks->data->size])
						{
							printExit ("Sanity Check Failed: found wrong Key");
						}
						elektraRand (&searchHashSeed);
					}
					if (!ks->data->opmphmPredictor)
					{
						printExit ("Sanity Check Failed: no predictor used");
					}
					// simulate data change
					ks->data->isOpmphmInvalid = true;
					if (ks->data->opmphm) opmphmClear (ks->data->opmphm);
				}

				__asm__ ("");
				gettimeofday (&end, 0);
				__asm__ ("");
				// END MEASUREMENT

				// save result
				repeats[repeatsI] = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
			}
			// sort repeats
			qsort (repeats, numberOfRepeats, sizeof (size_t), cmpInteger);
			resultPredition = repeats[numberOfRepeats / 2];

			// benchmark binary search

			// repeat measurement numberOfRepeats time
			for (size_t repeatsI = 0; repeatsI < numberOfRepeats; ++repeatsI)
			{
				// preparation for measurement
				struct timeval start;
				struct timeval end;
				Key * keyFound;

				// START MEASUREMENT
				__asm__ ("");
				gettimeofday (&start, 0);
				__asm__ ("");

				// for all sequences
				for (size_t s = 0; s < numberOfSequences; ++s)
				{
					// seed used for key to lookup and OPMPHM
					int32_t searchHashSeed = seeds[s];

					// do the lookups
					for (size_t lookups = 0; lookups < pattern[s]; ++lookups)
					{
						keyFound = ksLookup (ks, ks->data->array[searchHashSeed % ks->data->size],
								     KDB_O_NOCASCADING | KDB_O_BINSEARCH);
						if (!keyFound || keyFound != ks->data->array[searchHashSeed % ks->data->size])
						{
							printExit ("Sanity Check Failed: found wrong Key");
						}
						elektraRand (&searchHashSeed);
					}
				}

				__asm__ ("");
				gettimeofday (&end, 0);
				__asm__ ("");
				// END MEASUREMENT

				// save result
				repeats[repeatsI] = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
			}
			// sort repeats
			qsort (repeats, numberOfRepeats, sizeof (size_t), cmpInteger);
			resultBinarySearch = repeats[numberOfRepeats / 2];

			results[nI * patternsPerN * 2 + pI * 2] = resultPredition;
			results[nI * patternsPerN * 2 + pI * 2 + 1] = resultBinarySearch;

			ksDel (ks);
		}
	}
	printf ("\n");
	// write out
	FILE * out = openOutFileWithRPartitePostfix ("benchmark_prediction_time", opmphmPredictorHistoryMask >> 4); // shift 16 to 8 bit
	if (!out)
	{
		printExit ("open out file");
	}
	// print header
	fprintf (out, "n;predictiontime;binarysearchtime\n");
	for (size_t nI = 0; nI < nCount; ++nI)
	{
		for (size_t pI = 0; pI < patternsPerN; ++pI)
		{
			size_t predictiontime = results[nI * patternsPerN * 2 + pI * 2];
			size_t binarysearchtime = results[nI * patternsPerN * 2 + pI * 2 + 1];
			fprintf (out, "%zu;%zu;%zu\n", n[nI], predictiontime, binarysearchtime);
		}
	}
	fclose (out);

	elektraFree (n);
	elektraFree (keySetShapes);
	elektraFree (results);
	elektraFree (repeats);
	elektraFree (pattern);
	elektraFree (seeds);
}

/**
 * END ===================================================== Prediction Time =========================================================== END
 */

/**
 * START ================================================= Prints all KeySetShapes =================================================== START
 */

static void benchmarkPrintAllKeySetShapes (char * name)
{
	printf ("%s\n", name);
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
			printf (" ======================= shapeId %zu =======================\n\n", shapeId);
			Key * key;
			elektraCursor it;
			ssize_t ksSize = ksGetSize (ks);

			for (it = 0; it < ksSize; ++it)
			{
				key = ksAtCursor (ks, it);
				printf ("%s\n", keyName (key));
			}

			printf ("\n ======================== size %zd ========================\n\n", ksGetSize (ks));
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
	size_t benchmarksCount = 9;
#ifdef HAVE_HSEARCHR
	// hsearchbuildtime
	++benchmarksCount;
#endif
	Benchmark * benchmarks = elektraMalloc (benchmarksCount * sizeof (Benchmark));
	if (!benchmarks)
	{
		printExit ("malloc");
	}

	// hashfunctiontime
	char * benchmarkNameHashFunctionTime = "hashfunctiontime";
	benchmarks[0].name = benchmarkNameHashFunctionTime;
	benchmarks[0].benchmarkF = benchmarkHashFunctionTime;
	benchmarks[0].numberOfSeedsNeeded = 32;
	// mapping
	char * benchmarkNameMapping = "mapping";
	benchmarks[1].name = benchmarkNameMapping;
	benchmarks[1].benchmarkF = benchmarkMapping;
	benchmarks[1].numberOfSeedsNeeded = 12400;
	// mapping_opt
	char * benchmarkNameMappingOpt = "mapping_opt";
	benchmarks[2].name = benchmarkNameMappingOpt;
	benchmarks[2].benchmarkF = benchmarkMappingOpt;
	benchmarks[2].numberOfSeedsNeeded = 93920;
	// mapping_allseeds
	char * benchmarkNameMappingAllSeeds = "mapping_allseeds";
	benchmarks[3].name = benchmarkNameMappingAllSeeds;
	benchmarks[3].benchmarkF = benchmarkMappingAllSeeds;
	benchmarks[3].numberOfSeedsNeeded = 7;
	// printallkeysetshapes
	char * benchmarkNamePrintAllKeySetShapes = "printallkeysetshapes";
	benchmarks[4].name = benchmarkNamePrintAllKeySetShapes;
	benchmarks[4].benchmarkF = benchmarkPrintAllKeySetShapes;
	benchmarks[4].numberOfSeedsNeeded = 0;
	// opmphmbuildtime
	char * benchmarkNameOpmphmBuildTime = "opmphmbuildtime";
	benchmarks[5].name = benchmarkNameOpmphmBuildTime;
	benchmarks[5].benchmarkF = benchmarkOPMPHMBuildTime;
	benchmarks[5].numberOfSeedsNeeded = 1757;
	// opmphmsearchtime
	char * benchmarkNameOpmphmSearchTime = "opmphmsearchtime";
	benchmarks[6].name = benchmarkNameOpmphmSearchTime;
	benchmarks[6].benchmarkF = benchmarkOPMPHMSearchTime;
	benchmarks[6].numberOfSeedsNeeded = 54600;
	// binarysearchtime
	char * benchmarkNameBinarySearchTime = "binarysearchtime";
	benchmarks[7].name = benchmarkNameBinarySearchTime;
	benchmarks[7].benchmarkF = benchmarkBinarySearchTime;
	benchmarks[7].numberOfSeedsNeeded = 54600;
	// predictiontime
	char * benchmarkNamePredictionTime = "predictiontime";
	benchmarks[8].name = benchmarkNamePredictionTime;
	benchmarks[8].benchmarkF = benchmarkPredictionTime;
	benchmarks[8].numberOfSeedsNeeded = 3496500;
#ifdef HAVE_HSEARCHR
	// hsearchbuildtime
	char * benchmarkNameHsearchBuildTime = "hsearchbuildtime";
	benchmarks[benchmarksCount - 1].name = benchmarkNameHsearchBuildTime;
	benchmarks[benchmarksCount - 1].benchmarkF = benchmarkHsearchBuildTime;
	benchmarks[benchmarksCount - 1].numberOfSeedsNeeded = 1400;
#endif

	// run benchmark
	if (argc == 1)
	{
		fprintf (stderr, "Usage: cat <fileWithSeeds> | %s <benchmark>\n", argv[0]);
		fprintf (stderr, "\nUse the generate-seeds script to generate <fileWithSeeds>, number of seeds according to:\n\n");
		fprintf (stderr, "%-20s %10s\n", "<benchmark>", "seeds");
		for (size_t i = 0; i < benchmarksCount; ++i)
		{
			fprintf (stderr, "%-20s %10zu\n", benchmarks[i].name, benchmarks[i].numberOfSeedsNeeded);
		}
		elektraFree (benchmarks);
		return EXIT_FAILURE;
	}
	for (size_t i = 0; i < benchmarksCount; ++i)
	{
		if (!strncmp (benchmarks[i].name, argv[1], strlen (argv[1])))
		{
			benchmarks[i].benchmarkF (benchmarks[i].name);
			elektraFree (benchmarks);
			return EXIT_SUCCESS;
		}
	}
	fprintf (stderr, "Error: %s is not a benchmark\n", argv[1]);
	fprintf (stderr, "Available benchmarks:\n");
	for (size_t i = 0; i < benchmarksCount; ++i)
	{
		fprintf (stderr, "* %s\n", benchmarks[i].name);
	}
	elektraFree (benchmarks);
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
	// prevent empty lines
	if (strlen (data) == 0)
	{
		return NULL;
	}
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
static FILE * openOutFileWithRPartitePostfix (const char * name, uint8_t r)
{
	const char * const format = "%u.csv";
	char formatData[strlen (name) + strlen (format) + 1];
	char filename[strlen (name) + strlen (format) + 1];
	strcpy (formatData, name);
	strcpy (&formatData[strlen (name)], format);
	sprintf (filename, formatData, r);
	FILE * out = fopen (filename, "w");
	if (!out)
	{
		return NULL;
	}
	return out;
}

static const char * getString (void * data)
{
	return keyName ((Key *) data);
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
 * @brief comparison between integers suitable as qsort callback.
 *
 * @param a first integer
 * @param b second integer
 *
 */
static int cmpInteger (const void * a, const void * b)
{
	if (*(size_t *) a < *(size_t *) b)
	{
		return -1;
	}
	else if (*(size_t *) a > *(size_t *) b)
	{
		return 1;
	}
	else
	{
		return 0;
	}
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
 * modules, level 1 keys same, one level 2 key stores the modules. Like system:/elektra.
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
		// common start, simulates elektra in system:/elektra
		ret->subKeys = 1;
		ret->label = 0;
	}
	else if (level == 2)
	{
		// common name, simulates modules in system:/elektra/modules
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
	out[shapeCount].shapeDel = shapeCommonStartEndDel;
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
