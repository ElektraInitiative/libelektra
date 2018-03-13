/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map C benchmark.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// uncomment to use OPENMP and set USE_OPENMP in CMakeLists.txt
//~ #define USE_OPENMP

#ifdef USE_OPENMP
// set here you number of threads
#define NUMBEROFTHREADS 8
#else
#define NUMBEROFTHREADS 1
#endif

#define KDBRAND_BENCHMARK
#include "../src/libs/elektra/opmphm.c"
#include "../src/libs/elektra/rand.c"
#include "benchmarks.h"
#ifdef USE_OPENMP
#include <omp.h>
#endif
#include <sys/time.h>

int32_t elektraRandBenchmarkInitSeed;

// benchmarks helpers
static int32_t * getRandomSeed (int32_t * seed);
static FILE * openOutFileWithRPartitePostfix (const char * name, uint8_t r);
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
			fflush (stdout);
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

static void benchmarkMapping (void)
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
		printf ("now at: %lu/%lu\r", nI + 1, nCount);
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

	printf ("\nRun Benchmark:\n");

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
			printf ("now at: n = %lu/%lu c = %lu/%lu\r", nI + 1, nCount, cI + 1, cCount);
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
					init.getName = elektraGetString;
					init.data = (void **) (ks->array);
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
			fprintf (out, ";n_%luc_%f", n[nI], opmphmMinC (rUniPar) + c[cI]);
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
 * START ============================================== Mapping with Optimization ==================================================== START
 *
 * This benchmark counts the opmphmMapping (...)  invocations until success, for each KeySet size.
 * First the KeySets are build, for every KeySet size (n) there are numberOfShapes * keySetsPerShape KeySets.
 * Then the benchmarking for every KeySet size (n) takes place, with a fixed set of seeds for the opmphmMapping (...) invocations.
 * At the end the results are written out in the following format:
 *
 * trials;n_%lucr_%luc_%f;... (each n is unique)
 *
 * The number of needed seeds for this benchmarks is: nCount * numberOfShapes * keySetsPerShape (KeySets generation) + numberOfSeeds (tested
 * seeds)
 */

static void benchmarkMappingOpt (void)
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
		printf ("now at: %lu/%lu\r", nI + 1, nCount);
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

	printf ("\nRun Benchmark:\n");

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
		printf ("now at: n = %lu/%lu\r", nI + 1, nCount);
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
				init.getName = elektraGetString;
				init.data = (void **) (ks->array);
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
		fprintf (out, ";n_%lur_%uc_%f", n[nI], opmphmOptR (n[nI]), opmphmMinC (opmphmOptR (n[nI])) + opmphmOptC (n[nI]));
	}
	fprintf (out, "\n");
	// print data
	for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
	{
		fprintf (out, "%lu", mappingI + 1); // unshift, because 0 is not a result
		for (size_t nI = 0; nI < nCount; ++nI)
		{
			fprintf (out, ";%lu", results[nI * maxMappings + mappingI]);
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
 * trials;n_%lucr_%luc_%f;... (each n is unique)
 *
 * The number of needed seeds for this benchmarks is: nCount (KeySets generation)
 */

static void benchmarkMappingAllSeeds (void)
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

	printf ("\nRun Benchmark:\n");

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
			init.getName = elektraGetString;
			init.data = (void **) (ks->array);
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
					printf ("now at: n = %lu/%lu and seed %i from %i\r", nI + 1, nCount, seed, partIntervals[1]);
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
		fprintf (out, ";n_%lur_%uc_%f", n[nI], opmphmOptR (n[nI]), opmphmMinC (opmphmOptR (n[nI])) + opmphmOptC (n[nI]));
	}
	fprintf (out, "\n");
	// print data
	for (size_t mappingI = 0; mappingI < maxMappings; ++mappingI)
	{
		fprintf (out, "%lu", mappingI + 1); // unshift, because 0 is not a result
		for (size_t nI = 0; nI < nCount; ++nI)
		{
			fprintf (out, ";%lu", results[nI * maxMappings + mappingI]);
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
	const size_t benchmarksCount = 5;
	Benchmark * benchmarks = elektraMalloc (benchmarksCount * sizeof (Benchmark));
	if (!benchmarks)
	{
		printExit ("malloc");
	}
	// hashfunctiontime
	char * benchmarkNameHashFunctionTime = "hashfunctiontime";
	benchmarks[0].name = benchmarkNameHashFunctionTime;
	benchmarks[0].benchmarkF = benchmarkHashFunctionTime;
	// mapping
	char * benchmarkNameMapping = "mapping";
	benchmarks[1].name = benchmarkNameMapping;
	benchmarks[1].benchmarkF = benchmarkMapping;
	// mapping_opt
	char * benchmarkNameMappingOpt = "mapping_opt";
	benchmarks[2].name = benchmarkNameMappingOpt;
	benchmarks[2].benchmarkF = benchmarkMappingOpt;
	// mapping_allseeds
	char * benchmarkNameMappingAllSeeds = "mapping_allseeds";
	benchmarks[3].name = benchmarkNameMappingAllSeeds;
	benchmarks[3].benchmarkF = benchmarkMappingAllSeeds;
	// printallkeysetshapes
	char * benchmarkNamePrintAllKeySetShapes = "printallkeysetshapes";
	benchmarks[4].name = benchmarkNamePrintAllKeySetShapes;
	benchmarks[4].benchmarkF = benchmarkPrintAllKeySetShapes;

	// run benchmark
	if (argc == 1)
	{
		fprintf (stderr, "Usage: %s <benchmark>\n", argv[0]);
		fprintf (stderr, "Available benchmarks:\n");
		for (size_t i = 0; i < benchmarksCount; ++i)
		{
			fprintf (stderr, "* %s\n", benchmarks[i].name);
		}
		elektraFree (benchmarks);
		return EXIT_FAILURE;
	}
	for (size_t i = 0; i < benchmarksCount; ++i)
	{
		if (!strncmp (benchmarks[i].name, argv[1], strlen (argv[1])))
		{
			benchmarks[i].benchmarkF ();
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

static const char * elektraGetString (void * data)
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
