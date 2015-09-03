#include <iostream>
#include <cstdlib>
#include <fstream>
#include <sstream>

#include <modules.hpp>

#include "unorderedmap_Search.hpp"
#include "unorderedmap_Build.hpp"

extern "C" {
  #include "benchmark.h"
}

void runBenchmark (unorderedmap_Interface * bench);

//Data helpers
kdb::KeySet * readKeySet (int size, int version);
Data * prepareData (kdb::KeySet ks);


//TODO KURT print machine name + kernel bla ? uname ?
int main (int argc, char**argv)
{
	initRand ();
	bool b_build = true;
	bool b_search = true;

	if (argc == 2)
	{
		if (std::string("build").compare(argv[1]) == 0)
			b_search = false;
		if (std::string("search").compare(argv[1]) == 0)
			b_build = false;
	}

	if (b_build)
	{
		unorderedmap_Build b (true);
		runBenchmark(&b);
	}

	if (b_search)
	{
		unorderedmap_Search b (false);
		runBenchmark(&b);
	}

    return EXIT_SUCCESS;
}

/* provides the skeleton for the benchmark runs.
 */
void runBenchmark (unorderedmap_Interface * bench)
{
	for (int v=1;v <= KEYSET_VERSIONS;++v)
	{
		for (int n=MIN_KEYSET_SIZE;n <= MAX_KEYSET_SIZE;n+=STEP_KEYSET_SIZE)
		{
			kdb::KeySet * ks = readKeySet (n, v);

			std::ostringstream oss;
			oss << "unorderedmap_";
			if (bench->getMode ())
				oss << "b_";
			else
				oss << "s_";
			oss << n << "_" << v << ".bench";
			std::ofstream output (oss.str().c_str(), std::ios::binary);
			if (!output.is_open ())
			{
				delete ks;
				std::cerr << "output file could not be opened\n" << std::endl;
				std::exit (EXIT_FAILURE);
			}
			output << "KeySet size;suggested bucket size;bucket size;time" << std::endl;

			/* k is the bucket count and goes from n to 2n.
			 * The number of steps from n to 2n is calculated
			 * with a mapping form the interval starting with
			 * the MIN_KEYSET_SIZE  and ending at MAX_KEYSET_SIZE
			 * to the interval starting at MIN_BUCKET_STEP and ending
			 * at MAX_BUCKET_STEP.
			 * ex:
			 * MIN_KEYSET_SIZE= 10
			 * MAX_KEYSET_SIZE= 1000
			 * MIN_BUCKET_STEP = 3
			 * MAX_BUCKET_STEP = 30
			 * n = 10 has 3 steps
			 * n = 250 has 9 steps
			 * n = 500 has 16 steps
			 * n = 750 has 23 steps
			 * n = 1000 has 30 steps
			 */
			double diff_n = MAX_KEYSET_SIZE - MIN_KEYSET_SIZE;
			double diff_k = MAX_BUCKET_STEP - MIN_BUCKET_STEP;
			double ratio = diff_k/diff_n;
			int bucket_step_count = int ( (n*ratio) + MIN_BUCKET_STEP );
			if(bucket_step_count > MAX_BUCKET_STEP)
				bucket_step_count = MAX_BUCKET_STEP;
			int bucket_step = int ( n/bucket_step_count );
			if (bucket_step * bucket_step_count != n)
				++bucket_step;

			for (int k = n; k < n*2;k+=bucket_step)
			{
				output << n << ";" << k << ";";
				int times [REPEATS];
				int final_bucket_count;

				for (int r = 0;r < REPEATS;++r)
				{
					Data * data = prepareData (*ks);

					//call for worker
					final_bucket_count = bench->run (n, k, r, data, &times[0]);

					if (final_bucket_count < 0)
					{
						delete[] data;
						delete ks;
						output.close ();
						std::exit (EXIT_FAILURE);
					}

					delete[] data;

				}
				output << final_bucket_count << ";";
				output << median (times, REPEATS) << std::endl;
			}

			output.close ();
			delete ks;
		}
	}
}

kdb::KeySet * readKeySet (int size, int version)
{
	kdb::KeySet * out = new kdb::KeySet (size, KS_END);
	kdb::tools::Modules modules;
	kdb::tools::PluginPtr plugin = modules.load (EXPORT_PLUGIN);

	std::ostringstream oss;
	oss << size << "_" << version << ".edf";

	kdb::Key pkey;
	pkey.setString (oss.str ());

	plugin->get (*out, pkey);

	if (pkey.getMeta<const kdb::Key>("error"))
	{
		std::cerr << pkey.getMeta<std::string>("error/description") << std::endl;
		std::exit (EXIT_FAILURE);
	}

	return out;
}

Data * prepareData (kdb::KeySet ks)
{
	Data * out = new Data[ks.size ()];
	ks.rewind ();
	int i = 0;
	while (ks.next ())
	{
		out[i].first = ks.current().getName();
		out[i].second = ks.current ();
		++i;
	}
	return out;
}
