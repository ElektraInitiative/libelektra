#include <key.hpp>
#include <keyset.hpp>
#include <kdb.hpp>

#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <string>

#include <unordered_map>

extern "C" {
  #include "opmph_benchmarks.h"
}


/*
 * DATA TYPES
 */

typedef std::vector<std::pair<std::string, kdb::Key>> Data;

/* INTERFACE FOR THE BENCHMARK TYPES */
class Benchmark
{
public:
	virtual int run (int n,int k,int r, Data data, std::vector<int> * values) = 0;
};

/* BENCHMARK TYPE INSERT */
class Insert: public Benchmark
{
public:
	/* The actual benchmark procedure for the insert, executed for each
	 * setting.
	 */
	int run (int n,int k,int r, Data data, std::vector<int> * values)
	{
		struct timeval start;
		struct timeval end;

		gettimeofday (&start, 0);
		//measure

		std::unordered_map<std::string,kdb::Key> map;

		//suggest bucket count
		map.rehash (k);
		//not optimal during measurement, but needed to detect rehashes
		unsigned int bc = map.bucket_count();

		for (int i = 0;i < n;++i)
			map.insert (data[i]);

		gettimeofday (&end, 0);
		(*values)[r] = int ((end.tv_sec - start.tv_sec) * 1000000 +
						(end.tv_usec - start.tv_usec));

		if (bc != map.bucket_count())
		{
			std::cout << "rehashing occurred" << std::endl;
			std::exit (1);
		}

		return int (bc);
	}
};

/* BENCHMARK TYPE SEARCH */

//needed helper for search
int searchNext (std::vector<int> keys_searched_for);
int max (std::vector<int> values);

class Search: public Benchmark
{
public:
	/* The actual benchmark procedure for the search, executed for each
	 * setting.
	 */
	int run (int n,int k,int r, Data data, std::vector<int> * values)
	{
		struct timeval start;
		struct timeval end;
		//this vector saves all the times to search each key
		std::vector<int> keys_searched_for (n);

		for (int i = 0;i < n;++i) keys_searched_for[i] = -1;

		//Create map
		std::unordered_map<std::string,kdb::Key> map;
		//suggest bucket count
		map.rehash (k);
		unsigned int bc = map.bucket_count();

		for (int i = 0;i < n;++i)
			map.insert (data[i]);

		// search for each key randomly and save the time
		for(int i = 0;i < n;++i)
		{
			int search_for = searchNext (keys_searched_for);
			std::string lookfor = data[search_for].first;

			std::unordered_map<std::string,kdb::Key>::const_iterator got;

			gettimeofday (&start, 0);
			//measure

			got = map.find (lookfor);

			gettimeofday (&end, 0);

			int t = int ((end.tv_sec - start.tv_sec) * 1000000 +
						(end.tv_usec - start.tv_usec));

			keys_searched_for[search_for] = t;

			if (got == map.end())
			{
				std::cout << "not found while search" << std::endl;
				std::exit (1);
			}
			else
			{
				if (lookfor.compare (got->second.getName()) != 0)
				{
					std::cout << "found wrong Key while search" << std::endl;
					std::exit (1);
				}
			}
		}

		// take the maximum for this run
		(*values)[r] = max (keys_searched_for);

		if (bc != map.bucket_count())
		{
			std::cout << "rehashing occurred" << std::endl;
			std::exit (1);
		}

		return int (bc);
	}
};


/*
 * FUNCTIONS
 */

void runBenchmark (Benchmark * bench);

//Data helpers
Data prepareData (kdb::KeySet ks);

//other helper
int median (std::vector<int> values);


//TODO KURT print machine name + kernel bla
int main (int argc, char**argv)
{
	bool b_insert = true;
	bool b_search = true;

	if (argc == 2)
	{
		if (std::string("insert").compare(argv[1]) == 0)
			b_search = false;
		if (std::string("search").compare(argv[1]) == 0)
			b_insert = false;
	}

	if (b_insert)
	{
		Insert b;
		runBenchmark(&b);
	}

	if (b_search)
	{
		Search b;
		runBenchmark(&b);
	}

    return 0;
}

/* provides the skeleton for the benchmark runs.
 */
void runBenchmark (Benchmark * bench)
{
	std::cout << "KeySet size;suggested bucket size;bucket size;time" << std::endl;
	for (int n=MIN_KEYSET_SIZE;n <= MAX_KEYSET_SIZE;n+=STEP_KEYSET_SIZE)
	{
		/* k is the bucket count and goes from n to 2n.
		 * The step count is calculated with this function
		 * roundup((rounddown(n/min)-rounddown(n/max))/2)
		 */
		int temp1 = int (std::floor (n/BUCKET_MIN_STEP_COUNT));
		int temp2 = int (std::floor (n/BUCKET_MAX_STEP_COUNT));
		int bucket_step = int (std::ceil ( (temp1-temp2) / 2.0 ));

		for (int k = n; k <= n*2;k+=bucket_step)
		{
			std::cout << n << "," << k << ",";
			std::vector<int> times (REPEATS);
			//TODO KURT fix generateKeySet memory leakage
			kdb::KeySet ks (generateKeySet (n));
			int final_bucket_count;

			for (int r = 0;r < REPEATS;++r)
			{
				Data data = prepareData (ks);

				//call for worker
				final_bucket_count = bench->run (n, k, r, data, &times);

			}

			std::cout << final_bucket_count << ",";

			std::cout << median (times) << std::endl;

		}
	}
}

/* This helper generates a vector of pairs for the hash map, for having
 * a fresh data to insert on each run.
 */
Data prepareData (kdb::KeySet ks)
{
	Data data (ks.size());

	ks.rewind();
	int i = 0;
	while (ks.next())
	{
		std::pair<std::string,kdb::Key> insert;
		insert.first = std::string(ks.current().getName());
		insert.second = ks.current();
		data[i] = insert;
		++i;
	}
	return data;
}

/* Returns an random and free index for the search procedure,
 * where for each key a search is made.
 */
int searchNext (std::vector<int> keys_searched_for)
{
	int free_fields = 0;
	//get free fields to generate the right random number
	for (unsigned int i = 0;i < keys_searched_for.size();++i)
	{
		if (keys_searched_for[i] == -1) ++free_fields;
	}
	int next = genRand() % free_fields;
	free_fields = 0;
	//map the generated random number to the free fields
	for (unsigned int i = 0;i < keys_searched_for.size();++i)
	{
		if (keys_searched_for[i] == -1)
		{
			if (free_fields == next) return i;
			++free_fields;
		}
	}
	//should not happen
	return 0;
}

int max (std::vector<int> values)
{
	int max = values[0];
	for (unsigned int i = 1;i < values.size();++i)
	{
		max = (values[i] > max) ? values[i] : max;
	}
	return max;
}

int median (std::vector<int> values)
{
	std::sort (values.begin(), values.end());
	return values[std::floor(values.size()/2)];
}
