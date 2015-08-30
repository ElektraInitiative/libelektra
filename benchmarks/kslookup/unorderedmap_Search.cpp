#include "unorderedmap_Search.hpp"

unorderedmap_Search::unorderedmap_Search (bool m)
{
	mode = m;
}

int unorderedmap_Search::run (const int n,int k,int r, Data * data, int * values)
{
	struct timeval start;
	struct timeval end;
	//this vector saves all the times to search each key
	int * keys_searched_for = new int [n];

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
		int search_for = searchNext (keys_searched_for, n);
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
			delete[] keys_searched_for;
			return -1;
		}
		else
		{
			if (lookfor.compare (got->second.getName()) != 0)
			{
				std::cout << "found wrong Key while search" << std::endl;
				delete[] keys_searched_for;
				return -1;
			}
			if (std::string(GENDATA_KEY_VALUE).compare(got->second.getString()) != 0)
			{
				std::cout << "wrong Key value while search" << std::endl;
				delete[] keys_searched_for;
				return -1;
			}
		}
	}

	// take the maximum for this run
	values[r] = max (keys_searched_for, n);
	delete[] keys_searched_for;

	if (bc != map.bucket_count())
	{
		std::cout << "rehashing occurred" << std::endl;
		return -1;
	}

	return int (bc);
}

bool unorderedmap_Search::getMode ()
{
	return mode;
}
