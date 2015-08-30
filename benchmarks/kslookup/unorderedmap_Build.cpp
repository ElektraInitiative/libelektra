#include "unorderedmap_Build.hpp"


unorderedmap_Build::unorderedmap_Build (bool m)
{
	mode = m;
}

int unorderedmap_Build::run (int n,int k,int r, Data * data, int * values)
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
	values[r] = int ((end.tv_sec - start.tv_sec) * 1000000 +
					(end.tv_usec - start.tv_usec));

	if (bc != map.bucket_count())
	{
		std::cerr << "rehashing occurred" << std::endl;
		return -1;
	}

	return int (bc);
}

bool unorderedmap_Build::getMode ()
{
	return mode;
}
