#include <benchmark.h>

unsigned int genRand (void)
{
	struct timeval time;
	gettimeofday (&time, 0);
	return (int) time.tv_sec * 1000000 + time.tv_usec;
}
