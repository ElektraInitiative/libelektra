#include <benchmarks.h>

#include <sys/time.h>

struct timeval start;

void timeInit(void)
{
	gettimeofday (&start,0);
}

void timePrint(char * msg)
{
	struct timeval measure;
	time_t diff;

	gettimeofday (&measure, 0);

	diff = (measure.tv_sec - start.tv_sec) * 1000000 + (measure.tv_usec - start.tv_usec);
	fprintf (stdout, "%20s: %20d Microseconds\n", msg, (int)diff);
	
	gettimeofday (&start,0);
}
