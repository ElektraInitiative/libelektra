#define _GNU_SOURCE

#include <pthread.h>

// every resolver should use the same mutex
#if defined(__APPLE__) && defined(__MACH__)
pthread_mutex_t elektra_resolver_mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER;
#else
static pthread_mutex_t elektra_resolver_mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#endif

pthread_mutex_t *getElektraResolverMutex()
{
	return &elektra_resolver_mutex;
}
