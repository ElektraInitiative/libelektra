#define _GNU_SOURCE

#include <pthread.h>

// every resolver should use the same mutex
static pthread_mutex_t elektra_resolver_mutex;
pthread_mutex_t * getResolverMutex()
{
	return &elektra_resolver_mutex;
}
