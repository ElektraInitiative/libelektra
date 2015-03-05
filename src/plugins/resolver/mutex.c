#define _GNU_SOURCE

#include <pthread.h>
#include "mutex.h"

// every resolver should use the same mutex
#if defined(__APPLE__)
static pthread_mutex_t elektra_resolver_mutex;
// special mutex for recursive mutex creation
static pthread_mutex_t initializer_mutex = PTHREAD_MUTEX_INITIALIZER;
// marks if the recursive mutex has been initialized yet
static int mutex_unitialized = 1;
#else
static pthread_mutex_t elektra_resolver_mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#endif

pthread_mutex_t *getResolverMutex()
{
	return &elektra_resolver_mutex;
}

#if defined(__APPLE__)

pthread_mutex_t *getInitializerMutex()
{
	return &initializer_mutex;
}

int getMutexUninitialized()
{
	return mutex_unitialized;
}

void setMutexInitialized()
{
	mutex_unitialized = 0;
}

#endif
