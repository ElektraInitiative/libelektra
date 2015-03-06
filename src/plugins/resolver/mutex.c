#define _GNU_SOURCE

#include <pthread.h>

// every resolver should use the same mutex
#if defined(__APPLE__)
pthread_mutex_t elektra_resolver_mutex;
// special mutex for recursive mutex creation
pthread_mutex_t elektra_resolver_init_mutex = PTHREAD_MUTEX_INITIALIZER;
// marks if the recursive mutex has been initialized yet
int elektra_resolver_mutex_unitialized = 1;
#else
pthread_mutex_t elektra_resolver_mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#endif
