#define _GNU_SOURCE

#include <pthread.h>

// every resolver should use the same mutex
pthread_mutex_t elektra_resolver_mutex =
	PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
