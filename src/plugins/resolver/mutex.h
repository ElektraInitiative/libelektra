#ifndef __RESOLVER_MUTEX_H
#define __RESOLVER_MUTEX_H

#ifdef ELEKTRA_LOCK_MUTEX

#include <pthread.h>

pthread_mutex_t *getResolverMutex(void);

#if defined(__APPLE__)
// returns a pointer to a mutex that is used to secure the initialization of the recursive mutex
pthread_mutex_t *getInitializerMutex(void);
// returns 1 if the recursive mutex has not been initialized yet
int getMutexUninitialized(void);
// indicate that the recursive mutex has been initialized
void setMutexInitialized(void); 
#endif

#endif
#endif