/***************************************************************************
                   thread.c  -  Thread Managment
                           -------------------
    copyright            : (C) 2006 by Yannick Lecaillez
    email                : sizon5@gmail.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/* Subversion stuff

$Id$

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <pthread.h>

#include "thread.h"

#define THREADS_PREALLOC	5

typedef struct {
	pthread_t	thread;
	
	int		socketFd;
	int		handle;
} ThreadContext;

static ThreadContext **threads = NULL;
static int threadsArraySize = 0;
static pthread_mutex_t threadsMutex = PTHREAD_MUTEX_INITIALIZER;

static int threadListAdd(ThreadContext *thread);
static ThreadContext *threadListGet(int threadHandle);
static int threadListRemove(int threadHandle);

/*
 * Create a new ThreadContext and launch the thread
 * Launched thread must call threadExit(handle) at end.
 */
int threadCreate(int socketfd, void *(*start_routine)(void *))
{
	ThreadContext	*new;

	if ( (new = (ThreadContext *) malloc(sizeof(ThreadContext))) == NULL ) {
		return -1;
	}	
	memset(new, 0, sizeof(ThreadContext));
	new->socketFd = socketfd;

	if ( (new->handle = threadListAdd(new)) == -1 )
		return -1;
	
	if ( pthread_create(&new->thread, NULL, start_routine, (void *) &new->handle) ) {
		threadListRemove(new->handle);
		free(new);
		return -1;
	}
		
	return new->handle;
}

int threadGetSocket(int threadHandle)
{
	ThreadContext *thread;

	if ( (thread = threadListGet(threadHandle)) == NULL )
		return -1;

	return thread->socketFd;
}

pthread_t threadGetId(int threadHandle)
{
	ThreadContext *thread;

	if ( (thread = threadListGet(threadHandle)) == NULL )
		return -1;

	return thread->thread;
}

/*
 * Remove ThreadContext on thread exit
 * This functions shoudln't be called by hand !
 */
void threadExit(void *pIntThreadHandle)
{
	ThreadContext	*thread;
	int	threadHandle;

	threadHandle = *((int *) pIntThreadHandle);
	
	if ( (thread = threadListGet(threadHandle)) == NULL ) {
		fprintf(stderr, "threadExit(): Thread's handle %d doesn't exist.\n", threadHandle);
		return;
	}
	
	if ( threadListRemove(threadHandle) == -1 ) {
		fprintf(stderr, "threadExit(): Unable to remove thread's handle %d.\n", threadHandle);
		return;
	}

	close(thread->socketFd);
	free(thread);
	fprintf(stderr, "Thread %ld (handle=%d) freed.\n", pthread_self(), threadHandle);
}


/*
 * Keep a ThreadContext into an array
 * return an associated handle
 */
static int threadListAdd(ThreadContext *thread)
{
	ThreadContext **tmp;
	int threadHandle;
	int newSize;
	
	newSize = 0;
	if ( threadsArraySize == 0 ) {
		newSize = THREADS_PREALLOC;
	} else {
    		/* Search for a free space ... */
		pthread_mutex_lock(&threadsMutex);
		for(threadHandle = 0 ; threadHandle < threadsArraySize ; threadHandle++) {
			if ( threads[threadHandle] == NULL )
				break;
		}
		
		if ( threadHandle == threadsArraySize ) {
			/* No free space found, extend array */
 			newSize = threadsArraySize + THREADS_PREALLOC;
		}
		pthread_mutex_unlock(&threadsMutex);
	}
	
	if ( newSize ) {
		/* No free space found, extend array */
		pthread_mutex_lock(&threadsMutex);
		tmp = realloc(threads, sizeof(pthread_t *) * (newSize));
  		if ( tmp == NULL ) {
			pthread_mutex_unlock(&threadsMutex);
			return -1;
		}
		threads = tmp;
		threadHandle = threadsArraySize;
		threadsArraySize = newSize;
		pthread_mutex_unlock(&threadsMutex);
	}
	
	pthread_mutex_lock(&threadsMutex);
	threads[threadHandle] = thread;
	pthread_mutex_unlock(&threadsMutex);

	return threadHandle;
}


static int threadListRemove(int threadHandle)
{
	if ( threadHandle >= threadsArraySize )
		return -1;

	pthread_mutex_lock(&threadsMutex);
	threads[threadHandle] = NULL;
	pthread_mutex_unlock(&threadsMutex);

	return 0;
}

static ThreadContext *threadListGet(int threadHandle)
{
	ThreadContext *thread;
	
	if ( threadHandle >= threadsArraySize )
		return NULL;
	
	pthread_mutex_lock(&threadsMutex);
	thread = threads[threadHandle];
	pthread_mutex_unlock(&threadsMutex);

	return thread;
}
 	
