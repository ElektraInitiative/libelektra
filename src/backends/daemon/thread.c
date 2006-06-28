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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
	fprintf(stderr, "Thread %d launched.\n", new->thread);
		
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
 * Remove ThreadContext and exit the current thread
 * Must be called by the thread exiting only !
 */
int threadExit(int threadHandle, void *retVal)
{
	ThreadContext	*thread;
	
	if ( (thread = threadListGet(threadHandle)) == NULL )
		return -1;
	
	if ( threadListRemove(threadHandle) == -1 )
		return -1;

	close(thread->socketFd);
	fprintf(stderr, "Thread %d exited.\n", thread->thread);
	free(thread);
	pthread_exit(retVal);

	return 0;
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
 	
