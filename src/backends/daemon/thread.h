/***************************************************************************
                   thread.h  -  Thread Managment
                        -------------------
    copyright            : (C) 2006 by Yannick Lecaillez
    email                : sizon5@gmail.com
 **************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

int threadCreate(int socketfd, void *(*start_routine)(void *));
int threadGetSocket(int threadHandle);
pthread_t threadGetId(int threadHandle);
void threadExit(void *pIntThreadHandle);
