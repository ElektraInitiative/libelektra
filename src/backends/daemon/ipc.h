/***************************************************************************
                   ipc.h  -  Interprocess communication methods
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

int ipc_connect(int s,const char *p);
int ipc_stream(void);
int ipc_bind(int s,const char *p);
int ipc_bind_reuse(int s,const char *p);
int ipc_accept(int s,char *p,int l,int *trunc);
int ipc_local(int s,char *p,int l,int *trunc);
int ipc_listen(int s,int backlog);
int ipc_eid(int s,uid_t *u,gid_t *g, pid_t *p);

int ndelay_on(int fd);
int ndelay_off(int fd);

int getpeereid(int s,uid_t *u,gid_t *g,pid_t *p);
	
