/***************************************************************************
                   kdbd.c  -  The server for the daemon backend
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2005 by Yannick Lecaillez
    email                : yl@itioweb.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id: kdbd.c 788 2006-05-29 16:30:00Z aviram $

*/

#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include "sig.h"
#include "ipc.h"

#ifndef SOCKET_NAME
#define SOCKET_NAME "/tmp/elektra.sock"
#endif


int numchildren = 0;
int limit = 20;
char remotepath[512];


void printstatus(void)
{
	fprintf(stdout, "kdbd: status: %d/%d", numchildren, limit);
}

int wait_nohang(int *wstat)
{
#ifdef HASWAITPID
	return waitpid(-1,wstat,WNOHANG);
#else
	return wait3(wstat,WNOHANG,(struct rusage *) 0);
#endif
}

void sigterm()
{
	exit(0);
}

void sigchld()
{
	int wstat;
	int pid;
	fprintf(stderr, "SIGCHILD\n");	
	while ((pid = wait_nohang(&wstat)) > 0) {
		fprintf(stdout, "kdbd: end %d status %d", pid, wstat);
		if (numchildren) --numchildren; printstatus();
	}
}


int main(int argc, char **argv)
{
	mode_t	m;
	int	t, s;
	int	trunc;

	sig_block(sig_child);
      	sig_catch(sig_child,sigchld);
    	sig_catch(sig_term,sigterm);
  	sig_ignore(sig_pipe);

	s = ipc_stream();
	if ( s == -1 ) {
		perror(argv[0]);
		return 1;
	}
	
	m = umask(0);
	if ( ipc_bind_reuse(s, SOCKET_NAME) == -1 ) {
		perror(argv[0]);
		return 1;
	}
	umask(m);

	if ( ipc_local(s, 0, 0, &trunc) == -1 ) {
		perror(argv[0]);
		return 1;
	}
	if (ipc_listen(s, 20) == -1) {
		perror(argv[0]);
		return 1;
	}
	ndelay_off(s); 
	printstatus();

	for(;;) {
		while (numchildren >= limit) sig_pause();
		sig_unblock(sig_child);

		t = ipc_accept(s,remotepath,sizeof(remotepath),&trunc);
		sig_block(sig_child); 

		if (t == -1) continue;
		++numchildren; printstatus();

		switch(fork()) {
			case 0:
				close(s);
				kdbd(t);
				sig_uncatch(sig_child);
				sig_unblock(sig_child);
				sig_uncatch(sig_term);
				sig_uncatch(sig_pipe); 
				break;
				
			case -1:
				perror("kdbd");
				--numchildren; printstatus();
		}

		fprintf(stderr, "Closing ...\n");
		close(t);
		fprintf(stderr, "Closed ...\n");
	}
}

