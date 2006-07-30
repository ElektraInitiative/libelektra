/***************************************************************************
                   sig.c  - Handle signals
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

#include <signal.h>
#include "sig.h"

int sig_alarm = SIGALRM;
int sig_child = SIGCHLD;
int sig_cont = SIGCONT;
int sig_hangup = SIGHUP;
int sig_int = SIGINT;
int sig_pipe = SIGPIPE;
int sig_term = SIGTERM;

void (*sig_defaulthandler)() = SIG_DFL;
void (*sig_ignorehandler)() = SIG_IGN;

void sig_block(int sig)
{
#ifdef HASSIGPROCMASK
	sigset_t ss;
	sigemptyset(&ss);
	sigaddset(&ss,sig);
	sigprocmask(SIG_BLOCK,&ss,(sigset_t *) 0);
#else
	sigblock(1 << (sig - 1));
#endif
}

void sig_unblock(int sig)
{
#ifdef HASSIGPROCMASK
	sigset_t ss;
	sigemptyset(&ss);
	sigaddset(&ss,sig);
	sigprocmask(SIG_UNBLOCK,&ss,(sigset_t *) 0);
#else
	sigsetmask(sigsetmask(~0) & ~(1 << (sig - 1)));
#endif
}

void sig_blocknone(void)
{
#ifdef HASSIGPROCMASK
	sigset_t ss;
	sigemptyset(&ss);
	sigprocmask(SIG_SETMASK,&ss,(sigset_t *) 0);
#else
	sigsetmask(0);
#endif
}

void sig_catch(int sig,void (*f)())
{
#ifdef HASSIGACTION
	struct sigaction sa;
	sa.sa_handler = f;
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	sigaction(sig,&sa,(struct sigaction *) 0);
#else
	signal(sig,f); /* won't work under System V, even nowadays---dorks */
#endif
}

void sig_pause(void)
{
#ifdef HASSIGPROCMASK
	sigset_t ss;
	sigemptyset(&ss);
	sigsuspend(&ss);
#else
	sigpause(0);
#endif
}

