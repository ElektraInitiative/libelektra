/***************************************************************************
      kdbprivate.h  -  Private stuff for the kdb implementation
                             -------------------
    begin                : Mon Apr 12 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id$
$LastChangedBy$

*/

#ifndef KDBPRIVATE_H
#define KDBPRIVATE_H

/* Elektra data directories */
#define KDB_DB_SYSTEM            "/etc/kdb"
#define KDB_DB_USER              ".kdb"   /* $HOME/.kdb */



#define BUFFER_SIZE 100

#ifdef UT_NAMESIZE
#define USER_NAME_SIZE UT_NAMESIZE
#else
#define USER_NAME_SIZE 100
#endif




#define UTF8_TO   1
#define UTF8_FROM 0






size_t encode(void *unencoded, size_t size, char *returned);
size_t unencode(char *encoded, void *returned);

int kdbNeedsUTF8Conversion();
int UTF8Engine(int direction, char **string, size_t *inputByteSize);







#endif /* KDBPRIVATE_H */
