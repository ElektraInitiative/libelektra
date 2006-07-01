/***************************************************************************
                serial_keyset.h  -  Low level objects serialization etc
                             -------------------
    begin                : Sun Mar 12 2006
    copyright            : (C) 2006 by Yannick Lecaillez, Avi Alkalay
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

*/



ssize_t serialKeySet_getSize(const void *pKeySet);
ssize_t serialKeySet_unserialize(const void *pBuffer, void *pKeySet);
ssize_t serialKeySet_serialize(const void *pKeySet, void *pBuffer);
