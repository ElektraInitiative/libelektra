/***************************************************************************
                serial_key.h  -  Low level objects serialization etc
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

#include "kdb.h"

ssize_t serialKey_getSize(const void *pKey);
ssize_t serialKey_serialize(const void *pKey, void *pBuffer);
ssize_t serialKey_unserialize(const void *pBuffer, void *pKey);
	
