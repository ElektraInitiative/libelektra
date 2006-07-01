/***************************************************************************
                serial_string.h  -  Low level objects serialization etc
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


ssize_t serialString_getSize(const void *pChar);
ssize_t serialString_serialize(const void *pChar, void *pBuffer);
ssize_t serialString_unserialize(const void *pBuffer, void *ppChar);

