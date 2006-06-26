/***************************************************************************
                serial_bin.c  -  Low level objects serialization etc
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


ssize_t serialInt_getSize(const void *pInt);
ssize_t serialInt_serialize(const void *pInt, void *pBuffer);
ssize_t serialInt_unserialize(const void *pBuffer, void *pInt);

ssize_t serialUlong_getSize(const void *pInt);
ssize_t serialUlong_serialize(const void *pInt, void *pBuffer);
ssize_t serialUlong_unserialize(const void *pBuffer, void *pInt);

