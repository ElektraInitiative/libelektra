/***************************************************************************
 *    kdbobsolete.h  -  Obsolete API
 *
 * Do not use them if avoidable, they are to be removed next major
 * release.
 *
 *                         -------------------
 *  begin                : Sun 08 Dec, 2013
 *  copyright            : (C) 2013 by Markus Raab
 *  email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef KDBOBSOLETE_H
#define KDBOBSOLETE_H

#include <kdb.h>

char *keyNameGetOneLevel(const char *keyname, size_t *size);

#endif
