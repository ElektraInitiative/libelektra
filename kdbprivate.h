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
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id$
$LastChangedBy$

*/

#ifndef KDBPRIVATE_H
#define KDBPRIVATE_H

/* Registry data directories */
#define RG_DB_SYSTEM            "/etc/kdb"
#define RG_DB_USER              ".kdb"   /* $HOME/.kdb */



size_t keySetRaw(Key *key, const void *newBinary, size_t dataSize);



#endif /* KDBPRIVATE_H */
