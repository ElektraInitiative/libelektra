/***************************************************************************
      registryprivate.h  -  Private stuff for the registry implementation
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

#ifndef REGISTRYPRIVATE_H
#define REGISTRYPRIVATE_H

/* Registry data directories */
#define RG_DB_SYSTEM            "/etc/registry"
#define RG_DB_USER              ".registry"   /* $HOME/.registry */



size_t keySetRaw(Key *key, void *newBinary, size_t dataSize);



#endif /* REGISTRYPRIVATE_H */
