/***************************************************************************
      kdbcapability.c  -  Capability and attribute functions for backends
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/*
 * @defgroup capability KDB Backends :: Capabilities of backends
 * @brief The tactics to consider different possibilites of different backends.
 *
 * Since version 0.7 with the vast new possiblities of backends used
 * together in a global namespace the fact that not every backend can
 * fulfill any requirement as datastore and the intersection would
 * lead to a useless limited storage, capabilities were introduced.
 *
 * Applications heavily using kdbGet() and kdbSet() together with metadata
 * may be interested if what they show are default values or something you
 * can rely on.
 *
 * On the other hand backends may be interested to tell that they
 * can't fullfill the requriements described in kdbGet_backend() and
 * kdbSet_backend().
 *
 * Many backends are very limited in how granular the metadata is. If
 * many keys are stored in the same file, it is of course not possible
 * that one key has e.g. another owner than another key in the same
 * file.
 *
 * There might even be a limit in what names can be accepted by a
 * backend, because of syntax in a configuration file. A formal description
 * needed for that problem would be out of scope and not very useful.
 * For that reason capabilites are limited to yes/no questions
 * if something is provided or not.
 *
 * Capabilites are also very useful for testing driven or
 * iterative development. Beginning with simple reading everything out
 * transformed to keyset in kdbGet_backend() and generating a new
 * file with the keyset of kdbSet_backend() you can iterative
 * delete handicaps of your backend until you hit the full specification,
 * if it is possible.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <kdbbackend.h>

KDBCap *capNew ()
{
	KDBCap * cap = (KDBCap *)malloc(sizeof(KDBCap));
	if (!cap) return 0;
	memset(cap,0,sizeof(KDBCap));
	return cap;
}

void capDel (KDBCap *cap)
{
	free (cap);
}


/*Version of the library.
 *
 * @param cap the obsolete cap object
 * @ingroup capability*/
const char * kdbcGetVersion (const KDBCap *cap)
{
	return cap->version;
}


/*Name of backend being or that will be used 
 *
 * @param cap the obsolete cap object
 * @ingroup capability*/
const char * kdbcGetName (const KDBCap *cap)
{
	return cap->name;
}


/*Any text describing the backend.
 *
 * @param cap the obsolete cap object
 * @ingroup capability*/
const char * kdbcGetDescription (const KDBCap *cap)
{
	return cap->description;
}


/*The author of the backend.
 *
 * Best is format "Full Name <email@libelektra.org>"
 * e.g.:
 *
 * Avi Alkalay <avi@unix.sh>
 * Markus Raab <elektra@markus-raab.org>
 * Yannick Lecaillez <yl@itioweb.com>
 * Jens Andersen <jens.andersen@gmail.com>
 * Patrick Sabin <patricksabin@gmx.at>
 * Pier Luigi Fiorini <pierluigi.fiorini@mockup.org>
 * Rèmi <remipouak@yahoo.fr>
 * Studio-HB <contact@studio-hb.com>
 *
 *
 * @param cap the obsolete cap object
 * @ingroup capability*/
const char * kdbcGetAuthor (const KDBCap *cap)
{
	return cap->author;
}


/*The licence of the backend.
 *
 * - Must be "BSD" for bsd licence, this is the licence for elektra core.
 * - Must be "GPL2" for gpl version 2.
 * - Must be "GPL3" for gpl version 3.
 * - Must be "LGPL" for lgpl.
 *
 * Because of BSD licence even commercial is allowed.
 *
 *
 * @param cap the obsolete cap object
 * @ingroup capability*/
const char * kdbcGetLicence (const KDBCap *cap){
	return cap->licence;
}


/*You cant get specific keys.
 *
 * The specification of kdbGet_backend() says that you must
 * get a key with its subkey and nothing more or less. Declaring
 * onlyFullGet you are allowed to get all keys when you get
 * a kdbGet_backend() request of your mountpoint, but you need
 * to return 0 for any subkeys to avoid an infinite loop.
 *
 *
 * @param cap the obsolete cap object
 * @see kdbGet_backend()
 * @ingroup capability
 * */
unsigned int kdbcGetonlyFullGet (const KDBCap *cap)
{
	return cap->onlyFullGet;
}


/*All keys need to be in keyset when setting.
 *
 * Backends have not problem with setting single keys, when the
 * gap between those keys is already filled out with previous
 * calls of kdbSet(). The keys in between may not be in the
 * keyset.
 *
 * Declaring onlyFullSet requires the caller to kdbGet() all
 * keys of the backend and manipulating this full set. This
 * allows the backend to regenerate the whole config, making
 * implementation much easier.
 *
 *
 * @param cap the obsolete cap object
 * @see kdbSet_backend()
 * @ingroup capability
 **/
unsigned int kdbcGetonlyFullSet (const KDBCap *cap)
{
	return cap->onlyFullSet;
}


/*When getting keys they cant be stated.
 *
 * @see keyStat(), keyNeedStat()
 * @see kdbGet_backend()
 * @ingroup capability
 **/
unsigned int kdbcGetnoStat (const KDBCap *cap)
{
	return cap->noStat;
}


/*You can only remove all keys at once.
 *
 * @see kdbSet_backend()
 * @ingroup capability
 **/
unsigned int kdbcGetonlyRemoveAll (const KDBCap *cap)
{
	return cap->onlyRemoveAll;
}


/*When setting keys with new name below parentKey, they will be added.
 *
 * Otherwise (changing keys) all keys are required.
 *
 * @see kdbSet_backend()
 * @ingroup capability
 * */
unsigned int kdbcGetonlyAddKeys (const KDBCap *cap)
{
	return cap->onlyAddKeys;
}


/*The backend does not support a owner of keys.
 *
 *
 * @see keySetOwner() and keyGetOwner()
 * @ingroup capability
 * */
unsigned int kdbcGetnoOwner (const KDBCap *cap)
{
	return cap->noOwner;
}


/*No value is supported in the backend.
 *
 * A backend might be useful only having comments, metadata or only
 * names without any values as information for applications. Declare
 * this if your backend is not capable of storing values.
 *
 * @see keySetRaw()
 * @see keySetString() and keyGetString()
 * @see keyValue() (togehter with keyGetValueSize() if it is binary)
 * @ingroup capability
 * */
unsigned int kdbcGetnoValue (const KDBCap *cap)
{
	return cap->noValue;
}


/*No comment is supported in the backend.
 *
 * @see keySetComment() and keyGetComment()
 * @ingroup capability
 **/
unsigned int kdbcGetnoComment (const KDBCap *cap)
{
	return cap->noComment;
}


/*No uid is supported in the backend.
 *
 * @see keySetUID()
 * @ingroup capability
 **/
unsigned int kdbcGetnoUID (const KDBCap *cap)
{
	return cap->noUID;
}


/*No gid is supported in the backend.
 *
 * @see keySetGID()
 * @ingroup capability
 **/
unsigned int kdbcGetnoGID (const KDBCap *cap)
{
	return cap->noGID;
}


/*No mode is supported in the backend.
 *
 * Means that keys will only have 664 or
 * 775 as mode values.
 *
 * @see keySetMode()
 * @ingroup capability
 **/
unsigned int kdbcGetnoMode (const KDBCap *cap)
{
	return cap->noMode;
}


/*Directories are not supported in the backend.
 *
 * Tells that mode won't have any executable
 * bits set.
 *
 * Declaring kdbcGetnoDir() means
 * that the backend is flat, no key will be true for keyIsDir()
 * and so can't have any subkeys.
 *
 * @see keySetDir()
 * @ingroup capability
 **/
unsigned int kdbcGetnoDir (const KDBCap *cap)
{
	return cap->noDir;
}


/*Access Time not supported.
 *
 * @see keySetATime()
 * @ingroup capability
 **/
unsigned int kdbcGetnoATime (const KDBCap *cap)
{
	return cap->noATime;
}


/*Modification Time not supported.
 *
 * @see keySetMTime()
 * @ingroup capability
 **/
unsigned int kdbcGetnoMTime (const KDBCap *cap)
{
	return cap->noMTime;
}


/*Meta Info and Subkeys Change Time not supported.
 *
 * @see keySetCTime()
 * @ingroup capability
 **/
unsigned int kdbcGetnoCTime (const KDBCap *cap)
{
	return cap->noCTime;
}


/*The backend does not support removing keys.*/
unsigned int kdbcGetnoRemove (const KDBCap *cap)
{
	return cap->noRemove;
}



/*Mount type not supported.*/
unsigned int kdbcGetnoMount (const KDBCap *cap)
{
	return cap->noMount;
}


/*Binary types not supported.
 *
 * @see keySetBinary()
 * @ingroup capability
 **/
unsigned int kdbcGetnoBinary (const KDBCap *cap)
{
	return cap->noBinary;
}


/*String types not supported.
 *
 * @see keySetString()
 * @ingroup capability
 **/
unsigned int kdbcGetnoString (const KDBCap *cap)
{
	return cap->noString;
}


/*Typing of keys is not supported.
 *
 * @see keySetRaw() and keySetType()
 * @ingroup capability
 **/
unsigned int kdbcGetnoTypes (const KDBCap *cap)
{
	return cap->noTypes;
}


/*Do not expect errno to be set correctly.
 *
 * @see kdbhSetError()
 * @ingroup capability
 **/
unsigned int kdbcGetnoError (const KDBCap *cap)
{
	return cap->noError;
}


/*Backend does not lock.
 *
 * @ingroup capability
 **/
unsigned int kdbcGetnoLock (const KDBCap *cap)
{
	return cap->noLock;
}


/*Backend uses global variables and is not threadsafe.
 *
 * @ingroup capability
 **/
unsigned int kdbcGetnoThread (const KDBCap *cap)
{
	return cap->noThread;
}

