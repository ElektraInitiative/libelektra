/***************************************************************************
            kwallet.c  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libelektra.so a valid backend.                             *
 *   Simple fill the empty _kwallet functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "kwallet.hpp"

#if 0
#include <kapplication.h>
#include <kcmdlineargs.h>
#include <kaboutdata.h>
#include <kwallet.h>
#include <klocale.h>
#endif

#include <iostream>

using namespace std;
using namespace ckdb;


extern "C"
{

int kdbOpen_kwallet(KDB *handle)
{
	int errnosave = errno;
	KDBCap *cap = kdbhGetCapability (handle);

	cap->onlyFullGet=1;
	cap->noStat=1;

	cap->onlyRemoveAll=1;

	cap->onlyFullSet=1;
	cap->onlyAddKeys=1;

	cap->onlySystem=1;
	cap->onlyUser=1;

	cap->noOwner=1;
	cap->noValue=1;
	cap->noComment=1;
	cap->noUID=1;
	cap->noGID=1;
	cap->noMode=1;
	cap->noDir=1;
	cap->noATime=1;
	cap->noMTime=1;
	cap->noCTime=1;
	cap->noRemove=1;
	cap->noMount=1;
	cap->noBinary=1;
	cap->noString=1;
	cap->noTypes=1;
	cap->noError=1;

	cap->noLock=1;
	cap->noThread=1;

	/* backend initialization logic */

	cerr << "Before kde" << endl;

#if 0

	KAboutData about(QByteArray(BACKENDNAME),
			 QByteArray(BACKENDDISPLAYNAME),
			 KLocalizedString(),
			 QByteArray(BACKENDVERSION));
	KComponentData kcd(about);

	cerr << "After kde" << endl;

	errno = errnosave;

	if(KWallet::Wallet::isEnabled())
	{
		KWallet::Wallet* wallet = 0;

		if(KWallet::Wallet::isOpen(KWallet::Wallet::LocalWallet()))
		{
			wallet = KWallet::Wallet::openWallet(KWallet::Wallet::LocalWallet(), false);

			if(wallet)
			{
				ckdb::kdbhSetBackendData (handle, wallet);
				cerr << "Setting backend data worked" << endl;
			} else {

				cerr << "openWallet failed" << endl;
				return -1;
			}
		} else {
			cerr << "networkwallet already open" << endl;
			return -1;
		}
	} else {
		cerr <<  "is not enabled" << endl;
		return -1;
	}
#endif
	return 0;
}

int kdbClose_kwallet(KDB *handle)
{
	int errnosave = errno;
	/* free all backend resources and shut it down */

#if 0

	KWallet::Wallet::closeWallet(KWallet::Wallet::LocalWallet(), false);
	KWallet::Wallet* wallet = static_cast<KWallet::Wallet*>(ckdb::kdbhGetBackendData (handle));
	delete wallet;

#endif

	errno = errnosave;
	return 0; /* success */
}

ssize_t kdbGet_kwallet(KDB *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	int errnosave = errno;

#if 0

	KWallet::Wallet* wallet = static_cast<KWallet::Wallet*>(ckdb::kdbhGetBackendData (handle));

	cout << "in kdbGet_kwallet" << endl;
	QStringList list = wallet->folderList();
	QStringList::const_iterator it;
	for (it = list.begin(); it != list.end(); ++it)
	{
		std::string folder = (*it).toLocal8Bit().constData();
		cout << "Folder: " << folder << endl;
		ksAppendKey(returned, keyNew (("user/kwallet" + folder).c_str(), KEY_END));
		nr_keys ++;
	}

#endif

	errno = errnosave;
	return nr_keys; /* success */
}

ssize_t kdbSet_kwallet(KDB *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	int errnosave = errno;

	/* set all keys below parentKey and count them with nr_keys */

	errno = errnosave;
	return nr_keys;
}

KDB *KDBEXPORT(kwallet)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,	&kdbOpen_kwallet,
		KDB_BE_CLOSE,	&kdbClose_kwallet,
		KDB_BE_GET,	&kdbGet_kwallet,
		KDB_BE_SET,	&kdbSet_kwallet,
		KDB_BE_VERSION,        BACKENDVERSION,
		KDB_BE_AUTHOR,	"Markus Raab <elektra@markus-raab.org>",
		KDB_BE_LICENCE,	"BSD",
		KDB_BE_DESCRIPTION, "Kwallet Plugin",
		KDB_BE_END);
}

} // extern "C"

