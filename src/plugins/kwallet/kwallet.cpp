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

#include <kwallet.h>

#include <iostream>

using namespace std;
using namespace ckdb;


extern "C"
{

int elektraKwalletOpen(Plugin *handle, Key *)
{
	/* plugin initialization logic */

	cerr << "After kde" << endl;

	if(KWallet::Wallet::isEnabled())
	{
		KWallet::Wallet* wallet = 0;

		if(KWallet::Wallet::isOpen(KWallet::Wallet::LocalWallet()))
		{
			wallet = KWallet::Wallet::openWallet(KWallet::Wallet::LocalWallet(), false);

			if(wallet)
			{
				elektraPluginSetData(handle, wallet);
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
	return 0;
}

int elektraKwalletClose(Plugin *, Key *)
{
	/* free all backend resources and shut it down */

	cerr << "close kwallet" << endl;

#if 0

	KWallet::Wallet::closeWallet(KWallet::Wallet::LocalWallet(), false);
	KWallet::Wallet* wallet =
		static_cast<KWallet::Wallet*>(elektraPluginGetData(handle));
	delete wallet;

#endif

	return 0; /* success */
}

int elektraKwalletGet(Plugin *, KeySet *, Key *)
{
	cerr << "get kwallet" << endl;

#if 0

	KWallet::Wallet* wallet =
		static_cast<KWallet::Wallet*>(elektraPluginGetData(handle));

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

	return 1;
}

int elektraKwalletSet(ckdb::Plugin *, ckdb::KeySet *, ckdb::Key *)
{
	cerr << "set kwallet" << endl;
	/* set all keys below parentKey and count them with nr_keys */

	return 1;
}

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(kwallet)
{
	return elektraPluginExport("kwallet",
		ELEKTRA_PLUGIN_OPEN,		&elektraKwalletOpen,
		ELEKTRA_PLUGIN_CLOSE,		&elektraKwalletClose,
		ELEKTRA_PLUGIN_GET,		&elektraKwalletGet,
		ELEKTRA_PLUGIN_SET,		&elektraKwalletSet,
		ELEKTRA_PLUGIN_END);
}


} // extern "C"

