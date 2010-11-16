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

#include <kapplication.h>
#include <kaboutdata.h>
#include <kcmdlineargs.h>

#include <iostream>

using namespace std;
using namespace ckdb;

class PluginInfo
{
};


extern "C"
{

int elektraKwalletOpen(Plugin *handle, Key *)
{
	/* plugin initialization logic */

	cerr << "Open kde" << endl;

	// KInstance k("libelektra-kwallet");
	// qDebug(k.instanceName());

	// QCString("libelektra-kwallet"), 

	int argc = 1;
	char* argv[2];
	argv[0] = (char*)"libelektra-kwallet";
	argv[1] = 0;

	KAboutData aboutData ("libelektra-kwallet",
		I18N_NOOP("Elektra Access to KWallet"),
		"1.0",
		"Description",
		KAboutData::License_BSD,
		"(c) Markus Raab 2010",
		0,
		0,
		"elektra@markus-raab.org");
	KCmdLineArgs::init( argc, argv, &aboutData );


	KApplication k(false, false);

	cerr << "After kde" << endl;

	if(KWallet::Wallet::isEnabled())
	{
		KWallet::Wallet* wallet =
			KWallet::Wallet::openWallet(KWallet::Wallet::LocalWallet(), false);

		if(wallet)
		{
			elektraPluginSetData(handle, wallet);
			cerr << "Setting backend data worked" << endl;
		} else {
			cerr << "openWallet failed" << endl;
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

int elektraKwalletGet(Plugin *, KeySet *returned, Key *parentKey)
{
	cerr << "get kwallet" << endl;
	if (!strcmp (keyName(parentKey), "system/elektra/modules/kwallet"))
	{
		KeySet *moduleConfig = ksNew (50,
			keyNew ("system/elektra/modules/kwallet",
				KEY_VALUE, "kwallet plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/kwallet/exports", KEY_END),
			keyNew ("system/elektra/modules/kwallet/exports/open",
				KEY_FUNC, elektraKwalletOpen,
				KEY_END),
			keyNew ("system/elektra/modules/kwallet/exports/close",
				KEY_FUNC, elektraKwalletClose,
				KEY_END),
			keyNew ("system/elektra/modules/kwallet/exports/get",
				KEY_FUNC, elektraKwalletGet,
				KEY_END),
			keyNew ("system/elektra/modules/kwallet/exports/set",
				KEY_FUNC, elektraKwalletSet,
				KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos/description",
				KEY_VALUE, "Elektra Access to Kwallet", KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos/recommends",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/kwallet/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

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

