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
#include <klocale.h>

#include <iostream>

using namespace std;
using namespace ckdb;

class PluginData
{
	KApplication *k;
	KAboutData *aboutData;

public:
	KWallet::Wallet* wallet;
	PluginData()
	{
		cerr << "Open kde" << endl;

		// KInstance k("libelektra-kwallet");
		// qDebug(k.instanceName());

		// QCString("libelektra-kwallet"), 

		int argc = 1;
		char* argv[2];
		argv[0] = (char*)"libelektra-kwallet";
		argv[1] = 0;

		aboutData = new KAboutData ("libelektra-kwallet",
			I18N_NOOP("Elektra Access to KWallet"),
			"1.0",
			"Description",
			KAboutData::License_BSD,
			"(c) Markus Raab 2010",
			0,
			0,
			"elektra@markus-raab.org");
		KCmdLineArgs::init( argc, argv, aboutData, true);


		k = new KApplication (false, false);

		if(KWallet::Wallet::isEnabled())
		{
			wallet = KWallet::Wallet::openWallet(KWallet::Wallet::LocalWallet(), false);

			if(wallet)
			{
				cerr << "Setting backend data worked" << endl;
			} else {
				throw "openWallet failed";
			}
		} else {
			throw "KWallet is not enabled";
		}
	}

	~PluginData()
	{
		KWallet::Wallet::closeWallet(KWallet::Wallet::LocalWallet(), false);
		delete wallet;
		delete k;
		delete aboutData;
	}
};


extern "C"
{

int elektraKwalletOpen(Plugin *handle, Key *)
{
	/* plugin initialization logic */

	elektraPluginSetData(handle, 0);

	KeySet * config = elektraPluginGetConfig(handle);

	if (ksLookupByName(config, "/module", 0))
	{
		// suppress warnings if it is just a module
		// dont buildup the struct then
		return 0;
	}

	elektraPluginSetData(handle, new PluginData);

	cerr << "After kde" << endl;

	return 0;
}

int elektraKwalletClose(Plugin *handle, Key *)
{
	/* free all backend resources and shut it down */

	cerr << "close kwallet" << endl;

	delete static_cast<PluginData*>(elektraPluginGetData (handle));

	return 0; /* success */
}

int elektraKwalletGet(Plugin *handle, KeySet *returned, Key *parentKey)
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

	KWallet::Wallet* wallet =
		static_cast<PluginData*>(elektraPluginGetData(handle))->wallet;

	cout << "in kdbGet_kwallet" << endl;
	QStringList list = wallet->folderList();
	QStringList::const_iterator it;
	for (it = list.begin(); it != list.end(); ++it)
	{
		std::string folder ((*it).utf8());
		cout << "Folder: " << folder << endl;
		ksAppendKey(returned, keyNew (("user/kwallet" + folder).c_str(), KEY_END));
	}

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

