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

using namespace std;
using namespace ckdb;

#include <kdberrors.h>

class PluginData
{
	KAboutData *aboutData;

public:
	static KWallet::Wallet* wallet;
	PluginData()
	{
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


		if (!kapp) new KApplication (false, false);

		if(KWallet::Wallet::isEnabled())
		{
			/* Only the first plugin will open the wallet,
			 * the others access the singleton. */
			if (!wallet) wallet = KWallet::Wallet::openWallet(KWallet::Wallet::LocalWallet(), false);

			if (!wallet) throw "wallet still null pointer";
			if (!wallet->isOpen()) throw "isOpen returned false";
		} else {
			throw "KWallet is not enabled";
		}
	}

	~PluginData()
	{
		/* The first plugin, will close the wallet, the others
		 * do nothing */
		if (wallet)
		{
			KWallet::Wallet::closeWallet(KWallet::Wallet::LocalWallet(), false);
			delete wallet;
			wallet = 0;
		}
	}
};

KWallet::Wallet* PluginData::wallet;

extern "C"
{

int elektraKwalletOpen(Plugin *handle, Key *errorKey)
{
	/* plugin initialization logic */

	elektraPluginSetData(handle, 0);

	KeySet * config = elektraPluginGetConfig(handle);

	if (ksLookupByName(config, "/module", 0))
	{
		// suppress warnings if it is just a module
		// do not open wallet
		return 0;
	}

	try {
		elektraPluginSetData(handle, new PluginData);
	} catch (const char* msg) {
		ELEKTRA_ADD_WARNING (66, errorKey, msg);

		return -1;
	}

	return 0;
}

int elektraKwalletClose(Plugin *handle, Key *)
{
	/* free all backend resources and shut it down */

	KeySet * config = elektraPluginGetConfig(handle);

	if (ksLookupByName(config, "/module", 0))
	{
		// suppress warnings if it is just a module
		// do not close wallet
		return 0;
	}

	delete static_cast<PluginData*>(elektraPluginGetData (handle));
	elektraPluginSetData(handle, 0);

	return 0; /* success */
}

int elektraKwalletGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
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

	PluginData *pd = static_cast<PluginData*>(elektraPluginGetData(handle));
	if (!pd)
	{
		ELEKTRA_SET_ERROR (67, parentKey, "no plugindata found");
		return -1;
	}

	KWallet::Wallet* wallet = pd->wallet;
	if (!wallet)
	{
		ELEKTRA_SET_ERROR (67, parentKey, "wallet closed");
		return -1;
	}

	QStringList list = wallet->folderList();
	QStringList::const_iterator it;
	for (it = list.begin(); it != list.end(); ++it)
	{
		std::string folder ((*it).utf8());
		ksAppendKey(returned, keyNew ((keyName(parentKey)
						+ std::string("/")
						+ folder).c_str(), KEY_END));
		if (!wallet->setFolder(*it))
		{
			ELEKTRA_SET_ERROR (67, parentKey, "Could not set folder");
			return -1;
		}

		QStringList keyList = wallet->entryList();
		QStringList::const_iterator keyIt;
		for (keyIt = keyList.begin(); keyIt != keyList.end(); ++keyIt)
		{
			QByteArray value;
			std::string key ((*keyIt).utf8());
			if (wallet->readEntry (*keyIt, value) == -1)
			{
				ELEKTRA_SET_ERROR (67, parentKey, key.c_str());
				return -1;
			}
			std::string data (value.data(), value.size());

			Key *k;
			ksAppendKey(returned, k = keyNew ((keyName(parentKey)
						+ std::string("/")
						+ folder
						+ std::string("/")
						+ key).c_str(),
						KEY_BINARY,
						KEY_SIZE, data.size(),
						KEY_VALUE, data.c_str(),
						KEY_END));
			keySetMeta (k, "password", "1");
		}
	}

	return 1;
}

int elektraKwalletSet(ckdb::Plugin *, ckdb::KeySet *, ckdb::Key *errorKey)
{
	ELEKTRA_SET_ERROR (67, errorKey, "elektraKwalletSet currently not implemented");

	return -1;
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

