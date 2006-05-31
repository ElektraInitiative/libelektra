/**************************************************************************
 *  Copyright (C) 2006 by Yannick Lecaillez                               *
 *  sizon5@gmail.com                                                      *
 *                                                                        *
 *  This program is free software; you can redistribute it and/or modify  *
 *  it under the terms of the GNU General Public License as published by  *
 *  the Free Software Foundation; either version 2 of the License         *
 *                                                                        *
 *  This program is distributed in the hope that it will be useful,       *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *  GNU General Public License for more details.                          *
 *                                                                        *
 *  You should have received a copy of the GNU General Public License     *
 *  along with this program; if not, write to the                         *
 *  Free Software Foundation, Inc.,                                       *
 *  59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 **************************************************************************/

#include <iostream>
using namespace std;

#include <qdir.h>
#include <kdebug.h>
#include <kapplication.h>
#include <kconfig.h>
#include <kstandarddirs.h>

#include "kdb.h"

int kde2elektra(const QString &absFname, const QString &baseDir)
{
	KeySet		*ks;
	bool		user;
	QString 	relName(absFname);
	QFileInfo	info(absFname);
	
	relName.remove(0, baseDir.length());
	user = baseDir.startsWith(getenv("HOME"));

	ks = ksNew();
	
	KConfig c(absFname);
	QStringList groups = c.groupList();
	for (QStringList::const_iterator it = groups.constBegin();
		         it != groups.constEnd();
		          ++it) {

		QMap<QString, QString> entries = c.entryMap(*it);
		int numEntries = entries.count();

		if (numEntries < 1) {
			continue;
		}

		QString groupName((*it));
		groupName.replace('/', "\\/");

		QMap<QString, QString>::const_iterator endEntries = entries.constEnd();
		for (QMap<QString, QString>::const_iterator entryIt = entries.constBegin();
				entryIt != endEntries;
				++entryIt) {

			if ( entryIt.data().isEmpty() )
				continue;

			QString keyName(entryIt.key());
			keyName.replace('/', "\\/");
						
			QString kName(groupName + "/" + keyName);

			if ( user )
				kName.prepend("user/sw/kde/current/kde/config/" + relName + "/");
			else
				kName.prepend("system/sw/kde/current/kde/config/" + relName + "/");
			
			Key *key;
			
			key = keyNew((const char *) kName.utf8(), 
					KEY_SWITCH_UID, info.ownerId(),
					KEY_SWITCH_GID, info.groupId(),
					KEY_SWITCH_VALUE, (const char *) entryIt.data().utf8(),
					KEY_SWITCH_END);
			ksAppend(ks, key);
		}
	}

	char *oldLocale;
	
	oldLocale = setlocale(LC_ALL, NULL);
	if ( setlocale(LC_ALL, "UTF-8") == NULL ) {
		cout << "Can't change locale to UTF-8" << endl;
		exit(1);
	}

	KDBHandle	handle;
	
	kdbOpen(&handle);
	kdbSetKeys(handle, ks);
	kdbClose(&handle);

	ksDel(ks);

	setlocale(LC_ALL, oldLocale);
	
	return 0;
}
		
int convertDir(const QString &dirName, const QString &baseDir)
{
	QDir 		dir(dirName);
	QStringList	files = dir.entryList();

	for (QStringList::Iterator at = files.begin(); at != files.end(); at++) {
		if ( (*at) == "." || (*at) == ".." )
			continue;
		QString fName(dir.absFilePath((*at), false));
//		fName = dirName;
//		fName.append((*at));
		
		QFileInfo info(fName);
		if ( info.isDir() )
			convertDir(fName, baseDir);

		kde2elektra(fName, baseDir);
	}

	return 0;
}


int main(int argc, char **argv)
{
	QString	dir;
	KApplication  a( argc, argv, "kde2elektra" );
	
	QStringList configDir = KGlobal::dirs()->resourceDirs("config");
	if ( configDir.size() != 2 ) {
		cout << "Your KDE config use more than 2 directories. I can't do nothing for you :-(." << endl;
		exit(1);
	}

	if ( getuid() ) {
		cout << "You're root, importing system wide config." << endl;
		cout << "Re-run me after that as user for importing user specific config (if not done yet)." << endl;
		convertDir(configDir.last(), configDir.last());
		cout << "Importing root specific config ..." << endl;
		convertDir(configDir.first(), configDir.first());
	} else {
		cout << "You're user, importing user specific config." << endl;
		cout << "Re-run me after that as root for importing system wide config (if not done yet)." << endl;
		convertDir(configDir.first(), configDir.first());
	}

	return 0;	
//	kdDebug() << "config dir is " << configDir << endl;
}	
	    
