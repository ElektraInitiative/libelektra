#include <ckdb.h>

extern "C"
{
#include <kdb.h>
}

#include <iostream>

#define BUFFER_SIZE 3000



ckdb::ckdb (std::string root)
{
       	char keyName [BUFFER_SIZE + 1];
        char value   [BUFFER_SIZE + 1];
	std::string sname, svalue, app_root;
        Key *current;
	size_t csize;
	char * field;
	
	// Init class variables
	user_root = "user/";
	user_root += root;
	needs_sync = false;
	
	for (int i=0; i< 2; i++)
	{
		if (i) app_root = "user/";
		else app_root = "system/";
		app_root += root;

		// Initializise
		KeySet * myConfig = new KeySet;
		ksInit(myConfig);
		kdbOpen();

		// Get all value keys for this application
		if (kdbGetChildKeys(app_root.c_str(), myConfig, KDB_O_RECURSIVE))
			cerr << "Could not get Configuration for " << app_root << endl;

		for (current=myConfig->start; current; current=current->next) {
			
			cerr << "beg";
			csize = keyGetBaseNameSize (current);
			if (csize < BUFFER_SIZE)
			{
				keyGetBaseName(current,keyName,sizeof(keyName));
				sname = keyName;
			} else {
				field = new char [csize];
				keyGetBaseName(current, field, csize);
				sname = field;
				delete (field);
			}
			
			cerr << "got basename";
			csize = keyGetDataSize (current);
			if (csize < BUFFER_SIZE)
			{
				keyGetString(current,value,sizeof(value)); // fast Method
				svalue = value;
			} else {
				cerr << "get string" << endl;
				field = new char [csize]; // slow Method
				keyGetString(current, field, csize);
				cerr << "got string" << endl;
				svalue = field;
				delete (field);
			}
				
			svalue = value;

			container [sname] = svalue;
			cout << "container[" << sname << "] = " << svalue << endl;
		}

		// Destruct
		kdbClose();
		ksClose(myConfig);
		delete (myConfig);
	}
}

ckdb::~ckdb ()
{
	if (! needs_sync) return;
	char keyName [BUFFER_SIZE];
	std::string sname;
        Key *current;
	size_t csize;
	char * field;

	KeySet * myConfig = new KeySet;
        ksInit(myConfig);
        kdbOpen();

	if (kdbGetChildKeys(user_root.c_str(), myConfig, KDB_O_RECURSIVE))
		cerr << "Could not get Configuration for " << user_root << endl;
	
	for (current=myConfig->start; current; current=current->next) {

		if (csize < BUFFER_SIZE)
		{
			keyGetBaseName(current,keyName,sizeof(keyName));
			sname = keyName;
		} else {
			field = new char [csize];
			keyGetBaseName(current, field, csize);
			sname = field;
			delete (field);
		}
		keySetString(current, container [sname].c_str() );
	}

	kdbSetKeys (myConfig);

	// Destruct
	kdbClose();
	ksClose(myConfig);
	delete (myConfig);
}

std::string & ckdb::get (std::string key)
{
	return container [key];
}

void ckdb::set (std::string key, std::string value)
{
	needs_sync = true;
	container [key] = value;
}


