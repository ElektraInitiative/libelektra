#include <ckdb.h>

//TODO: increase Buffer Size
#define BUFFER_SIZE 5

/**Constructs a class ckdb. 
 * Read reads all values of root and stores them into a hash.
 * User and System focus is done automatically.
 * If there is no user-value is available, the system value is
 * taken.
 * The root is only the part without user or system. E.g. sw/test
 * will collect all keys/values pairs from system/sw/test and
 * user/sw/test.*/
ckdb::ckdb (std::string root /// Root of app
		)
{	
       	char keyName [BUFFER_SIZE + 1];
        char value   [BUFFER_SIZE + 1];
	std::string sname, svalue, app_root;
        Key *current;
	size_t csize;
	char * field;
	
	user_root = "user/";
	user_root += root;
	needs_sync = false;
	container.clear();

	for (int i=0; i< 2; i++)
	{
		if (i) app_root = "user/";
		else app_root = "system/";
		app_root += root;

		// Initializise
		user_keys = new KeySet;
		ksInit(user_keys);
		kdbOpen();


		// Get all value keys for this application
		if (kdbGetChildKeys(app_root.c_str(), user_keys, KDB_O_RECURSIVE))
			cerr << "Could not get Configuration for " << app_root << endl;

		// Destruct
		kdbClose();
		
		for (current=user_keys->start; current; current=current->next) {
			
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
			
			csize = keyGetDataSize (current);
			if (csize < BUFFER_SIZE)
			{
				keyGetString(current,value,sizeof(value)); // fast Method
				svalue = value;
			} else {
				field = new char [csize]; // slow Method
				keyGetString(current, field, csize);
				svalue = field;
				delete (field);
			}
				
			container [sname] = svalue;
		}


		if (i==0) // when fetching the user keys, don't close user_keys!
		{
			ksClose(user_keys);	
			delete (user_keys);
		}
	}
}

/**The destructor automatically commit a write.*/
ckdb::~ckdb ()
{
	write();
	ksClose (user_keys);
	delete user_keys;
}

/**Writes all commits to the keydatabase. The same root is taken
 * as in read(std::string). The key/value pairs are stored in
 * user/, so no root privilegies are required.*/
void ckdb::write ()
{	
	if (! needs_sync) return;
	needs_sync = false;

    kdbOpen();
	kdbSetKeys (user_keys);
	kdbClose();
	
	ksClose(user_keys);
	delete (user_keys);
	
	// Initializise new user_keys
	user_keys = new KeySet;
	ksInit (user_keys);
}

/**Get the value from a specific key. This access is very fast, because it only
 * returns the refernce from a hash.*/
std::string & ckdb::get (std::string key)
{
	return container [key];
}

/**Sets a key with a specific value.*/
void ckdb::set (std::string key, std::string value)
{
	needs_sync = true;
	container [key] = value;
	Key * k = new Key;
	keyInit (k);
	keySetName (k, (user_root + "/" + key).c_str());
	keySetString (k, value.c_str());
	ksAppend (user_keys, k);
}


