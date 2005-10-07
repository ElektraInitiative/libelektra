/***************************************************************************
            ini.c  -  Backend for ini-style like files
                             -------------------
    begin                : 01.03.2005
    updated              : 06.10.2005
    copyright            : (C) 2005 by Markus Raab
    email                : debian@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *
 *   This is a ini-style backend.
 *   Key/Value Pairs are stored in files in following scheme:
 *   
 *   key1=value1;comment
 *
 * TODO:
 *   create new files and folders
 *   allow subkeys setting/getting
 *   make \n\t engine
 *   setting errno properly
 *   remove debugging features
 *
 * EXTRA:
 *   Filelock does not work over NFS. (use fcntl instead of flock)
 *   include statement (like linking)
 *   monitor/stat files (in which the keys are)
 *   rename/remove keys features
 *
 ***************************************************************************/

#include <kdb.h>
#include <kdbbackend.h>

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <pwd.h>
#include <fcntl.h>
#include <dirent.h>

#include <sys/stat.h>
#include <sys/file.h>
#include <sys/types.h>

#define BACKENDNAME "ini"


/**Some systems have even longer pathnames */
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posix system */
#elif _POSIX_PATH_MAX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
/**Fallback: This value should be useful*/
#define MAX_PATH_LENGTH 4096
#endif


/**This buffer size is fastest for reading and writing
 * in files*/
#define BUFFER_RDWR_SIZE 8024

/**States of parsing the key*/
#define STATE_KEY 1
#define STATE_VALUE 2
#define STATE_COMMENT 4
#define STATE_END 8

/**Some more key types needed for ini
 * FILE ... is a real file on the system
 * DIR ... is a real directory
 * SUBDIR ... is a subdirectoy within a file*/
#define KEY_TYPE_FILE 4
#define KEY_TYPE_DIR 8
#define KEY_TYPE_SUBDIR 16

/**Global variables*/

/**These filedeskriptor hold the current open file*/
FILE * fc;
int fd;

/**Reallocate Storage in a save way
 * @code
if (srealloc ((void **) & buffer, new_length) < 0) {
	// here comes the failure handler
	fprintf (stderr, "Reallocation error\n");
	free (buffer);	// free the buffer
	exit (1);
}
 * @param void ** buffer is a pointer to a malloc
 * @param size is the new size for the memory
 * @return -1 on failure
 * @return 0 on success
 * @ingroup ini
 */
int srealloc (void ** buffer, size_t size)
{
	void * ptr;
	void * svr = *buffer;
	ptr = realloc(*buffer, size);
	if (ptr == NULL)
	{
		*buffer = svr;	// restore old buffer
		return -1;
	} else {
		*buffer = ptr;
		return 0;
	}
}


/**
 * Initialize the backend.
 *
 * It does not do anything.
 * Apps anyway must call it, to make sure
 * to be compatibel to other backends.
 * 
 * @return 0 on success
 * @ingroup ini
 */
int kdbOpen_ini() {
	return 0;
}




/**
 * Closes the backend.
 * 
 * It does not do anything.
 * Apps anyway must call it, to make sure
 * to be compatibel to other backends.
 * 
 * @return 0 on success
 * @ingroup ini
 */
int kdbClose_ini() {
	return 0; /* success */
}


/**
 * Opens a file filename.
 * The mode might be O_RDONLY or O_RDWR.
 *
 * It handles the failures very safty.
 * Don't use any other open inside the
 * backend.
 * 
 * @see close_file
 *
 * You have to close it with close_file
 * because there is also a file locking
 * done.
 * 
 * @return 0 on success, -1 on failure
 * @ingroup ini
 * */
int open_file (char * filename, int mode)
{
	char buffer [2] = "\0\0";
	int ret = 0;
	
	if (mode == O_RDWR)
	{
		buffer[0] = 'r';
		buffer[1] = '+';
	}
	else if (mode == O_RDONLY)
	{
		buffer[0] = 'r';
	} else {
		fprintf (stderr, "Mode not useful\n");
		return -1;
	}

	fd = open (filename, mode);
	if (fd == -1) {
		fprintf (stderr, "Unable to open file\n");
		return -1;
	}
	
	if (flock (fd, LOCK_EX) == -1) {
		fprintf (stderr, "Unable to lock file\n");
		ret = -1;
	}
		
	
	fc = fdopen (fd,buffer);
	if (fc == NULL) {
		fprintf (stderr, "fdopen() failed\n");
		ret -2;
	}
	return ret;
}

/**
 * Close previous with open_file() opened file
 * @return 0 on success, -1 on failure
 */
int close_file ()
{
	int ret = 0;
	
	if (flock (fd, LOCK_UN) == -1) {
		perror ("Unable to unlock file");
		ret = -1;
	}

	ret = fclose (fc); // close file
	if (ret != 0) {
		perror ("Could not close file");
		ret = -2;
	}
	return ret;
}


/**
 * Enlarges file on place where with space bytes. The new
 * place will contain the previous text. The text before
 * where will not be touched.
 * 
 * @param where: holds the place where a new space is needed
 * @param space: holds the size of the new needed space
 * 
 * @return 0 on success, -1 else
 * @ingroup ini
 */
int enlarge_file (long where, long space)
{
	char buffer [BUFFER_RDWR_SIZE+1];
	size_t sread;
	long diff = 0;
	int err;
	int finished = 0;
	long pos;

	fseek (fc,0,SEEK_END); // begin at end
	pos = ftell (fc);
	do {
		pos -= BUFFER_RDWR_SIZE;
		if (pos < where) {
			diff = where - pos;
			pos = where;
			finished = 1;
		}
		fseek (fc, pos, SEEK_SET);
		sread = fread (buffer,1,BUFFER_RDWR_SIZE-diff,fc);	// read last peace
		buffer[sread] = 0;	// mark end (not necessary)

		fseek (fc,pos+space,SEEK_SET);	// jump to writepos

		printf ("buffer: %s, sread: %d\n", buffer, sread);
		fwrite (buffer,1,sread,fc);
		err = ferror (fc);
		if (err != 0)
		{
			fprintf (stderr, "Error in stream\n");
			return -1;
		}
	} while (! finished);

	return 0;
}

/**
 * Shrinks file on place where with space bytes.
 * The old text (length space after where) will 
 * be lost! The text before where will not be touched.
 *
 * @param where: The File will be shrinked here
 * @param space: The size how much the file will be shrinked
 * 
 * @return 0 on success, -1 on error
 * @ingroup ini
 */
int shrink_file (long where, long space)
{
	char buffer [BUFFER_RDWR_SIZE+1];
	size_t sread;
	int err;
	long pos;

	fseek (fc,where, SEEK_SET);
	pos = ftell (fc);
	
	do {
		fseek (fc,pos+space,SEEK_SET); // jump to readposition
		sread = fread (buffer,1,BUFFER_RDWR_SIZE,fc);	// read a peace
		buffer[sread] = 0;	// mark end (not necessary)

		fseek (fc,pos,SEEK_SET);	// jump to writepos

		printf ("buffer: %s, sread: %d\n", buffer, sread);
		fwrite (buffer,1,sread,fc);
		err = ferror (fc);
		if (err != 0)
		{
			fprintf (stderr, "Error in stream\n");
			return -1;
		}
		pos += sread;
	} while (sread == BUFFER_RDWR_SIZE);

	ftruncate (fd,lseek(fd,0,SEEK_CUR));
	
	return 0;
}

/**
 * Get the basename for the Key forKey.
 *
 * This might be:
 * /etc/kdb
 * /home/markus/.kdb
 * 
 * There are 2 possibilites. It may be KEY_NS_SYSTEM
 * or KEY_NS_USER. When the key of a user is asked
 * for, then environment will be asked what USER is
 * logged on.
 *
 * @see IniGetFileName
 * 
 * @ingroup ini
 */
size_t IniGetBaseName (const Key * forKey, char * basename)
{
	size_t length;
	int namesize;
	char * name;

        switch (keyGetNamespace(forKey)) {
                case KEY_NS_SYSTEM: {
                        /* Prepare to use the 'system/ *' database */
                        strncpy(basename,KDB_DB_SYSTEM,MAX_PATH_LENGTH);
                        length=strlen(basename);
                        break;
                }
                case KEY_NS_USER: {
                        /* Prepare to use the 'user:????/ *' database */
                        struct passwd *user=0;

                        if (forKey->userDomain) user=getpwnam(forKey->userDomain);
                        else user=getpwnam(getenv("USER"));

                        if (!user) return 0; /* propagate errno */
                        length=snprintf(basename,MAX_PATH_LENGTH,"%s/%s",user->pw_dir,KDB_DB_USER);
                        break;
                }
                default: {
                        errno=KDB_RET_INVALIDKEY;
                        return 0;
                }
        }
	
	return length;
}

/**
 * Returns the filename from the Key forKey
 *
 * The name returned is normally not correct, because
 * it may have subdirs and it has the keyname in it.
 *
 * @see IniSearchFileName
 * will cut of the end until it has found a file.
 * 
 * @param filename: MAX_PATH_LENGTH size char*
 * @param keyname: MAX_PATH_LENGTH size char*
 * 
 * @ingroup ini
 */
size_t IniGetFileName (Key * forKey, char * filename)
{
	size_t length;
	size_t namesize;
	char * name;

	length = IniGetBaseName (forKey, filename);

        filename[length]='/'; length++; // now the Keyname follows

	namesize = keyGetNameSize (forKey);
	if (namesize == 0) return 0;
	name = (char*) malloc (namesize);

	keyGetName (forKey, name, namesize);

	if (length > MAX_PATH_LENGTH) return -1;	// too long

	strncpy (filename + length, name, namesize);
	length += namesize;
	free (name);

	return length;
}


/**
 * Returns a filename from the Key which can be opened for forKey
 *
 * The name returned should be correct, because
 * it removes subdirs and the keyname.
 *
 * It even tests with kdbStatKey if the File exists.
 * So be prepaired that stat information is filled
 * within the key.
 *
 * @see IniSearchFileName
 * will cut of the end until it has found a file.
 * 
 * @param filename: MAX_PATH_LENGTH size char*
 * @param keyname: MAX_PATH_LENGTH size char*
 * 
 * @ingroup ini
 */
size_t IniSearchFileName (Key * forKey, char * filename)
{
	size_t length;
	u_int8_t info = 0;
	char * end;

	length = IniGetFileName (forKey, filename);
	
	do {
		end = strrchr (filename, '/');
		if (end == NULL) {
			fprintf (stderr, "Could not find any file\n");
			return -1;
		}
		*end= '\0';
		IniStatFile (forKey, filename);
		info = keyGetType (forKey);
	} while (!(info & KEY_TYPE_FILE));

	return length;
}

/**Debug: Define very small Buffer Size to make
 * sure that realloc works!*/
// #define BUFFER_SIZE 4

/**
 * Read one key out of a file.
 *
 * It does not check the Name of the Key.
 *
 * @param key: Will contain information of key
 * @param root: The name of the key
 * 
 * @ingroup ini
 */
int IniGetKey (Key * key, char * root)
{
	char * buffer;
	char * buffer_value;
	char * buffer_key;
	char * buffer_comment;
	char * buffer_name;
	int rc;
	
	int i;
	int state = STATE_KEY;	// start reading the key
	
	int string_length = BUFFER_SIZE;
	int value_length = BUFFER_SIZE;
	int key_length = BUFFER_SIZE;
	int comment_length = BUFFER_SIZE;
	
	int v=0;	// position of value
	int k=0;	// position of key
	int c=0;	// position of comment
	
	buffer = (char*) malloc (BUFFER_SIZE+1);
	
	if (fgets (buffer, BUFFER_SIZE,fc) == NULL) {
		fprintf (stderr, "End of File\n");
		free (buffer);
		return -1;
	}
	
	buffer_value = (char*) malloc (BUFFER_SIZE+1);
	buffer_key = (char*) malloc (BUFFER_SIZE+1);
	buffer_comment = (char*) malloc (BUFFER_SIZE+1);
	
	for (i=0; i < string_length; i++) {
//		fprintf (stderr, "Processing |%c|%d|\n", buffer[i], buffer[i]);
		if (buffer[i] == '\n') { // end of line found
//			fprintf (stderr, "Found end of key (\\n)\n");
			break;
		}
		else if (buffer[i] ==  '\0' ) {	// anticipated end?
			if (i==string_length-1) { // no its not
				string_length += BUFFER_SIZE;
				if (srealloc ((void**) & buffer, string_length) < 0) {
					fprintf (stderr, "Reallocation error\n");
					free (buffer);
					free (buffer_key);
					free (buffer_value);
					free (buffer_comment);
					return -1;
				}
				fgets (buffer+string_length-BUFFER_SIZE,
					BUFFER_SIZE,fc);
			} else {
				fprintf (stderr, "No Enter found in this line?\n");
				return -1;
			}
		}
		else if (buffer[i] == '=') {	// value follows
			state = STATE_VALUE;
		}
		else if (buffer[i] == ';') {	// comment follows
			state = STATE_COMMENT;
		}
		else if (state == STATE_KEY) {
			buffer_key [k++] = buffer[i];
			if (k == key_length-1)
			{
				key_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_key, key_length) < 0) {
					fprintf (stderr, "Reallocation error\n");
					free (buffer);
					free (buffer_key);
					free (buffer_value);
					free (buffer_comment);
					return -1;
				}					
			}
		}
		else if (state == STATE_VALUE) {
			buffer_value [v++] = buffer[i];
			if (v == value_length-1) 
			{
				value_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_value, value_length) < 0) {
					fprintf (stderr, "Reallocation error\n");
					free (buffer);
					free (buffer_key);
					free (buffer_value);
					free (buffer_comment);
					return -1;
				}					
			}
		}
		else if (state == STATE_COMMENT) {
			buffer_comment [c++] = buffer[i];
			if (c == comment_length-1)
			{
				comment_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_comment, comment_length) < 0) {
					fprintf (stderr, "Reallocation error\n");
					free (buffer);
					free (buffer_key);
					free (buffer_value);
					free (buffer_comment);
					return -1;
				}					
			}
		}
	}

	free (buffer);
	buffer_value [v] = 0;
	buffer_key [k] = 0;
	buffer_comment [c] = 0;	// key eingelesen

	/*if (key->key) free (key->key);
	key->key = malloc (k+1);
	strcpy (key->key, buffer_key);*/

	
	if ((buffer_name = malloc (strlen(buffer_key) + strlen(root) + 2)) == NULL)
	{
		fprintf (stderr, "Allocation error");
		free (buffer_key);
		free (buffer_value);
		free (buffer_comment);
		return -1;
	}
	buffer_name[0] = '\0';	// buffer_name is empty
	strcat (buffer_name, root);
	strcat (buffer_name, "/");
	strcat (buffer_name, buffer_key);
	rc = keySetName (key, buffer_name);
	if (rc==0) {
		fprintf (stderr, "Unable to set name\n");
	} else	fprintf (stderr, "Name set to %s\n", buffer_name);
	free (buffer_name);
	
	/*if (key->data) free (key->data);	
	key->data = malloc (v+1);
	strcpy (key->data, buffer_value);
	key->dataSize = v+1;*/
	rc = keySetString (key, buffer_value);
	if (rc==0) {
		fprintf (stderr, "Unable to set value\n");
	} else fprintf (stderr, "Value set to %s\n", buffer_value);
	
	/*if (key->comment) free (key->comment);
	key->comment = malloc (c+1);
	strcpy (key->comment, buffer_comment);
	key->commentSize = c+1;*/
	rc = keySetComment (key, buffer_comment);
	if (rc==0) {
		fprintf (stderr, "Unable to set comment\n");
	} else fprintf (stderr, "Comment set to %s\n", buffer_comment);
	
	/*key->type = KEY_TYPE_STRING;
	key->recordSize = v+c+2;*/

	// remove sync flag
	key->flags &= ~KEY_SWITCH_NEEDSYNC;

	free (buffer_value);
	free (buffer_key);
	free (buffer_comment);
	
	return 0; /* success */
}

/**
 * Implementation for kdbGetKey() method.
 *
 * @see kdbGetKey() for expected behavior.
 * @ingroup ini
 */
int kdbGetKey_ini(Key *key) {
	char keyFileName [MAX_PATH_LENGTH];
	
	int pos;
	int keySize;
	char * keyFullName;
	char * keyRoot;
	char * end;
	
	fprintf (stderr, "kdbGetKey_ini() entered\n");
	
	pos = IniSearchFileName(key, keyFileName);
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	keyGetName(key, keyFullName, keySize);
	
	end = strrchr (keyFullName, '/');	// dirname
	*end = 0;
	keyRoot = malloc (strlen (keyFullName));
	strcpy (keyRoot, keyFullName);
	*end = '/';
	
	fprintf (stderr, "keyRoot: %s\n", keyRoot);

	if (! pos) {
		fprintf (stderr, "Could not receive filename");
		return -1;
	}
	fprintf (stderr, "Get Key [%d] in File: %s\n", keySize, keyFileName);

	open_file (keyFileName, O_RDONLY);
	
	while ((pos=IniGetKey (key, keyRoot)) == 0)
	{
		fprintf (stderr, "Compare: %s with %s\n", key->key, keyFullName);
		if (strcmp (key->key, keyFullName) == 0) {	// right Key found
			//TODO: use keySetName (key, keyFullName);
			//or DONT EVEN SET, because key->key and keyFullName is the same
			fprintf (stderr, "Key found\n");
			if (key->key) free(key->key);
			key->key = malloc (keySize+1);
			strncpy (key->key, keyFullName, keySize);
	
			fprintf (stderr, "key: %s, value: %s, comment: %s\n", 
				key->key, (char *) key->data, key->comment);
		
			
			pos = 1;
			break;
		}
	}
	if (pos != 1) {	// key not found, leave it, so that app won't sigfault
		errno = KDB_RET_NOKEY;
		pos = -1;
		fprintf (stderr, "Key not found!\n");
	} else if (pos == 1) { // key found, everything went ok!
		pos = 0;
	}
	
	close_file ();
	
	free (keyFullName);
	free (keyRoot);
	
	return pos; /* success */
}


/**
 * Get out all the keys of a file
 * 
 * @param keyFileName: Name of the file
 * @param keyRoot: Name of the root of files
 *  The root will be added before the keyName
 * 
 * @ingroup ini
 */
ssize_t kdbGetKeys (char * keyFileName, char * keyRoot, KeySet * returned)
{
	Key * key;
	int pos;

	open_file (keyFileName, O_RDONLY);
	key = keyNew (KEY_SWITCH_END);

	fprintf (stderr, "Call IniGetKey(%s)\n", keyRoot);
	while ((pos=IniGetKey (key, keyRoot)) == 0)
	{
		fprintf (stderr, "Append key\n");
		ksAppend (returned,key);

		key = keyNew (KEY_SWITCH_END);
	}
	
	keyDel (key); // delete the not used key left

	close_file();
	
	return 0; /* success */
}

/**
 * Reads the contents of the whole file which Key
 * points to.
 *
 * The Keys will be added in the Keyset.
 *
 * @ingroup ini
 */
int IniReadFile (Key * key, KeySet * returned, unsigned long options)
{
	char filename [MAX_PATH_LENGTH];
	char * keyname;
	size_t keyLength;
	int ret;

	fprintf (stderr, "IniReadfile\n");
	
	IniGetFileName(key, filename);

	keyLength = keyGetNameSize (key);
	keyname = malloc (keyLength);
	keyGetName (key, keyname, keyGetNameSize (key));

	fprintf (stderr, "Call kdbGetKeys(filename: %s, keyRoot: %s,returned)\n", 
		filename, keyname);
	kdbGetKeys (filename, keyname, returned);
	
	fprintf (stderr, "after kdbGetKeys\n");
	if (keyLength >0) free (keyname);
	fprintf (stderr, "after free\n");

	return 0;
}

/**
 * Reads all Keys of a directory.
 *
 * For every key the mapper IniChooseFile
 * will be called.
 * 
 * @ingroup ini
 * */
int IniReadDir(Key * key, KeySet * returned, unsigned long options)
{
	char pathName [MAX_PATH_LENGTH];
	char keyname [MAX_PATH_LENGTH];
	char keypath [MAX_PATH_LENGTH];
	DIR * dir;
	char * p;
	struct dirent * filename;
	int ret;
	
	fprintf (stderr, "IniReadDir\n");
	IniGetFileName (key, pathName);
	dir = opendir (pathName);
	if (dir == NULL) {
		fprintf (stderr, "Could not open directory %s\n", pathName);
		return -1;
	}
	
	keyGetName (key, keypath, MAX_PATH_LENGTH);
	
	while ((filename = readdir (dir)))
	{
		if (	strcmp(filename->d_name, ".")  == 0 || 
			strcmp(filename->d_name, "..") == 0)
			continue;
		
		if (filename->d_name[0] == '.' && !(options & KDB_O_INACTIVE))
			continue;

		fprintf (stderr, "Next entry filename: %s\n", filename->d_name);
		strncpy(keyname, keypath, MAX_PATH_LENGTH);
		strcat(keyname, filename->d_name);
		keySetName (key, keyname);
		
		fprintf (stderr, "New keyname: %s\n", keyname);
		ret = IniChooseFile (key, returned, options);
	}

	closedir (dir);

	return ret;
}

/**
 * This mapper chooses between the different
 * styles of files to start the correct function.
 *
 * For files it starts IniReadFile
 * For directorys it starts IniReadDir
 * TODO: Links, Subdirs
 *
 * @ingroup ini
 * */
int IniChooseFile(Key * key, KeySet * returned, unsigned long options)
{
	struct stat buf;
	char filename [MAX_PATH_LENGTH];
	char * keyname;
	size_t keylength;
	
	IniGetFileName(key, filename);
	
	stat (filename, &buf);
	//TODO fill stat info into the key

	fprintf (stderr, "IniChooseFile, pathName: %s\n", filename);
	
	if (S_ISDIR(buf.st_mode))
	{
		printf ("	next recursive step\n");
		if (filename[strlen(filename)-1] != '/')
		{
			keylength = keyGetNameSize(key);
			keyname = malloc (keylength + 2);
			keyGetName (key, keyname, keylength);
			keyname[keylength-1] = '/';
			keyname[keylength] = '\0';
			keySetName (key, keyname);
			fprintf (stderr, "add /: %s\n", keyname);
			free (keyname);
		}
			
		return IniReadDir (key, returned, options);
		printf("	return\n");
	}

	if (S_ISREG (buf.st_mode))
	{
		printf ("	will read file\n");
		return IniReadFile (key, returned, options);
		printf("	return\n");
	}

	fprintf (stderr, "Not a directory or file!");
	return -1;
}

/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * 
 * @ingroup ini
 */
ssize_t kdbGetKeyChildKeys_ini(const Key * key, KeySet *returned, unsigned long options)
{
	char t [MAX_PATH_LENGTH];
	Key * write;
	keyDup (key, write);
	
	IniGetFileName(write, t);
	fprintf (stderr, "IniGetFileName: %s\n",	t);
	IniGetBaseName(write, t);
	fprintf (stderr, "IniGetBaseName: %s\n",	t);
	//Immediately call IniChooseFile

	return IniChooseFile (write, returned, options);
}

/**
 * Writes out a key into file on pos.
 * keySet is the key which should be written there
 *
 * @ret Returnes 0 on success.
 * 
 * @ingroup ini
 */
int IniWriteKey (Key * setKey, long oldpos)
{
	long newpos;
	char * end;
	long needed_size;

	fprintf (stderr, "IniWriteKey\n");
	// use setkey to set the key to wished values
	newpos = ftell (fc);
	end = strrchr (keyStealName (setKey),'/')+1;
	needed_size = strlen (end) +// =
		keyGetDataSize (setKey) +	// ;
		keyGetCommentSize (setKey) + 1;	// \n
	if (newpos - oldpos > needed_size)
	{
		fprintf (stderr, "Shrinking File with %ld bytes\n",
				newpos - oldpos - needed_size);
		shrink_file (oldpos, newpos - oldpos -needed_size);
	} else if (newpos - oldpos < needed_size) {
		fprintf (stderr, "Enlarge File with %ld bytes\n",
				needed_size - (newpos - oldpos));
		enlarge_file (newpos, needed_size - (newpos - oldpos));
	}
	
	fprintf(stderr, "Writing key to disc (pos: %ld|%ld|%d) ...\n",
		oldpos, newpos, needed_size);
	fseek (fc, oldpos, SEEK_SET);
	
	fwrite (end, strlen(end), 1, fc);
	fwrite ("=", 1,1,fc);
	if (keyStealValue (setKey) != NULL)
		fwrite (keyStealValue (setKey), keyGetValueSize(setKey)-1, 1, fc);
	fwrite (";", 1,1,fc);
	if (keyStealComment (setKey) != NULL)
		fwrite (keyStealComment (setKey), keyGetCommentSize(setKey)-1, 1, fc);
	fwrite ("\n", 1,1,fc);

	newpos = ftell (fc);
	fprintf (stderr, "Real endpos: %ld\n", newpos);

	fprintf (stderr, "key: %s, value: %s, comment: %s\n", 
		setKey->key, (char *) setKey->data, setKey->comment);

			
	return 0;
}



/**
 * Implementation for kdbSetKey() method.
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup ini
 */
int kdbSetKey_ini(Key *origkey) {
	char keyFileName [MAX_PATH_LENGTH];
	
	int pos;
	int keySize;
	char * keyFullName;
	char * keyRoot;
	char * end;

	Key psetKey;
	Key pkey;
	Key * setKey = &psetKey;
	Key * key = &pkey;

	long oldpos, newpos;
	int needed_size;

	keyInit (setKey);
	keyInit (key);
	fprintf (stderr, "before dup");
	keyDup (origkey, setKey); // for writing
	fprintf (stderr, "during dup");
	keyDup (origkey, key);	// for searching
	fprintf (stderr, "after dup");
	
	pos = IniSearchFileName(key, keyFileName);
	
	fprintf (stderr, "kdbSetKey_ini(%s) entered\n", keyFileName);
	
	keySize = keyGetNameSize (key);
	keyFullName = malloc (keySize+1);
	keyGetName(key, keyFullName, keySize);
	
	end = strrchr (keyFullName, '/');	// dirname
	*end = 0;
	keyRoot = malloc (strlen (keyFullName));
	strcpy (keyRoot, keyFullName);
	*end = '/';
	
	fprintf (stderr, "keyRoot: %s\n", keyRoot);

	if (! pos) {
		fprintf (stderr, "Could not receive filename");
		return -1;
	}
	fprintf (stderr, "Set Key [%d] in File: %s\n",keySize, keyFileName);

	if (open_file (keyFileName, O_RDWR) < 0) return -1;
	
	while ((pos=IniGetKey (key, keyRoot)) == 0)
	{
		if (strcmp (key->key, keyFullName) == 0) 
		{	// right Key found
			fprintf (stderr, "Key found\n");
			fprintf(stderr, "Name: (%s), Value: (%s), Comment: (%s)\n",
				keyStealName (setKey), (char *) keyStealValue(setKey),
				(char *) keyStealComment (setKey));
			//switch to next key
			if (key->key) free(key->key);
			key->key = malloc (keySize+1);
			strncpy (key->key, keyFullName, keySize);

			IniWriteKey(setKey, oldpos);
			pos = 1;
			break;
		}
		oldpos = ftell (fc);
	}
	if (pos != 1) {	// key not found, leave it, so that app won't sigfault
		errno = KDB_RET_NOKEY;
		pos = -1;
		fprintf (stderr, "Key not found!\n");
	} else if (pos == 1) { // key found, everything went ok!
		pos = 0;
	}
	
	close_file ();
	
	free (keyFullName);
	free (keyRoot);
	
	return pos; /* success */
}


/**
 * Implementation for kdbRename() method.
 *
 * @see kdbRename() for expected behavior.
 * @ingroup ini
 */
int kdbRename_ini(Key *key, const char *newName) {
	fprintf (stderr, "Give Key a new Name\n");
	return 0; /* success */
}

/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup ini
 */
int kdbRemoveKey_ini(const Key *key) {
	fprintf (stderr, "Remove Key from Database\n");
	return 0;  /* success */
}




/**
 * IniStatKey stats the filename filename and write
 * the information in the key.
 *
 * So its possible to stat the real filename, without
 * changing the keyname (which is normally another
 * name then the filename).
 *
 * @param filename will be stated
 * @param key will get the information about filename
 *
 * @return 0 on success, -1 otherwise
 * 
 * @ingroup ini
 */
int IniStatFile (Key * key, char * filename)
{	
	struct stat buf;
	stat (filename, &buf);
	
	keySetAccess(key,buf.st_mode);
        keySetUID(key,buf.st_uid);
        keySetGID(key,buf.st_gid);
        if (S_ISDIR(buf.st_mode)) keySetType(key,KEY_TYPE_DIR);
        else if (S_ISREG (buf.st_mode)) keySetType(key, KEY_TYPE_FILE);
        else if (S_ISLNK (buf.st_mode)) keySetType(key, KEY_TYPE_LINK);
        key->atime=buf.st_atime;
        key->mtime=buf.st_mtime;
        key->ctime=buf.st_ctime;
        key->recordSize=buf.st_size;

	return 0;
}

/**
 * Implementation for kdbStatKey() method.
 * 
 * Trys to stat the file and fill the key
 * with information about that. keys inside
 * a file get the stat information of their
 * file.
 *
 * @ingroup ini
 */
int kdbStatKey_ini(Key *key) {
	char filename [MAX_PATH_LENGTH];
	
	IniGetFileName(key, filename);
	IniStatFile (key, filename);
	
	fprintf (stderr, "kdbStatKey, filename: %s\n", filename);
	return 0; /* success */
}



/**
 * Implementation for kdbSetKeys() method.
 * 
 * The implementation of this method is optional, and a builtin, probablly 
 * inefficient implementation can be explicitly used when exporting the
 * backend with kdbBackendExport(), using kdbSetKeys_default().
 * 
 * @see kdbSetKeys() for expected behavior.
 * @ingroup ini
 */
int kdbSetKeys_ini(KeySet *ks) {
	//TODO performance increase for kdbSetKeys
	fprintf (stderr, "Set many Keys at once\n");	
	return 0;
}


/**
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for each
 * key inside @p interests.
 *
 * @see kdbMonitorKeys() for expected behavior.
 * @ingroup ini
 */
u_int32_t kdbMonitorKeys_ini(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	fprintf (stderr, "Monitor many Keys at once\n");
	return 0;
}



/**
 *
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for
 * @p interest.
 *
 * @see kdbMonitorKey() for expected behavior.
 * @ingroup ini
 */
u_int32_t kdbMonitorKey_ini(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	fprintf (stderr, "Monitor a key\n");
	return 0;
}


/**
 * All KeyDB methods implemented by the backend can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the backend, and the first method of the backend
 * implementation that will be called.
 * 
 * Its purpose is to "publish" the exported methods for libelektra.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 * 
 * @return whatever kdbBackendExport() returns
 * @see kdbBackendExport() for an example
 * @see kdbOpenBackend()
 * @ingroup ini
 */
KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_ini,
		KDB_BE_CLOSE,          &kdbClose_ini,
		KDB_BE_GETKEY,         &kdbGetKey_ini,
		KDB_BE_SETKEY,         &kdbSetKey_ini,
		KDB_BE_STATKEY,        &kdbStatKey_ini,
		KDB_BE_RENAME,         &kdbRename_ini,
		KDB_BE_REMOVEKEY,      &kdbRemoveKey_ini,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_ini,
		KDB_BE_MONITORKEY,     &kdbMonitorKey_ini,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_ini,
		/* set to default implementation: */
		KDB_BE_SETKEYS,        &kdbSetKeys_default,
		KDB_BE_END);
}

#warning "Backend is not full featured"

