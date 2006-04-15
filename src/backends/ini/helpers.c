/***************************************************************************
            helpers.c  -  System depended helper functions (POSIX)
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


#include <ini.h>

#define _POSIX_SOURCE 1

/**POSIX include files*/
#include <unistd.h>
#include <pwd.h>
#include <fcntl.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>

#include <sys/stat.h>
#include <sys/file.h>
#include <sys/types.h>

/**Posix style file descriptor*/
int fd;

/**Reallocate Storage in a save way
 * @code
if (srealloc ((void **) & buffer, new_length) < 0) {
	// here comes the failure handler
	fprintf (stderr, "Reallocation error\n");
	free (buffer);
	//free the buffer
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
		*buffer = svr;	/* restore old buffer*/
		return -1;
	} else {
		*buffer = ptr;
		return 0;
	}
}



/**
 * Opens a file filename.
 * The mode might be 'r' or 'w'
 *  (will be mapped to O_RDONLY or O_RDWR.)
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
int open_file (char * filename, char mode)
{
	char buffer [2] = "\0\0";
	int ret = 0;
	int posix_mode;
	
	if (mode == 'w')
	{
		buffer[0] = 'r';
		buffer[1] = '+';
		posix_mode = O_RDWR;
	}
	else if (mode == 'r')
	{
		buffer[0] = 'r';
		posix_mode = O_RDONLY;
	} else {
		fprintf (stderr, "Mode not useful\n");
		return -1;
	}

	fd = open (filename, posix_mode);
	if (fd == -1) {
#ifdef DEBUG
		fprintf (stderr, "Will create new file\n");
#endif
		fd = open (filename, posix_mode | O_CREAT);
		if (fd == -1)
		{
			fprintf (stderr, "Unable to open file\n");
			perror ("Reason: ");
			return -1;
		}
		ret = fchmod (fd, 0644);	/* TODO: permissions!*/
		if (ret == -1)
		{
			fprintf (stderr, "Unable to chmod file\n");
			perror ("Reason: ");
		}
	}
	
	if (flock (fd, LOCK_EX) == -1) {
		fprintf (stderr, "Unable to lock file\n");
		perror ("Reason: ");
		ret = -1;
	}
		
	
	fc = (FILE *) fdopen (fd,buffer);
	if (fc == NULL) {
		fprintf (stderr, "fdopen() failed\n");
		perror ("Reason: ");
		ret = -2;
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

	ret = fclose (fc);
	if (ret != 0) {
		perror ("Could not close file");
		ret = -2;
	}
	return ret;
}

/**
 * stat_file stats the filename filename and write
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
int stat_file (Key * key, char * filename)
{	
	struct stat buf;
	stat (filename, &buf);
	
	keySetAccess(key,buf.st_mode);
        keySetUID(key,buf.st_uid);
        keySetGID(key,buf.st_gid);
        if 	(S_ISDIR (buf.st_mode))	keySetType(key, KEY_TYPE_DIR);
        else if (S_ISREG (buf.st_mode)) keySetType(key, KEY_TYPE_FILE);
        else if (S_ISLNK (buf.st_mode)) keySetType(key, KEY_TYPE_LINK);
        key->atime=buf.st_atime;
        key->mtime=buf.st_mtime;
        key->ctime=buf.st_ctime;
        key->recordSize=buf.st_size;

	return 0;
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

	fseek (fc,0,SEEK_END); /* begin at end*/
	pos = ftell (fc);
	do {
		pos -= BUFFER_RDWR_SIZE;
		if (pos < where) {
			diff = where - pos;
			pos = where;
			finished = 1;
		}
		fseek (fc, pos, SEEK_SET);
		sread = fread (buffer,1,BUFFER_RDWR_SIZE-diff,fc);	/* read last peace*/
		buffer[sread] = 0;	/* mark end (not necessary)*/

		fseek (fc,pos+space,SEEK_SET);	/* jump to writepos*/

#ifdef DEBUG
		printf ("buffer: %s, sread: %d\n", buffer, sread);
#endif
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
		fseek (fc,pos+space,SEEK_SET); /* jump to readposition*/
		sread = fread (buffer,1,BUFFER_RDWR_SIZE,fc);	/* read a peace*/
		buffer[sread] = 0;	/* mark end (not necessary)*/

		fseek (fc,pos,SEEK_SET);	/* jump to writepos*/
#ifdef DEBUG
		printf ("buffer: %s, sread: %d\n", buffer, sread);
#endif
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
 * @see file_name
 * 
 * @ingroup ini
 */
size_t base_name (const Key * forKey, char * basename)
{
	size_t length;

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
size_t file_name (const Key * forKey, char * filename)
{
	size_t length;
	size_t namesize;
	char * name;

	length = base_name (forKey, filename);

        filename[length]='/'; length++; /* now the Keyname follows*/

	namesize = keyGetNameSize (forKey);
	if (namesize == 0) return 0;
	name = (char*) malloc (namesize);

	keyGetName (forKey, name, namesize);

	if (length > MAX_PATH_LENGTH) return -1;	/* too long*/

	strncpy (filename + length, name, namesize);
	length += namesize;
	free (name);

	return length;
}

/**Opens a directory and returns a pointer to a
 * platform depended unique struct which indentifies
 * the opened directory.
 * With the pointer you can get the filenames with
 * repeated calling from read_dir.
 * @see read_dir
 * @return pointer to struct (DIR), or NULL if failed*/
void * open_dir (char * pathname)
{
	return (void *) opendir (pathname);
}

/**Creates directories recursively up to the filename
 * @return 0 on success, -1 on failure*/
int create_dir (char * keyFileName)
{
	char * end;
	char * fil;

#ifdef DEBUG
	fprintf (stderr, "will create_dir() for %s\n", keyFileName);
#endif
	end = strrchr(keyFileName, '/'); /* key abschneiden*/
	*end = 0;
	fil = strrchr(keyFileName, '/'); /* das hier wird file sein*/
	end = keyFileName +1;	/* fange suche nach dir an*/
	while (1) {
		end = strchr(end+1, '/');
		if (end == NULL) break;
		* end = '\0';
#ifdef DEBUG
		fprintf (stderr, "Create Folder %s\n", keyFileName);
#endif
		if (mkdir (keyFileName, 0777) == -1)
		{
			if (errno == EEXIST)
			{
#ifdef DEBUG
				/**TODO Check if it is dir. Make more robust with stat*/
				fprintf (stderr, "Directory already exists\n");
#endif
			} else {
				fprintf (stderr, "Could not create Folder %s\n", keyFileName);
				perror ("Reason: ");
				return -1;
			}
		}
		* end = '/';			
	}
	return 0;
}


/**Reads the next filename out of dir.
 * precondition:
 * filename size is MAX_PATH_LENGTH and guaranted to be allocated.
 * @return -1 if no more files, 0 on success, -1 on failure*/
int read_dir (void * dir, char * filename)
{
	struct dirent * n;
	n= readdir (dir);
	strncpy (filename, n->d_name, MAX_PATH_LENGTH);
	if (n == NULL) return -1; /*No more files*/
	return 0;
}

/**Closes previously opened dir
 * Use pointer from open_dir
 * @return 0 on success, -1 on failure*/
int close_dir (void * dir)
{
	return close_dir ((DIR *) dir);
}

