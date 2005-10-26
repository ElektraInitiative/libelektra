/***************************************************************************
                          kdb.c  -  Tool for the kdb administration
                             -------------------
    begin                : Mon Mar 02 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id$
$LastChangedBy: aviram $

*/





#include "kdb.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <time.h>
#include <locale.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <grp.h>
#include <pwd.h>
#include <ctype.h>
#include <ltdl.h>
#include <assert.h>


#define CMD_GET       1
#define CMD_SET       2
#define CMD_REMOVE    3
#define CMD_LIST      4
#define CMD_LINK      5
#define CMD_EDIT      6
#define CMD_LOAD      7
#define CMD_SAVE      8
#define CMD_MONITOR   9
#define CMD_MOVE      10
#define CMD_HELP      30

#define ARGSIZE      30


/* we are cheating . . . */
ssize_t unencode(char *encoded, void *returned);


/* We'll load this methods dynamically to avoid libxml dependencies */
int (*ksFromXMLfile)(KeySet *ks,char *filename);
int (*ksFromXML)(KeySet *ks,int fd);



/**
 * @defgroup libexample  The kdb Command Source Code: Example of Full Library Utilization
 * @{
 */

int commandHelp();

char *argComment=0;
char *argFile=0;
char *argData=0;
char *argKeyName=0;
char *argDomain=0;
uid_t *argUID=0;
uid_t *argGID=0;
int argCommand=0;
int argRecursive=0;
int argLong=0;
int argValue=0;
int argAll=0;
int argSort=1;
int argDescriptive=0;
int argFullName=0;
int argShow=1;
int argShell=0;
int argXML=0;
mode_t argMode=0;
int argType=KEY_TYPE_UNDEFINED;


int parseCommandLine(int argc, char *argv[]) {
	char sargType[ARGSIZE],argUser[ARGSIZE],argGroup[ARGSIZE];
	char sargMode[ARGSIZE],sargCommand[ARGSIZE];
	char * keyEnv;
	size_t keyEnvLength=0, keyOptLength=0, keyOldLength;

	int opt;

	*sargType=*argUser=*argGroup=*sargCommand=*sargMode=0;

	while ((opt=getopt(argc,argv,"ab:c:dfg:hilm:nrRst:u:vx"))!=-1)
	{
		if (opt == EOF)
			break;
		switch (opt)
		{
		case 'a':
			argAll=1;
			break;
		case 'b':
			argFile=realloc(argFile,strlen(optarg)+1);
			assert(argFile!=NULL);
			strcpy(argFile,optarg);
			break;
		case 'c':
			argComment=realloc(argComment,strlen(optarg)+1);
			assert(argComment!=NULL);
			strcpy(argComment,optarg);
			break;
		case 'd':
			argDescriptive=1;
			argLong=1;
			break;
		case 'f':
			argFullName=1;
			break;
		case 'g':
			strncpy(argGroup,optarg,ARGSIZE);
			break;
		case 'h':
			commandHelp();
			break;
		case 'i':
			argShow=0;
			break;
		case 'l':
			argLong=1;
			break;
		case 'm':
			strncpy(sargMode,optarg,ARGSIZE);
			break;
		case 'n':
			argSort=0;
			break;
		case 'R':
		case 'r':
			argRecursive=KDB_O_RECURSIVE;
			break;
		case 's':
			argShell=1;
			break;
		case 't':
			strncpy(sargType,optarg,ARGSIZE);
			break;
		case 'u':
			strncpy(argUser,optarg,ARGSIZE);
			break;
		case 'v':
			argValue=1;
			break;
		case 'x':
			argXML=1;
			break;
		default:
			fprintf(stderr, "Unknown error (%d %c) in parsing arguments\n",
					opt,opt);
			break;
		}
	}

	if (optind < argc) /*parse command*/
	{
		strncpy(sargCommand,argv[optind],ARGSIZE);
		optind ++;
	} else {
		fprintf (stderr, "No command given\n");
		exit (1);
	}
		
	if (optind < argc) /*parse key name*/
	{
		/**Use KDB_ROOT as prefix in key name*/
		keyEnv = getenv ("KDB_ROOT");
		if (keyEnv) keyEnvLength = strblen (keyEnv);
		else keyEnvLength = 0;
		keyOptLength = strblen (argv[optind]);
		argKeyName=realloc(argKeyName,
			keyEnvLength + keyOptLength + 1);
		assert(argKeyName!=NULL);
		if (keyEnv) strncpy (argKeyName, keyEnv,   keyEnvLength);
		strncpy(argKeyName + keyEnvLength, argv[optind], keyOptLength);
		if (keyEnv) *(argKeyName+keyEnvLength-1) = '/';
		optind ++;
	}

	keyOptLength = 0;
	keyOldLength = 0;
	while (optind < argc) /* parse value (rest of arguments) */
	{
		keyOptLength += strblen(argv[optind]);
		argData=realloc(argData,keyOptLength + 1);
		assert(argData!=NULL);
		if (keyOldLength > 0) *(argData+keyOldLength-1) = ' ';
		strcpy(argData+keyOldLength, argv[optind]);
		optind ++;
		keyOldLength = keyOptLength;
	}
	
	/* see if parsing worked:
	fprintf (stderr, "command: %s\n", sargCommand);
	fprintf (stderr, "key name: %s\n", argKeyName);
	fprintf (stderr, "value: %s\n", argData);
	exit (0);
	*/
		
	/* End of command line argument reading. Now parse and finalize */

	/* Check parsed command */
	if (!strcmp(sargCommand,"ls")) argCommand=CMD_LIST;
	else if (!strcmp(sargCommand,"set")) argCommand=CMD_SET;
	else if (!strcmp(sargCommand,"get")) argCommand=CMD_GET;
	else if (!strcmp(sargCommand,"ln")) argCommand=CMD_LINK;
	else if (!strcmp(sargCommand,"rm")) argCommand=CMD_REMOVE;
	else if (!strcmp(sargCommand,"vi")) argCommand=CMD_EDIT;
	else if (!strcmp(sargCommand,"edit")) argCommand=CMD_EDIT;
	else if (!strcmp(sargCommand,"load")) argCommand=CMD_LOAD;
	else if (!strcmp(sargCommand,"import")) argCommand=CMD_LOAD;
	else if (!strcmp(sargCommand,"save")) argCommand=CMD_SAVE;
	else if (!strcmp(sargCommand,"export")) argCommand=CMD_SAVE;
	else if (!strcmp(sargCommand,"mon")) argCommand=CMD_MONITOR;
	else if (!strcmp(sargCommand,"monitor")) argCommand=CMD_MONITOR;
	else if (!strcmp(sargCommand,"mv")) argCommand=CMD_MOVE;
	else if (!strcmp(sargCommand,"help")) argCommand=CMD_HELP;
	else {
		fprintf(stderr,"kdb: Invalid subcommand.\n");
		exit(1);
	}

	/* Parse type */
	if (*sargType!=0) {
		/* TODO: use regex */
		if      (!strcmp(sargType,"string")) argType=KEY_TYPE_STRING;
		else if (!strcmp(sargType,"bin"))    argType=KEY_TYPE_BINARY;
		else if (!strcmp(sargType,"binary")) argType=KEY_TYPE_BINARY;
		else if (!strcmp(sargType,"dir"))    argType=KEY_TYPE_DIR;
		else if (!strcmp(sargType,"link"))   argType=KEY_TYPE_LINK;
		else {
			argType=strtol(sargType,0,10);
			if (errno == ERANGE || errno == EINVAL)
				/* handle undefined later */
				argType=KEY_TYPE_UNDEFINED;
		}
	} else if (argCommand==CMD_SET) { /* We must have a type */
		argType=KEY_TYPE_STRING;
	}


	/* Parse UID */
	if (*argUser) {
		if (isdigit(*argUser)) {
			argUID=malloc(sizeof(uid_t));
			*argUID=atoi(argUser);
		} else {
			struct passwd *pwd;
			pwd=getpwnam(argUser);
			if (pwd) {
				argUID=malloc(sizeof(uid_t));
				*argUID=pwd->pw_uid;
			} else {
				fprintf(stderr,"kdb: Invalid user \'%s\'. Ignoring\n", argUser);
			}
		}
	}


	/* Parse GID */
	if (*argGroup) {
		if (isdigit(*argGroup)) {
			argGID=malloc(sizeof(gid_t));
			*argGID=atoi(argGroup);
		} else {
			struct group *grp;
			grp=getgrnam(argGroup);
			if (grp) {
				argGID=malloc(sizeof(gid_t));
				*argGID=grp->gr_gid;
			} else {
				fprintf(stderr,"kdb: Invalid group \'%s\'. Ignoring\n",argGroup);
			}
		}
	}



	/* Parse permissions */
	if (*sargMode!=0) argMode=strtol(sargMode,0,8);

	return argCommand;
}






/*
 * Helper for the 'kdb ls' command
 *
 */
void listAccess(Key *key,char *readable) {
	mode_t mode=keyGetAccess(key);

	if (S_ISDIR(mode)) readable[0]='d';
	else if (S_ISLNK(mode)) readable[0]='l';
	else readable[0]='-';

	readable[1] = mode & S_IRUSR ? 'r' : '-';
	readable[2] = mode & S_IWUSR ? 'w' : '-';
	readable[3] = mode & S_IXUSR ? 'x' : '-';
	readable[4] = mode & S_IRGRP ? 'r' : '-';
	readable[5] = mode & S_IWGRP ? 'w' : '-';
	readable[6] = mode & S_IXGRP ? 'x' : '-';
	readable[7] = mode & S_IROTH ? 'r' : '-';
	readable[8] = mode & S_IWOTH ? 'w' : '-';
	readable[9] = mode & S_IXOTH ? 'x' : '-';
	readable[10]= 0;
}




/*
 * Helper for the 'kdb ls' command
 *
 */
void listTime(time_t when,char *readable) {
	time_t current_time=time(0);
	char buf[400];
	time_t six_months_ago;
	int recent;

	/* If the file appears to be in the future, update the current
	   time, in case the file happens to have been modified since
	   the last time we checked the clock.  */

	/* Consider a time to be recent if it is within the past six
	   months.  A Gregorian year has 365.2425 * 24 * 60 * 60 ==
	   31556952 seconds on the average.  Write this value as an
	   integer constant to avoid floating point hassles.  */
	six_months_ago = current_time - 31556952 / 2;
	recent = (six_months_ago <= when) && (when <= current_time);

	ctime_r(&when,buf); /* buf will become "Wed Jun 30 21:49:08 1993\n" */
	memcpy(readable,buf+4,7); /* take only month and day */
	if (recent) {
		memcpy(readable,buf+4,12);
		readable[12]=0;
	} else {
		memcpy(readable,buf+4,7);
		readable[7]=' ';
		memcpy(readable+8,buf+20,4);
		readable[12]=0;
	}
}



/*
 * Helper for the 'kdb ls' command
 *
 */
void listSingleKey(Key *key) {
	char buffer[400];
	char *p=buffer;

	if (argLong) {
		struct passwd *pwd;
		struct group *grp;

		listAccess(key,p);
		p+=strlen(p);
		*p=' '; p++;
		*p=' '; p++;
		*p=' '; p++;

		pwd=getpwuid(keyGetUID(key));
		strcpy(p,pwd->pw_name);
		p+=strlen(p);
		*p=' '; p++;
		*p=' '; p++;

		grp=getgrgid(keyGetGID(key));
		strcpy(p,grp->gr_name);
		p+=strlen(p);
		*p=' '; p++;

		sprintf(p,"%*d ",5,keyGetRecordSize(key));
		p+=strlen(p);

		listTime(keyGetMTime(key),p);
		p+=strlen(p);
		*p=' '; p++;
	}
	if (argFullName) keyGetFullName(key,p,sizeof(buffer)-(p-buffer));
	else keyGetName(key,p,sizeof(buffer)-(p-buffer));
	if (argValue && (keyGetValueSize(key)>0)) {
		uint8_t ktype;

		p+=strlen(p);
		*p='='; p++;

		ktype=keyGetType(key);
		if (ktype >= KEY_TYPE_STRING)
			p+=keyGetString(key,p,sizeof(buffer)-(p-buffer));
		else if (ktype >= KEY_TYPE_BINARY)
			p+=sprintf(p,"<BINARY VALUE>");
		else if (ktype == KEY_TYPE_LINK)
			p+=keyGetLink(key,p,sizeof(buffer)-(p-buffer));

		*p=0;
	}
	puts(buffer);
}








/**
 * The business logic behind 'kdb rm' command
 * @par Example:
 * @code
 * bash$ kdb rm user/env/alias/ls   # get rid to the ls alias
 * @endcode
 *
 * @see kdbRemove()
 * @param argKeyName name of the key that will be removed
 */
int commandRemove() {
	if (!argKeyName) {
		fprintf(stderr,"kdb rm: No key name\n");
		return -1;
	}

	if (kdbRemove(argKeyName)) {
		char error[300];
		
		sprintf(error,"kdb rm: \'%s\'",argKeyName);
		perror(error);
		return -1;
	}
	return 0;
}



/**
 * The business logic behind 'kdb mv' command.
 * The central method used is kdbRename() but this function is
 * way more robust, and is an example on how to handle errors.
 * @par Example:
 * @code
 * bash# kdb mv user/env  user:tatiana/env
 * @endcode
 *
 * @see kdbRename()
 * @param argKeyName name of the source key
 * @param argData name of the target key
 */
int commandMove() {
	Key *key;
	size_t size=0;
	int rc;
	
	key=keyNew(argKeyName,KEY_SWITCH_END);
	size=keyGetNameSize(key);
	
	if (size == 0) {
		char error[100];
		
		sprintf(error,"kdb mv: \'%s\'", argKeyName);
		perror(error);
		
		keyDel(key);
		return 1;
	}
	
	rc=kdbRename(key,argData);
	if (rc != 0) {
		/* Handle a non-zero rc, with same behavior of Unix mv command */
		switch (errno) {
			
		}
	}
	
	keyDel(key);
	
	return rc;
}



/**
 * The business logic behind 'kdb set' command.
 * Sets value to a single key.
 *
 * @par Example:
 * @code
 * bash$ kdb set -c "My shell prompt" user/env/env1/PS1 '\h:\w\$'
 * @endcode
 *
 * @param argKeyName name of the key that will be set
 * @param argComment comment to be set to key (-c)
 * @param argType type of the key (-t)
 * @param argMode access permissions that will be set to sey (-m)
 * @param argUID UID to be set to sey
 * @param argGID GID to be set to sey
 * @param argData the value to the key
 * @param argFile a filename to use as the input for the value
 * @see kdbSetKey()
 */
int commandSet() {
	Key *key;
	int ret;
	char error[200];
	size_t offset=0;


	/* Consistency */
	if (!argKeyName) {
		fprintf(stderr,"kdb set: No key name\n");
		return -1;
	}

	key=keyNew(argKeyName,KEY_SWITCH_END);
	ret=kdbGetKey(key);
	if (ret == 0) { /* Key already exists. Good. */
		/* Use existed key type if user didn't give us one */
		if (argType==KEY_TYPE_UNDEFINED) argType=keyGetType(key);
	} else if (errno!=KDB_RET_NOTFOUND) {
		/* Handle errors different from NOTFOUND */
		sprintf(error,"kdb set: %s",argKeyName);
		perror(error);
	}

	/* Set or overwrite everything else... */
	
	if (argUID) keySetUID(key,*argUID);
	if (argGID) keySetGID(key,*argGID);
	if (argMode) keySetAccess(key,argMode);

	if (argComment) keySetComment(key,argComment);
	
	if (argFile) {
		FILE *f;
		int end=0;
		
		if (argData) free(argData);
		argData=0;
		f=fopen(argFile,"r");
		
		if (!f) {
			sprintf(error,"kdb set: \'%s\'",argFile);
			perror(error);
 			return -1;
		}
		while (! end) {
			char buffer[100];
			ssize_t r;
			
			r=read(fileno(f),buffer,sizeof(buffer));
			if (r == 0) {
				r=lseek(fileno(f),0,SEEK_END)-offset;
				end=1;
			}
			argData=realloc(argData,offset+r);
			assert(argData!=NULL);
			memcpy(argData+offset,buffer,r);
			offset+=r;
		}
		fclose(f);
	}


	/* Set key value . . . */
	if (argType == KEY_TYPE_UNDEFINED)
		keySetString(key,argData); /* the most common here */
	else if (argType == KEY_TYPE_DIR)
		keySetType(key,KEY_TYPE_DIR);
	else if (argType == KEY_TYPE_LINK)
		keySetLink(key,argData);
	else if (argData) { /* Handle special type values . . . */
	
		/* set raw data */
		if (offset) keySetRaw(key,argData,offset);
		else if (KEY_TYPE_BINARY <= argType && argType < KEY_TYPE_STRING)
			 /* command-line-passed bin values have unwanted \0 in the end */
			 keySetRaw(key,argData,strblen(argData)-1);
		else keySetRaw(key,argData,strblen(argData));
		
		/* set type explicitly */
		keySetType(key,argType);
	}


	ret=kdbSetKey(key);
	if (ret) {
		sprintf(error,"kdb set: \'%s\'",argKeyName);
		perror(error);
	}
	
	keyDel(key);
	
	return ret;
}






/**
 * The business logic behind 'kdb ln' command
 * 
 * @par Example:
 * @code
 * bash$ kdb ln user:valeria/sw/MyApp user/sw/MyApp  # make my personal MyApp configurations be a link to valerias configs
 * @endcode
 *
 * @param argKeyName name of the target key
 * @param argData name of the link key to be created
 * @see kdbLink(), keySetType()
 */
int commandLink() {
	int rc;

	/* Consistency */
	if (!argKeyName) {
		fprintf(stderr,"kdb ln: No target specified\n");
		return -1;
	}

	if (!argData) {
		fprintf(stderr,"kdb ln: \'%s\': No destination specified",argKeyName);
		return -1;
	}

	if ((rc=kdbLink(argKeyName,argData))) {
		perror("kdb ln");
	}

	return rc;
}














/**
 * The business logic behind 'kdb ls' command.
 * @param argKeyName key name to be listed
 * @param argRecursive whether to act recursivelly (-R)
 * @param argValue whether to show key values or not (-v)
 * @param argAll whether to list also inactive keys (-a)
 * @param argXML whether to create XML output (-x)
 *
 * @par Example:
 * @code
 * bash$ kdb ls -R   # list all keys from system and user trees
 * bash$ kdb ls -Ra  # list them all plus the hidden/inactive keys
 * bash$ kdb ls -Rav # list all showing value
 * bash# kdb ls -Rxv # equivalent to 'kdb export'
 * bash$ kdb ls -Rv user/env # list my aliases and environment vars
 * @endcode
 *
 * @see kdbGetRootKeys(), kdbGetKeyChildKeys(), keyToStream(), ksToStream()
 * @see commandExport() for the 'kdb export' command
 */
int commandList() {
	KeySet *ks; /* this is the container for all keys we'll collect bellow */
	ssize_t ret;

	ks=ksNew();

	if (!argKeyName) {
		KeySet *roots;
		/* User don't want a specific key, so list the root keys */

		roots=ksNew();
		kdbGetRootKeys(roots);

		if (argRecursive) {
			Key *walker=0;
			
			while ((walker=ksPop(roots))) {
				/* walk root by root, retrieve entire subtree
				 * and append it to ks
				 */
				KeySet *thisRoot=ksNew();
				
				if (argValue) ret=kdbGetKeyChildKeys(walker,thisRoot,
					(argSort?KDB_O_SORT:0) | (argRecursive?KDB_O_RECURSIVE:0) |
					KDB_O_DIR | (argAll?KDB_O_INACTIVE:0) | KDB_O_NFOLLOWLINK);
				else ret=kdbGetKeyChildKeys(walker,thisRoot,
					(argSort?KDB_O_SORT:0) | KDB_O_STATONLY |
					(argRecursive?KDB_O_RECURSIVE:0) | KDB_O_DIR |
					(argAll?KDB_O_INACTIVE:0) | KDB_O_NFOLLOWLINK);
				
				/* A hack to transfer a key from a keyset to another.
				 * Don't do this at home.
				 */
				ksAppend(ks,walker);
				ksAppendKeys(ks,thisRoot);
				ksDel(thisRoot); /* we don't need the container anymore */
			}
		} else ksAppendKeys(ks,roots);
		ksDel(roots);
	} else {
		/* User gave us a specific key to start with */

		if (argValue) ret=kdbGetChildKeys(argKeyName,ks,
			(argSort?KDB_O_SORT:0) | (argRecursive?KDB_O_RECURSIVE:0) |
			KDB_O_DIR | (argAll?KDB_O_INACTIVE:0) | KDB_O_NFOLLOWLINK);
		else ret=kdbGetChildKeys(argKeyName,ks,
			(argSort?KDB_O_SORT:0) | KDB_O_STATONLY |
			(argRecursive?KDB_O_RECURSIVE:0) | KDB_O_DIR |
			(argAll?KDB_O_INACTIVE:0) | KDB_O_NFOLLOWLINK);
	
		if (ret<0) {
			/* We got an error. Check if it is because its not a folder key */
			if (errno==ENOTDIR) {
				/* We still have a chance, since there is something there */
				Key *key=keyNew(argKeyName,KEY_SWITCH_END);
				
				if (argValue) ret=kdbGetKey(key);
				else ret=kdbStatKey(key);
				
				if (ret == 0) ksAppend(ks,key);
				else {
					/* There is absolutelly nothing there */
					char error[200];

					keyDel(key);
					ksDel(ks);
					
					sprintf(error,"kdb ls: %s",argKeyName);
					perror(error);
					return ret;
				}
				
			} else { /* A real error */
				char error[200];
				
				ksDel(ks);

				sprintf(error,"kdb ls: %s",argKeyName);
				perror(error);
				return ret;
			}
		}
	}

	if (argShow) {
		size_t listSize=ksGetSize(ks);
		
		if (argXML) ksToStream(ks,stdout,KDB_O_XMLHEADERS |
			(argFullName?(KDB_O_FULLNAME | KDB_O_FULLUGID):0));
		else {
			if (listSize == 1) listSingleKey(ksHead(ks));
			else if (listSize > 1) {
				Key *walker;
			
				ksRewind(ks);
				while ((walker=ksNext(ks)))
					listSingleKey(walker);
			}
		}
	}

	ksClose(ks);
	return 0;
}







/**
 * Business logic behind the 'kdb get' command.
 * Get a key and return its value to you.
 *
 * @par Example:
 * @code
 * bash$ kdb get user/env/alias/ls
 * ls -Fh --color=tty
 * @endcode
 *
 * @param argKeyName key to get value
 * @param argDescriptive show also the key comment (-d)
 * @param argShell output suitable for shell scripts (-s)
 * @param argLong show also the key name (-l)
 * @param argFullName with @p argLong, show the user domain too (-f)
 *
 * @see kdbGetKey(), kdbGetBaseName(), keyGetComment(), keyGetString()
 *
 */
int commandGet() {
	int ret;
	Key *key;
	char *buffer;
	char *p;
	size_t size,cs=0;
	uint8_t keyType;

	if (!argKeyName) {
		fprintf(stderr,"kdb get: No key name\n");
		return -1;
	}

	key=keyNew(argKeyName,KEY_SWITCH_END);
	
	ret=kdbGetKey(key);

	if (ret) {
		char error[200];

		keyDel(key);
		sprintf(error,"kdb get: %s",argKeyName);
		perror(error);
		return ret;
	}
	size=keyGetValueSize(key);
	if (argDescriptive) {
		cs=keyGetCommentSize(key);
		if (cs) size+=cs+3;
	}
	if (argShell) {
		size+=keyGetBaseNameSize(key);
		size+=2; /* for 2 '"' to wrap the value */
	} else if (argLong) {
		if (argFullName) size+=keyGetFullNameSize(key);
		else size+=keyGetNameSize(key);
	}


	p=buffer=malloc(size);


	if (argDescriptive) {
		if (cs) {
			p+=sprintf(p,"# ");
			p+=keyGetComment(key,p,size-(p-buffer));
			*--p='\n'; p++;
		}
	}
	if (argShell) {
		p+=keyGetBaseName(key,p,size-(p-buffer));
		*--p='='; p++;
		*p='\"'; p++;
	} else if (argLong) {
		if (argFullName) p+=keyGetFullName(key,p,size-(p-buffer));
		else p+=keyGetName(key,p,size-(p-buffer));
		*--p='='; p++;
	}
	
	keyType=keyGetType(key);

	if (keyIsBin(key)) p+=keyGetBinary(key,p,size-(p-buffer));
	else p+=keyGetString(key,p,size-(p-buffer));
	if (argShell) {
		*--p='\"'; p++;
		*p=0;
	}
	if (keyIsBin(key)) fwrite(buffer,size,1,stdout);
	else printf("%s\n",buffer);


	free(buffer);
	keyDel(key);

	return 0;
}






/**
 * Prints out help to the kdb (1) command.
 *TODO: should be same as man page
 * help2man?
 * --help does not work
 *
 * @par Example:
 * @code
 * bash$ kdb help
 * bash$ kdb -h
 * @endcode
 */
int commandHelp() {
	printf("Usage: kdb [OPTION] <command> <key> [value ...]\n");
	printf("Use kdb to manipulate the Key Database.\n");
	printf("\n");

	printf("ARGUMENTS\n");
	printf("Commands are explained with kdb -h command (Implemented soon)\n");
	printf("<key> is the name of the key. It can be prefixed\n");
	printf(" with environment KDB_ROOT. The slash between will\n");
	printf("be inserted.\n");
	printf("[value ...] hold the value which should be set\n");
	printf("\n");
	
	printf("EXAMPLES:\n");
	printf(" kdb get [-dlr] key/name\n");
	printf(" kdb set [-t type] [-c \"A comment about this key\"] [-m mode] [-u uid]\n");
	printf("         [-g gid] key/name \"the value\"\n");
	printf(" kdb set [-t type] [-m mode] [-c \"A comment\"] key/name -- \"the value\"\n");
	printf(" kdb set [-t type] [-b file] key/name\n");
	printf(" kdb ls [-lRfv] [key/dir | key/name]\n");
	printf(" kdb ls [-lRfvx] [key/dir | key/name] > keys.xml\n");
	printf(" kdb edit [-R] [key/dir | key/name]\n");
	printf(" kdb rm key/name\n");
	printf(" kdb mv key/src key/dest\n");
	printf(" kdb ln key/src key/dest\n");
	printf(" kdb export system/some/tree.root > file.xml\n");
	printf(" kdb import < file.xml\n");
	printf(" kdb import file.xml\n");
	printf(" kdb monitor some/key/name\n");
	printf(" export KDB_ROOT=\"user/test/dir\"\n");
	printf(" kdb get file/key ... will expand to user/test/dir/file/key\n");
	printf("\n");
	
	printf("SUBCOMMANDS\n");
	printf("get\n");
	printf(" Get the value from the specified key. Accepts options: -d, -l, -f, -s \n");
	printf("\n");
	printf("set\n");
	printf(" Set the value to the specified key. Accepts options: -c, -t, -m, -b \n");
	printf("\n");
	printf("ls\n");
	printf(" As the ls(1) command, list key names for the specified key, or children keys, if specified a folder key. The -v argument will make it show also the values of each key. The -d (descriptive) will make it show the comment, key name and its value, as you are watching a plain text file. Accepts options: -x, -d, -l, -f, -v, -R \n");
	printf("\n");
	printf("ln\n");
	printf(" Creates a key that is a symbolic links to another key. \n");
	printf("\n");
	printf("mv\n");
	printf(" Move, or renames a key. Currently it can't move keys across different filesystems. \n");
	printf("\n");
	printf("rm\n");
	printf(" As the rm(1) command, removes the key specified. \n");
	printf("\n");
	printf("edit\n");
	printf(" A very powerfull subcommand that lets you edit an XML representation of the keys. The parameters it accepts is usually a parent key, so its child keys will be gathered. Can be used with the -R flag to work recursively. The editor used is the one set in the $EDITOR environment variable, or vi. After editing the keys, kdb edit will analyze them and commit only the changed keys, remove the keys removed, and add the keys added. \n");
	printf("\n");
	printf("export, save, \n");
	printf("Export a subtree of keys to XML. If no subtree is defined right after the export command, system and current user trees will be exported. Output is written to standard output. The output encoding will allways be UTF-8, regardeless of your system encoding. UTF-8 is the most universal charset you can get when exchanging data between multiple systems. Accepts -f. \n");
	printf("\n");
	printf("import, load, \n");
	printf("Import an XML representation of keys and save it to the keys database. If no filename is passed right after the import command, standard input is used. \n");
	printf("\n");
	printf("monitor, mon, \n");
	printf("Monitor a key for some value change. It will block your command line until a change in the key value is detected, then return its new value. Options \n");
	printf("-R\n");
	printf(" Causes to work recursively. In ls, will list recursively. \n");
	printf("\n");
	printf("-x\n");
	printf(" Makes ls output an XML representation of the keys, instead of an ls-compatible output. \n");
	printf("\n");
	printf("-l\n");
	printf(" Causes to display long results. With ls, will generate lists similar to ls -l. With get, will show also the key name. \n");
	printf("\n");
	printf("-a\n");
	printf(" Causes ls to display also inactive keys. Generate lists similar to ls -a. Inactive keys are keys which basename begins with a '.' (dot). An example of inactive key: system/sw/XFree/current/Monitor/.Monitor1 \n");
	printf("\n");
	printf("-f\n");
	printf(" Causes to work with full key names. A full key name makes sense only on user/* keys, and differentiate from the regular key names in specifying the owner user. If the current user is someuser, the user/some/key full name is user:someuser/some/key. Makes effect in ls, export and get subcommands. \n");
	printf("\n");
	printf("-d\n");
	printf(" Causes get to work descriptivelly. When requesting a key it will show the comment, key name and its value in a fancy format \n");
	printf("\n");
	printf("-s\n");
	printf(" Causes get to be more friendly to Shell scripts. For example, when requesting user/env/env2/PATH, the output will be PATH=\"the value\", that is, only the basename of the key will be showed and the value will be surrounded by  \".\n");
	printf("\n");
	printf("-t type\n");
	printf(" When setting a key's value, you can specify the type with this switch. Currently accepted types are string for plain text, bin for binary as-is values, dir to create folder keys and link to create symbolic links between keys. Plain text are always stored as UTF-8(7) in Elektra, regardeless of your current encoding ($LANG). If you want to force a value to be stored without the UTF-8(7) encoding (a bad idea), you can set it as binary. Binary values should be avoided, because they are black boxes for system administrators. \n");
	printf("\n");
	printf("-b filename\n");
	printf(" Set the key value as the content of file filename. This option is more usefull when setting binary keys. \n");
	printf("\n");
	printf("-m mode\n");
	printf(" For the set command. Will set the key access permission to mode, which must be an octal number as for chmod(1). \n");
	printf("\n");
	printf("-u uid\n");
	printf(" Create the key with uid user ID. It can be a user name or a uid number. \n");
	printf("\n");
	printf("-g gid\n");
	printf(" Create the key with gid group ID. It can be a group name or a gid number \n");
	printf("\n");
	printf("-c comment\n");
	printf(" When setting keys, you can use this argument to set a descriptive comment for it. This comment is exactly as a comment in a plain text configuration file. The comment is stored as UTF-8(7) regardeless of your current encoding ($LANG). \n");
	printf("\n");
	printf("-v\n");
	printf(" With the ls subcommand, will make it show also the value stored in the key. \n");
	printf("\n");
	printf("--\n");
	printf(" With the set subcommand, everything after it will be considered the value, even text with dashes (-). Best Practices When Creating Keys\n");
	printf("\n");
	exit (0);
}






/**
 * Opens an editor to edit an XML representation of the keys.
 * This is one of the most complex commands of the kdb program.
 * It will
 * -# retrieve the desired keys
 * -# put them inside an editor in an XML format to let the user change them
 * -# wait for the editor to finish
 * -# reread the edited XML, converting to an internal KeySet
 * -# compare original and edited KeySets, with ksCompare(), to detect
 *    differences
 * -# remove removed keys
 * -# update updated keys
 * -# add added keys
 * -# leave untouched the not-changed keys
 *
 * @par Example:
 * @code
 * bash$ EDITOR=kedit kdb edit -R user/env # edit with kedit
 * bash# kdb edit -R system/sw/MyApp       # defaults to vi editor
 * @endcode
 *
 * @param argKeyName the parent key name (and children) that will be edited
 * @param argRecursive whether to act recursivelly or not
 * @param argAll whether to edit inactive keys or not
 * @param EDITOR environment var that defines editor to use, or @p vi
 * @see keyCompare(), ksCompare(), kdbGetChildKeys(), kdbSetKeys(),
 * 	ksToStream(), kdbRemoveKey()
 */
int commandEdit() {
	KeySet *ks;
	KeySet *ksEdited;
	KeySet *toRemove;
	Key *current;
	int ret;
	char filename[]="/var/tmp/kdbeditXXXXXX";
	char command[300];
	FILE *xmlfile=0;
	char choice[5];

	if (!ksFromXMLfile) return 1;
	
	ks=ksNew();

	kdbGetChildKeys(argKeyName,ks, KDB_O_SORT | KDB_O_NFOLLOWLINK |
		(argAll?KDB_O_INACTIVE:0) | (argRecursive?KDB_O_RECURSIVE:0));

	if (! ksGetSize(ks)) {
		/* Maybe the user parameter is not a parent key, but a single key */
		current=keyNew(argKeyName,KEY_SWITCH_END);
		if (kdbGetKey(current)) {
			/* Failed. Cleanup */
			keyDel(current);
			current=0;
		} else {
			/* We have something. */
			ksAppend(ks,current);
			current=0;
		}
	}

/*
	for (current=ks.start; current; current=current->next) {
		if (keyNeedsSync(current)) {
			printf("%s needs sync\n",current->key);
		}
	}
*/

	xmlfile=fdopen(mkstemp(filename),"rw+");

	ksToStream(ks,xmlfile,KDB_O_XMLHEADERS | 
		(argFullName?(KDB_O_FULLNAME | KDB_O_FULLUGID):0));
	fclose(xmlfile);

	do 
		{
	/* execute the editor and wait for it to finish */
	sprintf(command,"[ -z \"$EDITOR\" ] && EDITOR=vi; $EDITOR %s",filename);
	system(command);

	toRemove=ksNew();
	ksEdited=ksNew();

	/* ksFromXML is not a library function.
	 * It is implemented in and for this program only.
	 * It is pretty reusable code, though.
	 */
	 ret=ksFromXMLfile(ksEdited,filename);
	if (ret!=0)
		{
		printf("kdb cannot import this file, because it is not valid !\n");
		strcpy(choice,"");
		while (choice[0]!='E' && choice[0]!='C')
			{
			printf("Do you want to edit it again or to cancel ? (E/C) : ");
			fgets(choice,4, stdin );
			}
		}
	}
	while (ret!=0 && choice[0]=='E');
	remove(filename);
	
	if (ret==0)
		{

		ksCompare(ks,ksEdited,toRemove);
	
		/* Discard ksEdited because there is nothing else here
		* after keyCompare() */
		ksDel(ksEdited);
		
		/* Commit changed keys */
		ksRewind(ks);
		while ((ret=kdbSetKeys(ks))) {
			/* We got an error. Warn user. */
			Key *problem;
			char error[500];
			char keyname[300]="";
			
			problem=ksCurrent(ks);
			if (problem) keyGetFullName(problem,keyname,sizeof(keyname));
			sprintf(error,"kdb edit: while setting/updating %s", keyname);
			perror(error);
			
			/* And try to set keys again starting from the next key,
			* unless we reached the end of the KeySet */
			if (ksNext(ks) == 0) break;
		}
		
		ksDel(ks); /* Finished with this KeySet */
	
		/* Remove removed keys */
		ksRewind(toRemove);
		while ((current=ksNext(toRemove))) {
			char keyName[800];
	
			keyGetFullName(current,keyName,sizeof(keyName));
			ret=kdbRemove(keyName);
			if (ret != 0) {
				char error[850];
				
				sprintf(error,"kdb edit: while removing %s",keyName);
				perror(error);
			}
		}
	
		/* Finished with this KeySet too */
		ksDel(toRemove);
		}
	
	return 0;
}




/**
 * Business logic behind the 'kdb import' command.
 * Import an XML file (or standard input) into the key database.
 * This is usefull to import full programs keys, or restore backups.
 *
 * @par Example:
 * @code
 * bash$ kdb import myAppDefaultKeys.xml
 * bash$ generateKeys | kdb import
 * @endcode
 * 
 * @see kdbSetKeys(), commandExport()
 */
int commandImport() {
	KeySet *ks;
	int ret;

	if (!ksFromXMLfile || !ksFromXML) return 1;
	
	
	ks=ksNew();
	/* The command line parsing function will put the XML filename
	   in the argKeyName global. */
	if (argKeyName) ksFromXMLfile(ks,argKeyName);
	else ksFromXML(ks,fileno(stdin) /* more elegant then just '0' */);

	ksRewind(ks);
	while ((ret=kdbSetKeys(ks))) {
		/* We got an error. Warn user. */
		Key *problem;
		char error[500]="";
		char keyname[300]="";

		problem=ksCurrent(ks);
		if (problem) keyGetFullName(problem,keyname,sizeof(keyname));
		sprintf(error,"kdb import: while importing %s", keyname);
		perror(error);
		
		/* And try to set keys again starting from the next key,
		 *  unless we reached the end of KeySet */
		if (ksNext(ks) == 0) break;
	}
	
	return ret;
}





/**
 * Business logic behind the 'kdb export' command.
 * Export a set of keys to an XML format. Usefull to make backups or copy
 * keys to other machine or user.
 * Equivalent to 'kdb ls -xRv base/key/name'
 *
 * @par Example:
 * @code
 * bash# kdb export system > systemConfigurationBackup.xml
 * bash# kdb export system/sw/MyApp > myAppConfiguration.xml
 * bash$ kdb export system/sw/MyApp | sed -e 's|system/sw|user/sw|g' | kdb import
 * @endcode
 *
 * @see commandList(), commandImport()
 *
 */
int commandExport() {

	/* Equivalent to 'kdb ls -xRv
	   So lets mimic and reuse code */

	argSort=1;
	argRecursive=1;
	argAll=1;
	argXML=1;
	argShow=1;
	argValue=1;
	/* argFullName=1; */

	/* force a superuniversal modern charset: UTF-8 */
	setenv("LANG","en_US.UTF-8",1);
	
	/* reopen key database to forced charset to take effect */
	kdbClose();
	kdbOpen();

	return commandList();
}


/**
 * Business logic behind 'kdb mon' command.
 *
 * Will block your command line until some change happens to the
 * interested key.
 *
 * @par Example:
 * @code
 * bash$ kdb mon system/sw/MyApp/someKey
 * @endcode
 *
 * @see kdbMonitorKey(), kdbMonitorKeys()
 */
int commandMonitor() {
	Key *toMonitor;
	uint32_t diff;
	
	toMonitor=keyNew(argKeyName,KEY_SWITCH_NEEDSYNC,KEY_SWITCH_END);
	
	diff=kdbMonitorKey(
		toMonitor,           /* key to monitor */
		KEY_SWITCH_VALUE,    /* key info we are interested in */
		0,                   /* how many times to poll. 0 = ad-infinitum */
		500                  /* usecs between polls. 0 defaults to 1 second */);

	/*
	 * Since in our case we'll hang completelly until we get a key's
	 * value change, we don't have to check diff.
	 * So if method returned, the value has changed, and toMonitor has it.
	 */
	printf("New value is %s\n",(char *)keyStealValue(toMonitor));
	
	keyDel(toMonitor);
	return 0;
}


int loadToolsLib(void) {
	lt_dlhandle dlhandle=0;

	lt_dlinit();

	dlhandle=lt_dlopen("libelektratools.so");
	if (dlhandle == 0) {
		fprintf(stderr, "kdb: %s\n",dlerror());
		return 1;
	}
	
	ksFromXMLfile=lt_dlsym(dlhandle,"ksFromXMLfile");
	ksFromXML=lt_dlsym(dlhandle,"ksFromXML");
	
	return 0;
}

int doCommand(int command) {
	switch (command) {
		case CMD_SET:             return commandSet();
		case CMD_LIST:            return commandList();
		case CMD_LINK:            return commandLink();
		case CMD_GET:             return commandGet();
		case CMD_REMOVE:          return commandRemove();
		case CMD_EDIT:            return commandEdit();
		case CMD_LOAD:            return commandImport();
		case CMD_SAVE:            return commandExport();
		case CMD_MONITOR:         return commandMonitor();
		case CMD_MOVE:            return commandMove();
		case CMD_HELP:            return commandHelp();
	}
	return 0;
}

void cleanup()
{
	kdbClose();
}

int main(int argc, char **argv) {
	int command=0;
	int ret=0;


	if (loadToolsLib())
		fprintf(stderr,"kdb: XML importing and editing disabled\n");
	
	command=parseCommandLine(argc,argv);

	kdbOpen();
	atexit(cleanup); /**Make sure kdbClose will be used*/

	ret=doCommand(command);

	exit(ret);
}

/**
 * @}
 */
