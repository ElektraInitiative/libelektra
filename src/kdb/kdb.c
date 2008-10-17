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

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef HASGETOPT
#include "BSDgetopt.h"
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <time.h>
#include <locale.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_GRP_H
#include <grp.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <ctype.h>
#include <ltdl.h>
#include <assert.h>
/* We need fcntl.h for open and related constants used in our definition of mkstemp */
#ifdef HAVE_WIN32
#include <fcntl.h>
#endif

#include "kdb.h"
#include "kdbLibLoader.h"

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
#define CMD_INFO      25
#define CMD_HELP      30

#define ARGSIZE      30


#ifdef HAVE_WIN32
#define mkstemp(m) open(mktemp(m), O_RDWR)
#endif

/* we are cheating . . . */
ssize_t unencode(char *encoded, void *returned);


/* We'll load this methods dynamically to avoid libxml dependencies */
int (*ksFromXMLfile)(KeySet *ks,char *filename);
int (*ksFromXML)(KeySet *ks,int fd);



/**
 * @defgroup libexample  The kdb Command Source Code: Example of Full Library Utilization
 * @{
 */

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
int argDir=0;
int argHelp=0;
mode_t argMode=0;
int argType=KEY_TYPE_UNDEFINED;


/* Exported functions from help.c */
int commandHelp();
int helpCommand(int command);


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
			argDir=1;
			break;
		case 'f':
			argFullName=1;
			break;
		case 'g':
			strncpy(argGroup,optarg,ARGSIZE);
			break;
		case 'h':
			argHelp=1;
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
		commandHelp();
		exit(0);
	}
		
	/**Use KDB_ROOT as prefix in key name*/
	keyEnv = getenv ("KDB_ROOT");
	if (keyEnv) keyEnvLength = strblen (keyEnv);
	else keyEnvLength = 0;
	if (optind < argc) /*parse key name*/
	{
		keyOptLength = strblen (argv[optind]);
		argKeyName=realloc(argKeyName,
			keyEnvLength + keyOptLength + 1);
		assert(argKeyName!=NULL);
		if (keyEnv) strncpy (argKeyName, keyEnv,   keyEnvLength);
		strncpy(argKeyName + keyEnvLength, argv[optind], keyOptLength);
		if (keyEnv) *(argKeyName+keyEnvLength-1) = '/';
		optind ++;
	} else if (keyEnv) {
		argKeyName=realloc(argKeyName, keyEnvLength + 1);
		assert(argKeyName!=NULL);
		if (keyEnv) strncpy (argKeyName, keyEnv, keyEnvLength);
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
	if (!strcmp(sargCommand,"ls"))           argCommand=CMD_LIST;
	else if (!strcmp(sargCommand,"set"))     argCommand=CMD_SET;
	else if (!strcmp(sargCommand,"get"))     argCommand=CMD_GET;
	else if (!strcmp(sargCommand,"ln"))      argCommand=CMD_LINK;
	else if (!strcmp(sargCommand,"rm"))      argCommand=CMD_REMOVE;
	else if (!strcmp(sargCommand,"vi"))      argCommand=CMD_EDIT;
	else if (!strcmp(sargCommand,"edit"))    argCommand=CMD_EDIT;
	else if (!strcmp(sargCommand,"load"))    argCommand=CMD_LOAD;
	else if (!strcmp(sargCommand,"import"))  argCommand=CMD_LOAD;
	else if (!strcmp(sargCommand,"save"))    argCommand=CMD_SAVE;
	else if (!strcmp(sargCommand,"export"))  argCommand=CMD_SAVE;
	else if (!strcmp(sargCommand,"mon"))     argCommand=CMD_MONITOR;
	else if (!strcmp(sargCommand,"monitor")) argCommand=CMD_MONITOR;
	else if (!strcmp(sargCommand,"mv"))      argCommand=CMD_MOVE;
	else if (!strcmp(sargCommand,"info"))    argCommand=CMD_INFO;
	else if (!strcmp(sargCommand,"help"))    argCommand=CMD_HELP;
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
		else if (!strcmp(sargType,"dir"))    argDir=1; /* bkwrds compatibility */
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

#ifdef HAVE_PWD_H
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
#endif
#ifdef HAVE_GRP_H
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
#endif


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
	#ifdef S_ISLNK
	else if (S_ISLNK(mode)) readable[0]='l';
	#endif
	else readable[0]='-';

	readable[1] = mode & S_IRUSR ? 'r' : '-';
	readable[2] = mode & S_IWUSR ? 'w' : '-';
	readable[3] = mode & S_IXUSR ? 'x' : '-';
	#ifdef HAVE_WIN32
	readable[4] = 0;
	#else
	readable[4] = mode & S_IRGRP ? 'r' : '-';
	readable[5] = mode & S_IWGRP ? 'w' : '-';
	readable[6] = mode & S_IXGRP ? 'x' : '-';
	readable[7] = mode & S_IROTH ? 'r' : '-';
	readable[8] = mode & S_IWOTH ? 'w' : '-';
	readable[9] = mode & S_IXOTH ? 'x' : '-';
	readable[10]= 0;
	#endif
}




/*
 * Helper for the 'kdb ls' command
 *
 */
void listTime(time_t when,char *readable) {
	time_t current_time=time(0);
	char buf[400];
	#ifndef HAVE_CTIME_R
	char *ctimep = NULL;
	#endif
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
	#ifdef HAVE_CTIME_R
	ctime_r(&when,buf); /* buf will become "Wed Jun 30 21:49:08 1993\n" */
	#else
	ctimep = ctime(&when);
	strncpy(buf, ctimep, sizeof(buf));
	#endif
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
#ifdef HAVE_PWD_H
		if ( (pwd=getpwuid(keyGetUID(key))) != NULL ) {
			strcpy(p,pwd->pw_name);
			p+=strlen(p);
			*p=' '; p++;
			*p=' '; p++;
		} else {
			strcpy(p, "<unknown>");
			p+=strlen(p);
			*p=' '; p++;
			*p=' '; p++;
		}
#endif
#ifdef HAVE_GRP_H
		if ( (grp=getgrgid(keyGetGID(key))) != NULL ) {
			strcpy(p,grp->gr_name);
			p+=strlen(p);
			*p=' '; p++;
		} else {
			strcpy(p, "<unknow>");
			p+=strlen(p);
			*p=' '; p++;
			*p=' '; p++;
		}
#endif
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




/*
 * Helper for the 'kdb ls' command
 *
 */
void listAllKeys (KeySet * ks) {
	size_t listSize=ksGetSize(ks);
	Key *walker;
	
	if (listSize == 1) listSingleKey(ksHead(ks));
	else if (listSize > 1) {
		ksRewind(ks);
		while ((walker=ksNext(ks)))
			listSingleKey(walker);
	}
}



/*
 * Helper for the 'kdb ls -s' command
 *
 */
void listAllKeysForShell(KeySet *ks) {
	Key *walker=0;
	int isParent=0;
	size_t parentNameSize=strlen(argKeyName);
	char *keyName=0;
	void *keyValue=0;
	char *child=0;
	
	ksRewind(ks);
	while ((walker=ksNext(ks))) {
		keyName=keyStealName(walker);
		isParent=(int)strstr(keyName,argKeyName);
		
		if (isParent) {
			keyName+=parentNameSize;
			while (*keyName && *keyName == '/') keyName++;
		} else {
			keyName=0;
		}
		
		/* At this point keyName points to the begining of the
		   trailing key name after stripping argKeyName */
		if (keyName) {
			/* output key name */
			while ((child=strchr(keyName,'/'))) {
				fwrite(keyName,child-keyName,1,stdout);
				fputc('_',stdout);
				keyName=child+1;
			}
			fputs(keyName,stdout); /* writes remaining stuff */
			
			/* now output the value surrounded by '"' */
			fputs("=\"",stdout);
			keyValue=keyStealValue(walker);
			if (keyValue) fputs(keyValue,stdout);
			fputs("\";\n",stdout);
		}
	}
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
int commandRemove(KDBHandle handle) {
	if (!argKeyName) {
		fprintf(stderr,"kdb rm: No key name\n");
		return -1;
	}

	if (kdbRemove(handle,argKeyName)) {
		char error[300];
		
		sprintf(error,"kdb rm: \'%s\'",argKeyName);
		kdbPrintError(error);
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
int commandMove(KDBHandle handle) {
	Key *key;
	size_t size=0;
	int rc;
	
	/* Consistency */
	if (!argKeyName) {
		fprintf(stderr,"kdb mv: No target specified\n");
		return -1;
	}

	if (!argData) {
		fprintf(stderr,"kdb mv: \'%s\': No destination specified\n",argKeyName);
		return -1;
	}
	
	key=keyNew(argKeyName,KEY_SWITCH_END);
	size=keyGetNameSize(key);
	
	if (size == 0) {
		char error[100];
		
		sprintf(error,"kdb mv: \'%s\'", argKeyName);
		kdbPrintError(error);
		
		keyDel(key);
		return 1;
	}
	
	rc=kdbRename(handle,key,argData);
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
int commandSet(KDBHandle handle) {
	Key *key=0;
	int ret=0;
	char error[200];
	size_t offset=0;


	/* Consistency */
	if (!argKeyName) {
		fprintf(stderr,"kdb set: No key name\n");
		return -1;
	}

	key=keyNew(argKeyName,KEY_SWITCH_END);
	ret=kdbGetKey(handle,key);
	if (ret == 0) { /* Key already exists. Good. */
		/* Use existed key type if user didn't give us one */
		if (argType==KEY_TYPE_UNDEFINED) argType=keyGetType(key);
	} else if (errno!=KDB_RET_NOTFOUND) {
		/* Handle errors different from NOTFOUND */
		sprintf(error,"kdb set: %s",argKeyName);
		kdbPrintError(error);
	}

	/* Set or overwrite everything else... */
	
	if (argUID) keySetUID(key,*argUID);
	if (argGID) keySetGID(key,*argGID);
	if (argMode) keySetAccess(key,argMode);
	if (argDir) {
		mode_t mask=umask(0);
		umask(mask);
		keySetDir(key,mask);
	}

	if (argComment) keySetComment(key,argComment);
	
	if (argFile) {
		FILE *f;
		int end=0;
		
		if (argData) free(argData);
		argData=0;
		f=fopen(argFile,"r");
		
		if (!f) {
			sprintf(error,"kdb set: \'%s\'",argFile);
			kdbPrintError(error);
 			return -1;
		}
		while (! end) {
			char buffer[100];
			ssize_t r;
			
			r=read(fileno(f),buffer,sizeof(buffer));
			switch (r) {
				case 0:
					r=lseek(fileno(f),0,SEEK_END)-offset;
					end=1;
					break;
				case -1:
					/* those bizarre errors */
					fprintf(stderr,"kdb set: \'%s\': problem reading file\n",argFile);
					fclose(f);
					return -1;
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


	ret=kdbSetKey(handle,key);
	if (ret) {
		sprintf(error,"kdb set: \'%s\'",argKeyName);
		kdbPrintError(error);
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
int commandLink(KDBHandle handle) {
	int rc;

	/* Consistency */
	if (!argKeyName) {
		fprintf(stderr,"kdb ln: No target specified\n");
		return -1;
	}

	if (!argData) {
		fprintf(stderr,"kdb ln: \'%s\': No destination specified\n",argKeyName);
		return -1;
	}

	if ((rc=kdbLink(handle,argKeyName,argData))) {
		kdbPrintError("kdb ln");
	}

	return rc;
}














/**
 * The business logic behind 'kdb ls' command.
 * @param argKeyName key name to be listed
 * @param argRecursive whether to act recursively (-R)
 * @param argValue whether to show key values or not (-v)
 * @param argAll whether to list also inactive keys (-a)
 * @param argShell operate in a shell script friendly mode (-s)
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
int commandList(KDBHandle handle) {
	KeySet *ks; /* this is the container for all keys we'll collect bellow */
	KeySet *tmp;
	ssize_t ret;
	unsigned long options=0;
	char * buffer;
	Key *walker=0;

	/* Build our option set */
	
	if (argSort)                options |= KDB_O_SORT;
	if (argRecursive)           options |= KDB_O_RECURSIVE;
	if (argAll)                 options |= KDB_O_INACTIVE;
	if (!argValue)              options |= KDB_O_STATONLY;
	
	/* these options make sense only to ksToStream() */
	if (argFullName)            options |= KDB_O_FULLNAME | KDB_O_FULLUGID;
	/* ksToStream() defaults */ options |= KDB_O_XMLHEADERS | KDB_O_HIER;
	
	/* bad options for argShell */
	if (!argShell)              options |= KDB_O_DIR | KDB_O_NFOLLOWLINK;
	
	ks=ksNew();

	if (!argKeyName || strcmp(argKeyName, "/") == 0) {
		/* User don't want a specific key, so list the root keys */

		tmp=ksNew();
		kdbGetRootKeys(handle,tmp);

		if (argRecursive) {
			
			while ((walker=ksPop(tmp))) {
				/* walk root by root, retrieve entire subtree
				 * and append it to ks
				 */
				KeySet *thisRoot=ksNew();
				
				ret=kdbGetKeyChildKeys(handle,walker,thisRoot,options);
				
				/* A hack to transfer a key from a keyset to another.
				 * Don't do this at home.
				 */
				ksAppend(ks,walker);
				ksAppendKeys(ks,thisRoot);
				ksDel(thisRoot); /* we don't need the container anymore */
			}
		} else ksAppendKeys(ks,tmp);
		ksDel(tmp);
	} else {
		/* User gave us a specific key to start with */
	
		if (argKeyName[0] == '/')
		{
			buffer = malloc (strlen (argKeyName)+sizeof("system\0"));
			
			tmp = ksNew (); /* tmp workaround bug bec of filesys */
			strcpy (buffer, "system\0");
			ret=kdbGetChildKeys(handle,strcat (buffer, argKeyName),tmp,options);
			/*listAllKeys (tmp);*/
			ksAppendKeys (ks,tmp);
			ksDel (tmp);
		
			tmp = ksNew ();
			strcpy (buffer, "user\0");
			ret=kdbGetChildKeys(handle,strcat (buffer, argKeyName),tmp,options);
			/*listAllKeys (tmp);*/
			ksAppendKeys (ks,tmp);
			ksDel (tmp);

			free (buffer);
		} else {	
			ret=kdbGetChildKeys(handle,argKeyName,ks,options);
		}
	
		if (ret<0) {
			/* We got an error. Check if it is because its not a folder key */
			if (errno==ENOTDIR) {
				/* We still have a chance, since there is something there */
				Key *key=keyNew(argKeyName,KEY_SWITCH_END);
				
				if (argValue)
					/*TODO: cascading*/
					ret=kdbGetKey(handle,key);
				else ret=kdbStatKey(handle,key);
				
				if (ret == 0) ksAppend(ks,key);
				else {
					/* There is absolutelly nothing there */
					char error[200];

					keyDel(key);
					ksDel(ks);
					
					sprintf(error,"kdb ls: %s",argKeyName);
					kdbPrintError(error);
					return ret;
				}
				
			} else { /* A real error */
				char error[200];
				
				ksDel(ks);

				sprintf(error,"kdb ls: %s",argKeyName);
				kdbPrintError(error);
				return ret;
			}
		}
	}

	if (argShow) {
		if (argXML) ksToStream(ks,stdout,options);
		else if (argShell) listAllKeysForShell(ks);
		else listAllKeys(ks);
	}

	ksDel(ks);
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
int commandGet(KDBHandle handle) {
	int ret;
	Key *key = 0;
	char *buffer = 0; // used two times
	char *p;
	size_t size,cs=0;
	uint8_t keyType;
	char error[200];


	if (!argKeyName) {
		fprintf(stderr,"kdb get: No key name\n");
		fprintf(stderr,"run kdb get -h for more info\n");
		return -1;
	}
	
	key=keyNew(argKeyName,KEY_SWITCH_END);

	if (argKeyName[0] == '/')
	{
		buffer = malloc (strlen (argKeyName)+sizeof("system\0"));
		
		strcpy (buffer, "user\0");
		keySetName(key, strcat (buffer, argKeyName));
		ret=kdbGetKey(handle,key);
		if (ret == 0) goto done;
		
		strcpy (buffer, "system\0");
		keySetName(key, strcat (buffer, argKeyName));
		ret=kdbGetKey(handle,key);
		if (ret == 0) goto done;
	} else {	
		ret=kdbGetKey(handle,key);
	}

	if (ret) {
		sprintf(error,"kdb get: %s",argKeyName);
		kdbPrintError(error);
		goto cleanup;
	}
done:
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


	if (buffer) free (buffer);
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

	ret = 0;

cleanup:
	free(buffer);
	keyDel(key);

	return ret;
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
int commandEdit(KDBHandle handle) {
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

	kdbGetChildKeys(handle,argKeyName,ks, KDB_O_SORT | KDB_O_NFOLLOWLINK |
		(argAll?KDB_O_INACTIVE:0) | (argRecursive?KDB_O_RECURSIVE:0));

	if (! ksGetSize(ks)) {
		/* Maybe the user parameter is not a parent key, but a single key */
		current=keyNew(argKeyName,KEY_SWITCH_END);
		if (kdbGetKey(handle,current)) {
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

	ksToStream(ks,xmlfile,KDB_O_XMLHEADERS | KDB_O_HIER |
		KDB_O_FULLNAME | KDB_O_FULLUGID);
	fclose(xmlfile);

	do {
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
		if (ret!=0) {
			printf("kdb cannot import this file, because it is not valid !\n");
			strcpy(choice,"");
			while (choice[0]!='E' && choice[0]!='C') {
				printf("Do you want to edit it again or to cancel ? (E/C) : ");
				fgets(choice,4, stdin );
			}
		}
	} while (ret!=0 && choice[0]=='E');
	remove(filename);
	
	if (ret==0) {
		ksCompare(ks,ksEdited,toRemove);
	
		/* Discard ksEdited because there is nothing else here
		* after keyCompare() */
		ksDel(ksEdited);
		
		/* Commit changed keys */
		ksRewind(ks);
		while ((ret=kdbSetKeys(handle,ks))) {
			/* We got an error. Warn user. */
			Key *problem;
			char error[500];
			char keyname[300]="";
			
			problem=ksCurrent(ks);
			if (problem) keyGetFullName(problem,keyname,sizeof(keyname));
			sprintf(error,"kdb edit: while setting/updating %s", keyname);
			kdbPrintError(error);
			
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
			ret=kdbRemove(handle,keyName);
			if (ret != 0) {
				char error[850];
				
				sprintf(error,"kdb edit: while removing %s",keyName);
				kdbPrintError(error);
			}
		}
	
		/* Finished with this KeySet too */
		ksDel(toRemove);
		}
	
	return 0;
}


/**
 * Business logic behind the 'kdb info' command.
 * Displays some information about the Elektra library, version, backend, etc.
 *
 * @par Example:
 * @code
 * bash$ kdb info
 * @endcode
 * 
 * @see kdbGetInfo(), kdbInfoToString(), kdbFreeInfo()
 */
int commandInfo(KDBHandle handle) {
	KDBInfo *libraryInfo;
	char textInfo[200];

	libraryInfo=kdbGetInfo(handle);
	kdbInfoToString(libraryInfo, textInfo, sizeof(textInfo));

	printf("%s\n", textInfo);

	return 0;
}


/**
 * Business logic behind the 'kdb import' command.
 * Import an XML file (or standard input) into the key database.
 * This is usefull to import full application's keys, or restore backups.
 *
 * @par Example:
 * @code
 * bash$ kdb import myAppDefaultKeys.xml
 * bash$ generateKeys | kdb import
 * @endcode
 * 
 * @see kdbSetKeys(), commandExport()
 */
int commandImport(KDBHandle handle) {
	KeySet *ks;
	int ret;

	if (!ksFromXMLfile || !ksFromXML) return 1;
	
	
	ks=ksNew();
	/* The command line parsing function will put the XML filename
	   in the argKeyName global. */
	if (argKeyName) ksFromXMLfile(ks,argKeyName);
	else ksFromXML(ks,fileno(stdin) /* more elegant then just '0' */);

	ksRewind(ks);
	while ((ret=kdbSetKeys(handle,ks))) {
		/* We got an error. Warn user. */
		Key *problem;
		char error[500]="";
		char keyname[300]="";

		problem=ksCurrent(ks);
		if (problem) keyGetFullName(problem,keyname,sizeof(keyname));
		sprintf(error,"kdb import: while importing %s", keyname);
		kdbPrintError(error);
		
		/* And try to set keys again starting from the next key,
		 *  unless we reached the end of KeySet */
		if (ksNext(ks) == 0) break;
	}
	
	ksDel(ks);
	
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
int commandExport(KDBHandle *handle) {

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
	/*setenv("LANG","en_US.UTF-8",1);*/
	putenv("LANG=en_US.UTF-8");
	/*setlocale(LC_CTYPE, "UTF-8");*/
	
	/* reopen key database to forced charset to take effect */
	kdbClose(handle);
	kdbOpen(handle);

	return commandList(*handle);
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
int commandMonitor(KDBHandle handle) {
	Key *toMonitor;
	uint32_t diff;
	
	toMonitor=keyNew(argKeyName,KEY_SWITCH_NEEDSYNC,handle,KEY_SWITCH_END);
	
	diff=kdbMonitorKey(handle,
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
	kdbLibHandle dlhandle=0;

	kdbLibInit();

	dlhandle=kdbLibLoad("libelektratools");
	if (dlhandle == 0) {
		return 1;
	}
	
	ksFromXMLfile=kdbLibSym(dlhandle,"ksFromXMLfile");
	ksFromXML=kdbLibSym(dlhandle,"ksFromXML");
	
	return 0;
}


int doCommand(int command, KDBHandle *handle) {
	switch (command) {
		case CMD_SET:             return commandSet(*handle);
		case CMD_LIST:            return commandList(*handle);
		case CMD_LINK:            return commandLink(*handle);
		case CMD_GET:             return commandGet(*handle);
		case CMD_REMOVE:          return commandRemove(*handle);
		case CMD_EDIT:            return commandEdit(*handle);
		case CMD_LOAD:            return commandImport(*handle);
		case CMD_SAVE:            return commandExport(handle);
		case CMD_MONITOR:         return commandMonitor(*handle);
		case CMD_MOVE:            return commandMove(*handle);
		case CMD_INFO:            return commandInfo(*handle);
		case CMD_HELP:            return commandHelp(*handle);
	}
	return 0;
}


/*

void cleanup() {
	kdbClose();
}

*/

int main(int argc, char **argv) {
	KDBHandle handle=0;
	int command=0;
	int ret=0;


	if (loadToolsLib())
		fprintf(stderr,"kdb: XML importing and editing disabled\n");
	
	/* Parse the command line */
	command=parseCommandLine(argc,argv);

	/* Check if user only wants some help (kdb -h {command})*/
	if (argHelp) helpCommand(command);

	/* Make sure kdbClose() will be used */
	/* atexit(cleanup); */
	
	/* Open key database */
	kdbOpen(&handle);

	/* Execute command with parameters from command line */
	ret=doCommand(command,&handle);

	kdbClose(&handle);
	
	exit(ret);
}

/**
 * @}
 */
