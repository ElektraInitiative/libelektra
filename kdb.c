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
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id$
$LastChangedBy$

*/


/**
 * @defgroup libexample Example of Full Library Utilization: kdb command
 * @{
 */







#include "kdb.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <grp.h>
#include <pwd.h>
#include <time.h>
#include <ctype.h>
#include <locale.h>

/* FIXME: remove libXML dependencies for kdb command */
#include <libxml/xmlreader.h>


#define CMD_GET       1
#define CMD_SET       2
#define CMD_REMOVE    3
#define CMD_LIST      4
#define CMD_LINK      5
#define CMD_EDIT      6
#define CMD_LOAD      7
#define CMD_SAVE      8
#define CMD_MONITOR   9

#define ARGSIZE      30

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

	int opt;

	int test;

	*sargType=*argUser=*argGroup=*sargCommand=*sargMode=0;

	while ((opt=getopt(argc,argv,"-t:c:u:g:m:b:raflvRdxsi"))!=-1) {
		switch (opt) {
			case 't':
				strncpy(sargType,optarg,ARGSIZE);
				break;
			case 'c':
				argComment=realloc(argComment,strlen(optarg)+1);
				strcpy(argComment,optarg);
				break;
			case 'b':
				argFile=realloc(argFile,strlen(optarg)+1);
				strcpy(argFile,optarg);
				break;
			case 'u':
				strncpy(argUser,optarg,ARGSIZE);
				break;
			case 'g':
				strncpy(argGroup,optarg,ARGSIZE);
				break;
			case 'm':
				strncpy(sargMode,optarg,ARGSIZE);
				break;
			case 'R':
				argRecursive=KDB_O_RECURSIVE;
				break;
			case 'l':
				argLong=1;
				break;
			case 'v':
				argValue=1;
				break;
			case 'd':
				argDescriptive=1;
				argLong=1;
				break;
			case 'a':
				argAll=1;
				break;
			case 's':
				argShell=1;
				break;
			case 'f':
				argFullName=1;
				break;
			case 'n':
				argSort=0;
				break;
			case 'i':
				argShow=0;
				break;
			case 'x':
				argXML=1;
				break;
			case 1: { /* handle non '-x' args */
				test=optind;
				if (*sargCommand==0) { /* parse sub-command */
					strncpy(sargCommand,optarg,ARGSIZE);
				} else if (!argKeyName) { /* parse key name */
					argKeyName=realloc(argKeyName,strlen(optarg)+1);
					strcpy(argKeyName,optarg);
				} else if (!argData) { /* parse value */
					argData=realloc(argData,strlen(optarg)+1);
					strcpy(argData,optarg);
				}
				break;
			}
		}
	}
	test=optind;

	/* Now check if we have the '--' argument, and get all its values */
	if (!strcmp(argv[optind-1],"--") && optind<argc) {
		int wordind=optind;
		size_t valueLength=0;

		while (wordind<argc) valueLength+=strlen(argv[wordind++])+1;
		argData=realloc(argData,valueLength);
		strcpy(argData,argv[optind++]);
		/* very ugly */
		while (optind<argc) sprintf(argData,"%s %s",argData,argv[optind++]);
	}

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
	else {
		fprintf(stderr,"Invalid subcommand\n");
		exit(1);
	}

	/* Parse type */
	if (*sargType!=0) {
		/* TODO: use regex */
		if (!strcmp(sargType,"string")) argType=KEY_TYPE_STRING;
		else if (!strcmp(sargType,"bin")) argType=KEY_TYPE_BINARY;
		else if (!strcmp(sargType,"binary")) argType=KEY_TYPE_BINARY;
		else if (!strcmp(sargType,"dir")) argType=KEY_TYPE_DIR;
		else if (!strcmp(sargType,"link")) argType=KEY_TYPE_LINK;
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
				fprintf(stderr,"kdb: Invalid user %s. Ignoring\n", argUser);
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
				fprintf(stderr,"kdb: Invalid group %s. Ignoring\n",argGroup);
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
	if (argValue && (keyGetDataSize(key)>0)) {
		u_int8_t ktype;

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
		perror("kdb rm");
		return -1;
	}
	return 0;
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
 * @see kdbSetKey()
 * @param argKeyName name of the key that will be set
 * @param argComment comment to be set to key (-c)
 * @param argType type of the key (-t)
 * @param argMode access permissions that will be set to sey (-m)
 * @param argUID UID to be set to sey
 * @param argGID GID to be set to sey
 * @param argData the value to the key
 * @param argFile a filename to use as the input for the value
 */
int commandSet() {
	Key key;
	int ret;
	char error[200];
	size_t offset=0;


	/* Consistency */
	if (!argKeyName) {
		fprintf(stderr,"kdb set: No key name\n");
		return -1;
	}

	keyInit(&key);
	keySetName(&key,argKeyName);
	ret=kdbGetKey(&key);
	if (!ret) { /* key already exist. good. */
		if (argComment) keySetComment(&key,argComment);
		if (argType==KEY_TYPE_UNDEFINED) argType=keyGetType(&key);
	} else if (errno!=KDB_RET_NOTFOUND) {
		sprintf(error,"kdb set: %s",argKeyName);
		perror(error);
	}

	if (argUID) keySetUID(&key,*argUID);
	if (argGID) keySetGID(&key,*argGID);

	if (argMode) keySetAccess(&key,argMode);

	if (argFile) {
		FILE *f;
		int end=0;
		
		if (argData) free(argData);
		argData=0;
		f=fopen(argFile,"r");
		
		if (!f) {
			sprintf(error,"kdb set: %s",argFile);
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
			memcpy(argData+offset,buffer,r);
			offset+=r;
		}
		fclose(f);
	}
	
	switch (argType) {
		case KEY_TYPE_DIR: keySetType(&key,KEY_TYPE_DIR);
			break;
		case KEY_TYPE_STRING:
			if (argData) keySetString(&key,argData);
			break;
		case KEY_TYPE_BINARY:
			if (argData) {
				if (offset) keySetBinary(&key,argData,offset);
				else keySetBinary(&key,argData,strblen(argData));
			}
			break;
		case KEY_TYPE_LINK: keySetLink(&key,argData);
			break;
	}

	ret=kdbSetKey(&key);
	if (ret) {
		sprintf(error,"kdb set: %s",argKeyName);
		perror(error);
	}
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
 * @see kdbLink()
 * @see keySetType()
 * @param argKeyName name of the target key
 * @param argData name of the link key to be created
 */
int commandLink() {
	int rc;

	/* Consistency */
	if (!argKeyName) {
		fprintf(stderr,"kdb ln: No target specified\n");
		return -1;
	}

	if (!argData) {
		fprintf(stderr,"kdb ln: %s: No destination specified",argKeyName);
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
 * @see kdbGetRootKeys()
 * @see kdbGetChildKeys()
 * @see keyToStream()
 * @see ksToStream()
 * @see commandExport() for the 'kdb export' command
 */
int commandList() {
	KeySet ks;
	Key *key=0;
	int ret;

	ksInit(&ks);

	if (!argKeyName) {
		KeySet roots;
		/* User don't want a specific key, so list the root keys */

		ksInit(&roots);
		kdbGetRootKeys(&roots);

		if (argRecursive) {
			key=roots.start;
			while (key) {
				char rootName[200];
				KeySet thisRoot;
				Key *temp;

				ksInit(&thisRoot);
				keyGetFullName(key,rootName,sizeof(rootName));
				if (argValue) ret=kdbGetChildKeys(rootName,&thisRoot,
					(argSort?KDB_O_SORT:0) | argRecursive | KDB_O_DIR |
					(argAll?KDB_O_INACTIVE:0) | KDB_O_NFOLLOWLINK);
				else ret=kdbGetChildKeys(rootName,&thisRoot,
					(argSort?KDB_O_SORT:0) | KDB_O_STATONLY | argRecursive |
					KDB_O_DIR | (argAll?KDB_O_INACTIVE:0) | KDB_O_NFOLLOWLINK);
				temp=key->next;
				ksAppend(&ks,key);
				ksAppendKeys(&ks,&thisRoot);
				key=temp;
			}
		} else ksAppendKeys(&ks,&roots);
	} else {
		/* User gave us a specific key to start with */

		if (argValue) ret=kdbGetChildKeys(argKeyName,&ks,
			(argSort?KDB_O_SORT:0) | argRecursive | KDB_O_DIR |
			(argAll?KDB_O_INACTIVE:0) | KDB_O_NFOLLOWLINK);
		else ret=kdbGetChildKeys(argKeyName,&ks,
			(argSort?KDB_O_SORT:0) | KDB_O_STATONLY | argRecursive |
			KDB_O_DIR | (argAll?KDB_O_INACTIVE:0) | KDB_O_NFOLLOWLINK);
	
		if (ret) {
				/* We got an error. Check if it is because its not a folder key */
			if (errno==ENOTDIR) {
				/* We still have a chance, since there is something there */
				key=(Key *)malloc(sizeof(Key));
				keyInit(key);
				keySetName(key,argKeyName);
				if (argValue) ret=kdbGetKey(key);
				else ret=kdbStatKey(key);
				if (ret) {
					char error[200];

					keyClose(key); free(key);
					ksClose(&ks);
					
					sprintf(error,"kdb ls: %s",argKeyName);
					perror(error);
					return ret;
				}
			} else { /* A real error */
				char error[200];
				
				ksClose(&ks);

				sprintf(error,"kdb ls: %s",argKeyName);
				perror(error);
				return ret;
			}
		}
	}

	if (argShow) {
		if (argXML) {
			if (key) keyToStream(key,stdout,0);
			else if (ks.size)
				ksToStream(&ks,stdout,KDB_O_XMLHEADERS);
		} else {
			if (key) listSingleKey(key);
			else if (ks.size) {
				ksRewind(&ks);
				while ((key=ksNext(&ks)))
					listSingleKey(key);
			}
		}
	}

	ksClose(&ks);
	if (key) {
	        keyClose(key);
	        free(key);
	}
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
 * @see kdbGetKey()
 * @see kdbGetBaseName()
 * @see keyGetComment()
 * @see keyGetString()
 *
 */
int commandGet() {
	int ret;
	Key key;
	char *buffer;
	char *p;
	size_t size,cs=0;
	u_int8_t keyType;

	if (!argKeyName) {
		fprintf(stderr,"kdb get: No key name\n");
		return -1;
	}

	keyInit(&key);
	keySetName(&key,argKeyName);

	ret=kdbGetKey(&key);
	if (ret) {
		char error[200];

		sprintf(error,"kdb get: %s",argKeyName);
		perror(error);
		return ret;
	}
	size=keyGetDataSize(&key);
	if (argDescriptive) {
		cs=keyGetCommentSize(&key);
		if (cs) size+=cs+3;
	}
	if (argShell) {
		size+=keyGetBaseNameSize(&key);
		size+=2; /* for 2 '"' to wrap the value */
	} else if (argLong) {
		if (argFullName) size+=keyGetFullNameSize(&key);
		else size+=keyGetNameSize(&key);
	}


	p=buffer=malloc(size);


	if (argDescriptive) {
		if (cs) {
			p+=sprintf(p,"# ");
			p+=keyGetComment(&key,p,size-(p-buffer));
			*--p='\n'; p++;
		}
	}
	if (argShell) {
		p+=keyGetBaseName(&key,p,size-(p-buffer));
		*--p='='; p++;
		*p='\"'; p++;
	} else if (argLong) {
		if (argFullName) p+=keyGetFullName(&key,p,size-(p-buffer));
		else p+=keyGetName(&key,p,size-(p-buffer));
		*--p='='; p++;
	}
	
	keyType=keyGetType(&key);

	if (keyType<KEY_TYPE_STRING) p+=keyGetBinary(&key,p,size-(p-buffer));
	else p+=keyGetString(&key,p,size-(p-buffer));
	if (argShell) {
		*--p='\"'; p++;
		*p=0;
	}
	if (keyType<KEY_TYPE_STRING) fwrite(buffer,size,1,stdout);
	else printf("%s\n",buffer);


	free(buffer);

	return 0;
}











/*
 * This function is completelly dependent on libxml.
 */
int processNode(KeySet *ks, xmlTextReaderPtr reader) {
	xmlChar *nodeName=0;
	xmlChar *buffer=0;
	Key *newKey=0;
	
	nodeName=xmlTextReaderName(reader);
	if (!strcmp(nodeName,"key")) {
		int end=0;
		
		newKey=malloc(sizeof(Key));
		keyInit(newKey);
		
		xmlFree(nodeName); nodeName=0;
		
		buffer=xmlTextReaderGetAttribute(reader,"name");
		keySetName(newKey,(char *)buffer);
		xmlFree(buffer); buffer=0;
		
		buffer=xmlTextReaderGetAttribute(reader,"type");
		if (!strcmp(buffer,"string"))
			keySetType(newKey,KEY_TYPE_STRING);
		else if (!strcmp(buffer,"binary"))
			keySetType(newKey,KEY_TYPE_BINARY);
		else if (!strcmp(buffer,"link"))
			keySetType(newKey,KEY_TYPE_LINK);
		else if (!strcmp(buffer,"directory"))
			keySetType(newKey,KEY_TYPE_DIR);
		xmlFree(buffer); buffer=0;

		
		/* Parse UID */
		buffer=xmlTextReaderGetAttribute(reader,"uid");
		if (isdigit(*buffer)) {
			keySetUID(newKey,atoi(buffer));
		} else {
			struct passwd *pwd;
			pwd=getpwnam(buffer);
			if (pwd) keySetUID(newKey,pwd->pw_uid);
			else fprintf(stderr,"kdb: Ignoring invalid user %s.\n", buffer);
		}
		xmlFree(buffer); buffer=0;

		
		/* Parse GID */
		buffer=xmlTextReaderGetAttribute(reader,"gid");
		if (isdigit(*buffer)) {
			keySetGID(newKey,atoi(buffer));
		} else {
			struct group *grp;
			grp=getgrnam(buffer);
			if (grp) keySetGID(newKey,grp->gr_gid);
			else fprintf(stderr,"kdb: Ignoring invalid group %s.\n",buffer);
		}
		xmlFree(buffer); buffer=0;


		/* Parse permissions */
		buffer=xmlTextReaderGetAttribute(reader,"mode");
		if (buffer) keySetAccess(newKey,strtol(buffer,0,8));
		xmlFree(buffer); buffer=0;


		/* Parse everything else */
		while (!end) {
			xmlFree(nodeName); nodeName=0;
			xmlTextReaderRead(reader);
			nodeName=xmlTextReaderName(reader);

			if (!strcmp(nodeName,"value")) {
				if (xmlTextReaderIsEmptyElement(reader) ||
					xmlTextReaderNodeType(reader)==15) continue;
				xmlTextReaderRead(reader);
				buffer=xmlTextReaderValue(reader);
				if (buffer) {
					switch (keyGetType(newKey)) {
						case KEY_TYPE_STRING:
							keySetString(newKey,buffer);
							break;
						case KEY_TYPE_BINARY:
							keySetBinary(newKey,buffer,strlen(buffer)+1);
							break;
						case KEY_TYPE_LINK:
							keySetLink(newKey,buffer);
							break;
					}
				}
			} else if (!strcmp(nodeName,"comment")) {
				if (xmlTextReaderIsEmptyElement(reader) ||
					xmlTextReaderNodeType(reader)==15) continue;
				xmlTextReaderRead(reader);
				buffer=xmlTextReaderValue(reader);

				keySetComment(newKey,buffer);
			} else if (!strcmp(nodeName,"key")) {
				if (xmlTextReaderNodeType(reader)==15) end=1;
			}

			xmlFree(buffer); buffer=0;
		}
	}

	if (nodeName) xmlFree(nodeName),nodeName=0;

	if (newKey) ksAppend(ks,newKey);
	return 0;
}






/*
 * This function is completelly dependent on libxml.
 * It is the workhorse for ksFromXML() and ksFromXMLfile()
 */
int ksFromXMLReader(KeySet *ks,xmlTextReaderPtr reader) {
	int ret;

	ret = xmlTextReaderRead(reader); /* <keyset> */
	ret = xmlTextReaderRead(reader); /* first <key> */
	while (ret == 1) {
		processNode(ks, reader);
		ret = xmlTextReaderRead(reader);
	}
	xmlFreeTextReader(reader);
	if (ret) fprintf(stderr,"kdb: Failed to parse XML input\n");

	return ret;
}









int ksFromXMLfile(KeySet *ks,char *filename) {
	xmlTextReaderPtr reader;
	int ret;

	reader = xmlNewTextReaderFilename(filename);
	if (reader) {
		ret=ksFromXMLReader(ks,reader);
	} else {
		perror("kdb");
		return 1;
	}
	return ret;
}





/* FIXME: its not working when fd is stdin */
int ksFromXML(KeySet *ks,int fd) {
	/* Support for old XML library, that doesn't have xmlReaderForFd() */
	char filename[]="/var/tmp/rgeditXXXXXX";
	FILE *xmlfile=0;
	xmlfile=fdopen(mkstemp(filename),"rw+");
	while (! feof(xmlfile)) {
		char buffer[1000];
		ssize_t readed, writen;

		readed=read(fd,buffer,sizeof(buffer));
		if (readed<0) {
			perror("kdb");
			fclose(xmlfile);
			remove(filename);
			return 1;
		}

		writen=write(fileno(xmlfile),buffer,readed);
		if (writen<0) {
			perror("kdb");
			fclose(xmlfile);
			remove(filename);
			return 1;
		}
	}
	fclose(xmlfile);
	return ksFromXMLfile(ks,filename);
	/* end of support */

	/* This code requires a newer version of XML library. Don't use it yet
	// a complete XML document is expected
	xmlTextReaderPtr reader=0;
	int ret;
	reader=xmlReaderForFd(fd,"file:/tmp/imp.xml",0,0);
	if (reader) {
		ret=ksFromXMLReader(ks,reader);
	} else {
		printf("kdb: Unable to open file descriptor %d for XML reading\n", fd);
		return 1;
	}
	return ret;
	// end of newer code */
}

















/**
 * Opens an editor to edit an XML representation of the keys.
 * This is one of the most complex commands of the kdb program.
 * Is will
 * -# retrieve the desired keys
 * -# put them as inside an editor in an XML format to let the user edit
 * -# wait for the editor to finish
 * -# reread the edited XML, converting to an internal KeySet
 * -# compare original and edited KeySets to detect differences
 * -# remove removed keys
 * -# update updated keys
 * -# add added keys
 * -# leave untouched the not changed keys
 *
 * @par Example:
 * @code
 * bash$ EDITOR=kedit kdb edit -R user/env # edit with kedit
 * bash# kdb edit -R system/sw/MyApp       # defaults to vi editor
 * @endcode
 *
 * @see keyCompare()
 * @see ksCompare()
 * @see kdbGetChildKeys()
 * @see ksToStream()
 * @see kdbSetKeys()
 * @param argKeyName the parent key name (and children) that will be edited
 * @param argRecursive whether to act recursivelly or not
 * @param argAll whether to edit inactive keys or not
 * @param EDITOR environment var that defines editor to use, or @p vi
 */
int commandEdit() {
	KeySet ks;
	KeySet ksEdited;
	KeySet toRemove;
	Key *current;
	int ret;
	char filename[]="/var/tmp/rgeditXXXXXX";
	char command[300];
	FILE *xmlfile=0;

	ksInit(&ks);

	kdbGetChildKeys(argKeyName,&ks, KDB_O_SORT | KDB_O_NFOLLOWLINK |
		(argAll?KDB_O_INACTIVE:0) | (argRecursive?KDB_O_RECURSIVE:0));

	if (!ks.size) {
		/* Maybe the user parameter is not a parent key, but a single key */
		current=malloc(sizeof(Key));
		keyInit(current);
		keySetName(current,argKeyName);
		if (kdbGetKey(current)) {
			/* Failed. Cleanup */
			keyClose(current);
			free(current);
			current=0;
		} else {
			/* We have something. */
			ksAppend(&ks,current);
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

	ksToStream(&ks,xmlfile,KDB_O_XMLHEADERS);
	fclose(xmlfile);

	sprintf(command,"[ -z \"$EDITOR\" ] && EDITOR=vi; $EDITOR %s",filename);
	system(command);

	ksInit(&toRemove);
	ksInit(&ksEdited);

	/* ksFromXML is not a library function.
	 * It is implemented in and for this program only.
   *       It is pretty reusable code, though.
	 */
	ksFromXMLfile(&ksEdited,filename);
	remove(filename);

	ksCompare(&ks,&ksEdited,&toRemove);

	ksRewind(&ks);
	ret=kdbSetKeys(&ks);
	if (ret != 0) {
		Key *problem;
		char error[500];
		char keyname[300]="";
		
		problem=ksCurrent(&ks);
		if (problem) keyGetFullName(problem,keyname,sizeof(keyname));
		sprintf(error,"kdb edit: when commiting %s",keyname);
		perror(error);
	}

	ksRewind(&toRemove);
	while ((current=ksNext(&toRemove))) {
		char keyName[800];

		keyGetFullName(current,keyName,sizeof(keyName));
		kdbRemove(keyName);
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
 * @see kdbSetKeys()
 * @see commandExport()
 */
int commandImport() {
	KeySet ks;
	int ret;

	ksInit(&ks);
	/* The command line parsing function will put the XML filename
	   in the argKeyName global, so forget the variable name. */
	if (argKeyName) ksFromXMLfile(&ks,argKeyName);
	else ksFromXML(&ks,fileno(stdin) /* more elegant then just '0' */);

	ksRewind(&ks);
	ret=kdbSetKeys(&ks);
	
	if (ret != 0) {
		Key *problem;
		char error[500];
		char keyname[300]="";
		
		problem=ksCurrent(&ks);
		if (problem) keyGetFullName(problem,keyname,sizeof(keyname));
		sprintf(error,"kdb import: when commiting %s",keyname);
		perror(error);
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
 * @see commandList()
 * @see commandImport()
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
	argFullName=1;

	/* force UTF-8 for a superuniversal charset */
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
 * @see kdbMonitorKey()
 * @see kdbMonitorKeys()
 */
int commandMonitor() {
	Key toMonitor;
	u_int32_t diff;
	char *newData=0;
	size_t dataSize;
	
	keyInit(&toMonitor);
	keySetName(&toMonitor,argKeyName);
	kdbGetKey(&toMonitor);
	
	diff=kdbMonitorKey(
		&toMonitor,          /* key to monitor */
		KEY_FLAG_HASDATA,    /* particular info from the key we are interested */
		0,                   /* how many times to poll. 0 = ad-infinitum */
		500                  /* usecs between polls. 0 defaults to 1 second */);

	/*
	 * Since in our case we'll hang completelly until we get a key's
	 * value change, we don't have to check diff.
	 * So if method returned, the value has changed, and toMonitor has it.
	 */
	newData=malloc(dataSize=keyGetDataSize(&toMonitor));
	keyGetString(&toMonitor,newData,dataSize);
	printf("New value is %s\n",newData);
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
	}
	return 0;
}


int main(int argc, char **argv) {
	int command=0;
	int ret=0;

	command=parseCommandLine(argc,argv);

	kdbOpen();
	ret=doCommand(command);
	kdbClose();

	return ret;
}

/**
 * @}
 */
