/***************************************************************************
            kdbtools.c  -  Elektra High Level Methods
                             -------------------
    begin                : Sat Jan 22 2005
    copyright            : (C) 2005 by Avi Alkalay
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
#include <config.h>
#endif

#include <ctype.h>
#include <string.h>

#ifdef HAVE_GRP_H
#include <grp.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <unistd.h>
#include <stdlib.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include <libxml/xmlreader.h>
#include <libxml/xmlschemas.h>

#include "kdbtools.h"
#include "kdbprivate.h"
#include "kdb.h"

/* #define KDB_SCHEMA_PATH       DATADIR KDB_SCHEMA_REL_PATH */
#define KDB_SCHEMA_PATH_KEY   "system/sw/kdb/current/schemapath"

#ifdef ELEKTRA_STATIC
#define ksFromXMLfile libelektratools_LTX_ksFromXMLfile
#define ksFromXML libelektratools_LTX_ksFromXML
#endif


/**
 * @defgroup tools KDB Tools :: Library with some high-level functions
 * @brief General methods mainly for XML manipulation.
 *
 * To use them:
 * @code
#include <kdbtools.h>
 * @endcode
 *
 * Here are some functions that are in a separate library because they
 * depend on non-basic libraries as libxml. Use the kdbtools library if your
 * program won't be installed in /bin, or is not essential in early boot
 * stages.
 */


/*
 * Processes the current <key> node from reader, converting from XML
 * to a Key object, and ksAppend() it to ks.
 * 
 * See keyToStream() for an example of a <key> node.
 * 
 * 
 * This function is completelly dependent on libxml.
 */
int consumeKeyNode(KeySet *ks, const char *context, xmlTextReaderPtr reader) {
	xmlChar *nodeName=0;
	xmlChar *buffer=0;
	xmlChar *privateContext=0;
	Key *newKey=0;
	int appended=0;

	/* printf("%s", KDB_SCHEMA_PATH); */
	
	nodeName=xmlTextReaderName(reader);
	if (!strcmp((char *)nodeName,"key")) {
		uint8_t type=KEY_TYPE_STRING; /* default type */
		int end=0;
		
		newKey=keyNew(0);

		xmlFree(nodeName); nodeName=0;


		/* a <key> must have one of the following:
		   - a "name" attribute, used as an absolute name overriding the context
		   - a "basename" attribute, that will be added to the current context
		   - a "parent" plus "basename" attributes, both added to current context
		   - only a "parent", added to current context
		*/
		buffer=xmlTextReaderGetAttribute(reader,(const xmlChar *)"name");
		if (buffer) {
			/* set absolute name */
			keySetName(newKey,(char *)buffer);
			xmlFree(buffer); buffer=0;
		} else {
			/* logic for relative name calculation */
			
			privateContext=xmlTextReaderGetAttribute(reader,
				(const xmlChar *)"parent");
			buffer=xmlTextReaderGetAttribute(reader,
				(const xmlChar *)"basename");

			if (context) keySetName(newKey,context);
			if (privateContext) keyAddBaseName(newKey, (char *)privateContext);
			if (buffer) keyAddBaseName(newKey,(char *)buffer);

			xmlFree(privateContext); privateContext=0;
			xmlFree(buffer); buffer=0;
		}


		/* test for a short value attribute, instead of <value> bellow */
		buffer=xmlTextReaderGetAttribute(reader,(const xmlChar *)"value");
		if (buffer) {
			keySetRaw(newKey,buffer,strblen((char *)buffer));
			xmlFree(buffer); buffer=0;
		}


#ifdef HAVE_PWD_H
		/* Parse UID */
		buffer=xmlTextReaderGetAttribute(reader,(const xmlChar *)"uid");
		if (buffer) {
			if (isdigit(*buffer))
				keySetUID(newKey,atoi((char *)buffer));
			else {
				struct passwd *pwd;
				pwd=getpwnam((char *)buffer);
				if (pwd) keySetUID(newKey,pwd->pw_uid);
				else fprintf(stderr,"%s: Ignoring invalid user %s.\n",
						newKey->key, buffer);
			}
			xmlFree(buffer); buffer=0;
		}
#endif

#ifdef HAVE_GRP_H
		/* Parse GID */
		buffer=xmlTextReaderGetAttribute(reader,(const xmlChar *)"gid");
		if (buffer) {
			if (isdigit(*buffer)) {
				keySetGID(newKey,atoi((char *)buffer));
			} else {
				struct group *grp;
				grp=getgrnam((char *)buffer);
				if (grp) keySetGID(newKey,grp->gr_gid);
				else fprintf(stderr,"%s: Ignoring invalid group %s.\n",
						newKey->key, buffer);
			}
			xmlFree(buffer); buffer=0;
		}
#endif

		/* Parse permissions */
		buffer=xmlTextReaderGetAttribute(reader,(const xmlChar *)"mode");
		if (buffer) keySetAccess(newKey,strtol((char *)buffer,0,0));
		xmlFree(buffer); buffer=0;



		if (xmlTextReaderIsEmptyElement(reader)) {
			/* we have a <key ..../> element */
			if (newKey && !appended) {
				ksAppend(ks,newKey);
				appended=1;
				end=1;
			}
		}


		buffer=xmlTextReaderGetAttribute(reader,(const xmlChar *)"type");
		if (buffer) {
			if (!strcmp((char *)buffer,"string"))
				type=KEY_TYPE_STRING;
			else if (!strcmp((char *)buffer,"link"))
				type=KEY_TYPE_LINK;
			else if (!strcmp((char *)buffer,"directory"))
				type=KEY_TYPE_DIR;
			else if (!strcmp((char *)buffer,"binary"))
				type=KEY_TYPE_BINARY;
			else if (!strcmp((char *)buffer,"undefined"))
				type=KEY_TYPE_UNDEFINED;
			else { /* special user-defined value types */
				void *converter=0;

				type=strtol((char *)buffer,(char **)&converter,10);
				if ((void *)buffer==converter)
					/* in case of error, fallback to default type again */
					type=KEY_TYPE_STRING;
			}
		}
		xmlFree(buffer); buffer=0;


		if (type == KEY_TYPE_DIR) {
			mode_t mask=umask(0);
			umask(mask);
			keySetDir(newKey,mask);
		} else keySetType(newKey,type);


		/* Parse everything else */
		while (!end) {
			xmlFree(nodeName); nodeName=0;
			xmlTextReaderRead(reader);
			nodeName=xmlTextReaderName(reader);

			if (!strcmp((char *)nodeName,"value")) {
				if (xmlTextReaderIsEmptyElement(reader) ||
					xmlTextReaderNodeType(reader)==15) continue;
					
				xmlTextReaderRead(reader);
				buffer=xmlTextReaderValue(reader);
				
				if (buffer) {
					/* Key's value type was already set above */
					if (KEY_TYPE_BINARY <= type && type < KEY_TYPE_STRING) {
						char *unencoded=0;
						size_t unencodedSize;
						
						unencodedSize=strblen((char *)buffer)/2;
						unencoded=malloc(unencodedSize);
						unencodedSize=unencode((char *)buffer,unencoded);
						if (!unencodedSize) return -1;
							keySetRaw(newKey,unencoded,unencodedSize);
						free(unencoded);
					} else keySetRaw(newKey,buffer,strblen((char *)buffer));
				}
			} else if (!strcmp((char *)nodeName,"comment")) {
				ssize_t commentSize=0;
				
				if (xmlTextReaderIsEmptyElement(reader) ||
					xmlTextReaderNodeType(reader)==15) continue;
					
				xmlTextReaderRead(reader);
				buffer=xmlTextReaderValue(reader);
				
				if ((commentSize=keyGetCommentSize(newKey)) > 0) {
					char *tmpComment=0;
					tmpComment=malloc(commentSize+
						xmlStrlen(buffer)*sizeof(xmlChar)+1);

					if (tmpComment) {
						keyGetComment(newKey,tmpComment,commentSize);

						strcat(tmpComment,"\n");
						strcat(tmpComment,(char *)buffer);

						keySetComment(newKey,tmpComment);

						free(tmpComment); tmpComment=0;
					}
				} else keySetComment(newKey,(char *)buffer);
			} else if (!strcmp((char *)nodeName,"key")) {
				/* Here we found </key> or a sub <key>.
				   So include current key in the KeySet. */
				if (newKey && !appended) {
					ksAppend(ks,newKey);
					appended=1;
				}
				
				if (xmlTextReaderNodeType(reader)==15)
					/* found a </key> */
					end=1;
				else {
					/* found a sub <key> */
					mode_t mask=umask(0);
					umask(mask);
					
					keySetDir(newKey,mask);
					/* prepare the context (parent) */
					consumeKeyNode(ks,newKey->key,reader);
				}
			}
			xmlFree(buffer); buffer=0;
		}

/*		printf("%s: %o\n",newKey->key,keyGetAccess(newKey)); */
		if (privateContext) xmlFree(privateContext);
	}

	if (nodeName) xmlFree(nodeName),nodeName=0;
	return 0;
}




int consumeKeySetNode(KeySet *ks, const char *context, xmlTextReaderPtr reader) {
	xmlChar *nodeName=0;
	xmlChar *privateContext=0;
	xmlChar fullContext[800]="";
	
	nodeName=xmlTextReaderName(reader);
	if (!strcmp((char *)nodeName,"keyset")) {
		int end=0;

		privateContext=xmlTextReaderGetAttribute(reader,(const xmlChar *)"parent");
		if (context && privateContext) {
			xmlStrPrintf(fullContext,sizeof(fullContext),
				(const xmlChar *)"%s/%s", context, privateContext);
		}

		/* Parse everything else */
		while (!end) {
			xmlFree(nodeName); nodeName=0;
			xmlTextReaderRead(reader);
			nodeName=xmlTextReaderName(reader);

			if (!strcmp((char *)nodeName,"key")) {
				if (privateContext) consumeKeyNode(ks,(char *)(*fullContext?fullContext:privateContext),reader);
				else consumeKeyNode(ks,context,reader);
			} else if (!strcmp((char *)nodeName,"keyset")) {
				/* A <keyset> can have nested <keyset>s */
				if (xmlTextReaderNodeType(reader)==15)
					/* found a </keyset> */
					end=1;
				else if (privateContext)
					consumeKeySetNode(ks, (char *)(*fullContext?fullContext:privateContext), reader);
				else consumeKeySetNode(ks, context, reader);
			}
		}
		if (privateContext) xmlFree(privateContext),privateContext=0;
	}
	return 0;
}



/*
 * This is the workhorse behind for ksFromXML() and ksFromXMLfile().
 * It will process the entire XML document in reader and convert and
 * save it in ks KeySet. Each node is processed by the processNode() function.
 *
 * This function is completelly dependent on libxml.
 */
int ksFromXMLReader(KeySet *ks,xmlTextReaderPtr reader) {
	int ret;
	xmlChar *nodeName=0;

	ret = xmlTextReaderRead(reader); /* go to first node */
	while (ret == 1) {
		/* walk node per node until the end of the stream */
		nodeName=xmlTextReaderName(reader);
		
		if (!strcmp((char *)nodeName,"key"))
			consumeKeyNode(ks, 0, reader);
		else if (!strcmp((char *)nodeName,"keyset"))
			consumeKeySetNode(ks, 0, reader);
		
		ret = xmlTextReaderRead(reader);
	}
	xmlFreeTextReader(reader);
	if (ret) fprintf(stderr,"kdb: Failed to parse XML input\n");

	return ret;
}



int isValidXML(xmlDocPtr doc,char *schemaPath) {
	xmlSchemaPtr wxschemas = NULL;
	xmlSchemaValidCtxtPtr ctxt;
	xmlSchemaParserCtxtPtr ctxt2=NULL;
	int ret=0;

	ctxt2 = xmlSchemaNewParserCtxt(schemaPath);


	if (ctxt2==NULL) {
		xmlFreeDoc(doc);
		return 1;
	}
	
	xmlSchemaSetParserErrors(ctxt2,
		(xmlSchemaValidityErrorFunc) fprintf,
		(xmlSchemaValidityWarningFunc) fprintf,
		stderr);
	wxschemas = xmlSchemaParse(ctxt2);
	
	if (wxschemas==NULL) {
		xmlSchemaFreeParserCtxt(ctxt2);
		xmlFreeDoc(doc);
		return 1;
	}
	
	/* try to validate the doc against the xml schema */
	ctxt = xmlSchemaNewValidCtxt(wxschemas);
	xmlSchemaSetValidErrors(ctxt,
		(xmlSchemaValidityErrorFunc) fprintf,
		(xmlSchemaValidityWarningFunc) fprintf,
		stderr);
	
	if (ctxt==NULL) {
		xmlSchemaFree(wxschemas);
		xmlSchemaFreeParserCtxt(ctxt2);
		xmlFreeDoc(doc);
		return 1;
	}
	
	ret = xmlSchemaValidateDoc(ctxt, doc);
	xmlSchemaFreeValidCtxt(ctxt);
	xmlSchemaFree(wxschemas);
	xmlSchemaFreeParserCtxt(ctxt2);

	return ret;
}



/**
 * Given an XML @p filename, open it, validate schema, process nodes,
 * convert and save it in the @p ks KeySet.
 * 
 * Currently, the XML file can have many root <keyset> and <key> nodes.
 * They will all be reduced to simple keys returned in @p ks.
 * 
 * @ingroup tools
 */
int ksFromXMLfile(KeySet *ks,const char *filename) {
	xmlTextReaderPtr reader;
	xmlDocPtr doc;
	int ret=0;
	char schemaPath[513];

	doc = xmlParseFile(filename);
	if (doc==NULL) return 1;
	
	/* Open the kdb to get the xml schema path */
	schemaPath[0]=0;
	// ret=kdbGetValue(KDB_SCHEMA_PATH_KEY,schemaPath,sizeof(schemaPath));

//	if (ret==0) ret = isValidXML(filename,schemaPath);
//	else ret = isValidXML(filename,KDB_SCHEMA_PATH); /* fallback to builtin */

	
	/* if the validation was successful */
	if (!ret) {
		reader=xmlReaderWalker(doc);
		if (reader) ret=ksFromXMLReader(ks,reader);
		else {
			perror("kdb");
			return 1;
		}
	}
	xmlFreeDoc(doc);
	return ret;
}





/* FIXME: not working when fd is stdin */
/**
 * Given a file descriptor (that can be @p stdin) for an XML file, validate
 * schema, process nodes, convert and save it in the @p ks KeySet.
 * @ingroup tools
 */
int ksFromXML(KeySet *ks,int fd) {
	/* Start of support for old XML library (no xmlReaderForFd()) */
	char filename[]="/var/tmp/kdbeditXXXXXX";
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

	/* This code requires a newer version of XML library, not present in all
	   Linux/BSD/Unix distros. Don't use it yet.
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







