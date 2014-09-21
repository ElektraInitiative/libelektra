/**\file
  * Some common functions in use for testing framework.
  * @see tests_internal.h
  */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef TESTS_H
#define TESTS_H

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <kdb.h>
#include <kdbhelper.h>

#define BUFFER_LENGTH 4096

extern int nbError;
extern int nbTest;

extern uid_t nbUid;
extern gid_t nbGid;

extern char *tempHome;
extern int tempHomeLen;

int init(int argc, char** argv);

#define warn_if_fail(expression, message) \
{ \
	nbTest++; \
	if (!(expression)) \
	{ \
		printf("%s:%d: warn in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, message); \
	} \
}

#define yield_error(message) \
{ \
       nbError++; \
       printf("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, message); \
}

#define succeed_if(expression, message) \
{ \
	nbTest++; \
	if (!(expression)) \
	{ \
		yield_error(message); \
	} \
}

#define exit_if_fail(expression, message) \
{ \
	nbTest++; \
	if (!(expression)) \
	{ \
		printf("%s:%d: fatal in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, message); \
		exit(1); \
	} \
}

#define quote_string(x) #x

#define compare_key_name(k1, k2) \
{ \
	nbTest++; \
	if (strcmp(keyName(k1), keyName(k2))) \
	{ \
		char errorMsg [BUFFER_LENGTH]; \
		 \
		strcpy(errorMsg, "key name "); \
		strcat(errorMsg, keyName(k1)); \
		strcat(errorMsg, " is not equal "); \
		strcat(errorMsg, keyName(k2)); \
		 \
		yield_error(errorMsg); \
	} \
}

#define compare_key_string(k1, k2) \
{ \
	nbTest++; \
	if (strcmp(keyString(k1), keyString(k2))) \
	{ \
		char errorMsg [BUFFER_LENGTH]; \
		 \
		strcpy(errorMsg, "key name "); \
		strcat(errorMsg, keyName(k1)); \
		strcat(errorMsg, " is not equal "); \
		strcat(errorMsg, keyName(k2)); \
		 \
		yield_error(errorMsg); \
	} \
}

//GCC diagnostic not allowed inside functions
//does not work with 4.4
//4.6 and 4.7 as in debian work with it, that are:
//4.6.3
//4.7.2
//(https://gcc.gnu.org/bugzilla/show_bug.cgi?id=52116)
#if __GNUC__ > 4 || \
	(__GNUC__ == 4 && (__GNUC_MINOR__ > 6 || (__GNUC_MINOR__ == 6 && __GNUC_PATCHLEVEL__ > 2))) || \
	(__GNUC__ == 4 && (__GNUC_MINOR__ > 7 || (__GNUC_MINOR__ == 7 && __GNUC_PATCHLEVEL__ > 1)))
#define ELEKTRA_GCC_WARNING(x) _Pragma(ELEKTRA_GCC_HELPER2(x))
#define ELEKTRA_GCC_HELPER2(y) ELEKTRA_GCC_HELPER1(#y)
#define ELEKTRA_GCC_HELPER1(x) ELEKTRA_GCC_HELPER0(GCC diagnostic ignored x)
#define ELEKTRA_GCC_HELPER0(x) #x
#else
#define ELEKTRA_GCC_WARNING(x)
#endif

#define succeed_if_same_string(s1, s2) \
{ \
	nbTest++; \
	ELEKTRA_GCC_WARNING(-Waddress) \
	if (!s1) yield_error("left hand side is null pointer") \
	else if (!s2) yield_error("right hand side is null pointer") \
	else if (strcmp(s1, s2)) \
	{ \
		char errorMsg [BUFFER_LENGTH]; \
		 \
		strcpy(errorMsg, "string \""); \
		strcat(errorMsg, s1); \
		strcat(errorMsg, "\" is not equal \""); \
		strcat(errorMsg, s2); \
		strcat(errorMsg, "\""); \
		 \
		yield_error(errorMsg); \
	} \
	_Pragma("GCC diagnostic pop") \
}

// not recommended to use, only works with int (not size_t, ssize_t,...)
#define succeed_if_same_int(s1, s2) \
{ \
	nbTest++; \
	if (s1 != s2) \
	{ \
		char errorMsg [BUFFER_LENGTH]; \
		 \
		snprintf(errorMsg, BUFFER_LENGTH, \
			 "int %d is not equal %d", s1, s2); \
		 \
		yield_error(errorMsg); \
	} \
}



/**
 * Checks if two keys are equal.
 *
 */
#define compare_key(k1, k2) \
{ \
	nbTest++; \
	if (k1 != k2) \
	{ \
		compare_key_name(k1, k2); \
		 \
		compare_key_string(k1, k2); \
		 \
		const Key * meta; \
		keyRewindMeta(k1); \
		keyRewindMeta(k2); \
		while ((meta = keyNextMeta (k1)) != 0) \
		{ \
			const Key *const metaCmp = keyNextMeta(k2); \
			if (metaCmp == 0) \
			{ \
				nbError++; \
				printf("%s:%d: error in %s: Compare key \"%s\" with \"%s\" failed, did not find corresponding meta key %s (k1 > k2)\n", \
					__FILE__, __LINE__, __FUNCTION__, \
					quote_string(k1), \
					quote_string(k2), \
					keyName(meta) \
					); \
				break; \
			} \
		} \
	 \
		const Key *const metaCmp = keyNextMeta(k2); \
		if (metaCmp != 0) \
		{ \
			nbError++; \
			printf("%s:%d: error in %s: Compare key \"%s\" with \"%s\" failed, too many meta keys found (k1 < k2)\n", \
				__FILE__, __LINE__, __FUNCTION__, \
				quote_string(k1), \
				quote_string(k2) \
				); \
		} \
	} \
}


/**Compare two keysets.
 *
 * Compare if two keysets contain the same keys.
 * @return 0 on success
 * */
#define compare_keyset(ks1, ks2) \
{ \
	nbTest++; \
	if (ks1 != ks2) \
	{ \
		Key	*key1 = 0; \
		Key     *key2 = 0; \
 \
		if (ksGetSize (ks1)  == 0) yield_error("real size of " quote_string(ks1) " was 0"); \
		if (ksGetSize (ks2) == 0) yield_error("real size of " quote_string(ks2) " was 0"); \
 \
		if (ksGetSize (ks1) != ksGetSize(ks2) ) \
		 { \
			nbError++; \
			printf("%s:%d: error in %s: Compare keyset failed, size of keysets are not equal with size(%s): %d, size(%s): %d\n", \
				__FILE__, __LINE__, __FUNCTION__, quote_string(ks1), (int)ksGetSize(ks1), quote_string(ks2), (int)ksGetSize(ks2)); \
		} \
		else \
		{ \
 \
			ksRewind(ks1); \
			ksRewind(ks2); \
 \
			while ((key1 = ksNext(ks1)) != 0) \
			{ \
				key2 = ksNext(ks2); \
				if (!key2) \
				{ \
					yield_error("Compare keyset " quote_string(ks1) " with " quote_string(ks2) " failed, did not find corresponding key") \
					break; \
				} \
 \
				compare_key (key1, key2); \
			} \
		} \
	} \
}

int compare_files (const char * filename);
int compare_line_files (const char *filename, const char *genfilename);

char *srcdir_file(const char * fileName);
const char *elektraFilename();
void elektraUnlink(const char* filename);

Key * create_root_key (const char *backendName);
KeySet *create_conf (const char *filename);

void output_meta(Key *k);
void output_key (Key *ks);
void output_keyset (KeySet *ks);

int output_warnings(Key *errorKey);
int output_error(Key *errorKey);

#endif
