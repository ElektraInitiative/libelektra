/**@file
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
#define ELEKTRA_TEST_ROOT "/tests/ckdb/"

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
		printf("%s:%d: warn in %s: %s\n", __FILE__, __LINE__, __func__, message); \
	} \
}

#define yield_error(message) \
{ \
	nbError++; \
	printf("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __func__, message); \
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
		printf("%s:%d: fatal in %s: %s\n", __FILE__, __LINE__, __func__, message); \
		exit(1); \
	} \
}

#define quote_string(x) #x

#define compare_key_name(pk1, pk2) \
{ \
	Key *nmmk1 = pk1; \
	Key *nmmk2 = pk2; \
	nbTest++; \
	if (strcmp(keyName(nmmk1), keyName(nmmk2))) \
	{ \
		char errorMsg [BUFFER_LENGTH]; \
		 \
		strcpy(errorMsg, "key name "); \
		strcat(errorMsg, keyName(nmmk1)); \
		strcat(errorMsg, " is not equal "); \
		strcat(errorMsg, keyName(nmmk2)); \
		 \
		yield_error(errorMsg); \
	} \
}

#define compare_key_string(pk1, pk2) \
{ \
	Key *smmk1 = pk1; \
	Key *smmk2 = pk2; \
	nbTest++; \
	if (strcmp(keyString(smmk1), keyString(smmk2))) \
	{ \
		char errorMsg [BUFFER_LENGTH]; \
		 \
		strcpy(errorMsg, "key value "); \
		strcat(errorMsg, keyName(smmk1)); \
		strcat(errorMsg, " is not equal "); \
		strcat(errorMsg, keyName(smmk2)); \
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
#define ELEKTRA_PRAGMA(x)  _Pragma(ELEKTRA_PRAGMA_STR(x))
#else
#define ELEKTRA_PRAGMA(x)
#endif
#define ELEKTRA_PRAGMA_STR(x) #x
#define ELEKTRA_DIAG_STORE    ELEKTRA_PRAGMA(GCC diagnostic push)
#define ELEKTRA_DIAG_OFF(x)   ELEKTRA_PRAGMA(GCC diagnostic ignored ELEKTRA_PRAGMA_STR(x))
#define ELEKTRA_DIAG_RESTORE  ELEKTRA_PRAGMA(GCC diagnostic pop)

#define succeed_if_same_string(ps1, ps2) \
{ \
	nbTest++; \
	const char* s1 = ps1; \
	const char* s2 = ps2; \
	ELEKTRA_DIAG_STORE \
	ELEKTRA_DIAG_OFF(-Waddress) \
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
	ELEKTRA_DIAG_RESTORE \
}

// only works with types convertible to int
#define succeed_if_same_int(s1, s2) \
{ \
	nbTest++; \
	int s1 = ps1; \
	int s2 = ps2; \
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
 * Warning: messes up internal cursor
 *
 */
#define compare_key(pk1, pk2) \
{ \
	nbTest++; \
	Key *mmk1 = (Key*) pk1; \
	Key *mmk2 = (Key*) pk2; \
	if (mmk1 != mmk2) \
	{ \
		compare_key_name(mmk1, mmk2); \
		 \
		compare_key_string(mmk1, mmk2); \
		 \
		const Key * meta; \
		keyRewindMeta(mmk1); \
		keyRewindMeta(mmk2); \
		while ((meta = keyNextMeta (mmk1)) != 0) \
		{ \
			const Key *const metaCmp = keyNextMeta(mmk2); \
			if (metaCmp == 0) \
			{ \
				nbError++; \
				printf("%s:%d: error in %s: Compare key \"%s\" with \"%s\" failed, did not find corresponding meta key %s (k1 > k2)\n", \
					__FILE__, __LINE__, __func__, \
					quote_string(mmk1), \
					quote_string(mmk2), \
					keyName(meta) \
					); \
				break; \
			} \
		} \
	 \
		const Key *const metaCmp = keyNextMeta(mmk2); \
		if (metaCmp != 0) \
		{ \
			nbError++; \
			printf("%s:%d: error in %s: Compare key \"%s\" with \"%s\" failed, too many meta keys found (k1 < k2)\n", \
				__FILE__, __LINE__, __func__, \
				quote_string(mmk1), \
				quote_string(mmk2) \
				); \
		} \
	} \
}


/**Compare two keysets.
 *
 * Compare if two keysets contain the same keys.
 * @retval 0 on success
 * */
#define compare_keyset(pks1, pks2) \
{ \
	nbTest++; \
	KeySet *mmks1 = (KeySet*) pks1; \
	KeySet *mmks2 = (KeySet*) pks2; \
	if (mmks1 != mmks2) \
	{ \
		Key	*cmmk1 = 0; \
		Key     *cmmk2 = 0; \
 \
		if (ksGetSize (mmks1) == 0) yield_error("real size of " quote_string(mmks1) " was 0"); \
		if (ksGetSize (mmks2) == 0) yield_error("real size of " quote_string(mmks2) " was 0"); \
 \
		if (ksGetSize (mmks1) != ksGetSize(mmks2) ) \
		 { \
			nbError++; \
			printf("%s:%d: error in %s: Compare keyset failed, size of keysets are not equal with size(%s): %d, size(%s): %d\n", \
				__FILE__, __LINE__, __func__, quote_string(mmks1), (int)ksGetSize(mmks1), quote_string(mmks2), (int)ksGetSize(mmks2)); \
			printf ("mmks1:\n"); \
			output_keyset(mmks1); \
			printf ("mmks2:\n"); \
			output_keyset(mmks2); \
		} \
		else \
		{ \
 \
			ksRewind(mmks1); \
			ksRewind(mmks2); \
 \
			while ((cmmk1 = ksNext(mmks1)) != 0) \
			{ \
				cmmk2 = ksNext(mmks2); \
				if (!cmmk2) \
				{ \
					yield_error("Compare keyset " quote_string(mmks1) " with " quote_string(mmks2) " failed, did not find corresponding key") \
					break; \
				} \
 \
				compare_key (cmmk1, cmmk2); \
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
