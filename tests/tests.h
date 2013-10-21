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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
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

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#include <kdb.h>

#define BUFFER_LENGTH 4096

extern int nbError;
extern int nbTest;

extern uid_t nbUid;
extern gid_t nbGid;

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

#define check_attributes(attributes) \
{ \
	succeed_if ((attributes & KEY_NAME) == 0 , "compare key: NAME not equal"); \
	succeed_if ((attributes & KEY_VALUE) == 0 , "compare key: VALUE not equal"); \
	succeed_if ((attributes & KEY_OWNER) == 0 , "compare key: OWNER not equal"); \
	succeed_if ((attributes & KEY_COMMENT) == 0 , "compare key: COMMENT not equal"); \
	succeed_if ((attributes & KEY_UID) == 0 , "compare key: UID not equal"); \
	succeed_if ((attributes & KEY_GID) == 0 , "compare key: GID not equal"); \
	succeed_if ((attributes & KEY_MODE ) == 0 , "compare key: MODE  not equal"); \
	succeed_if ((attributes & KEY_NULL ) == 0, "compare key: one of the keys is null"); \
}

#define quote_string(x) #x

/**
 * Checks if two keys are equal.
 *
 */
#define compare_key(k1, k2) \
{ \
	nbTest++; \
	if (k1 != k2) \
	{ \
		keyswitch_t attributes = keyCompare(k1, k2); \
		check_attributes(attributes); \
	 \
		const Key * meta; \
		keyRewindMeta(k1); \
		keyRewindMeta(k2); \
		while ((meta = keyNextMeta (k1)) != 0) \
		{ \
			const Key const * metaCmp = keyNextMeta(k2); \
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
			attributes = keyCompare(meta, metaCmp); \
			check_attributes(attributes); \
		} \
	 \
		const Key const * metaCmp = keyNextMeta(k2); \
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

Key * create_root_key (const char *backendName);
KeySet *create_conf (const char *filename);

void output_meta(Key *k);
void output_key (Key *ks);
void output_keyset (KeySet *ks);

int output_warnings(Key *errorKey);
int output_errors(Key *errorKey);

int check_for_error(Key *errorKey);
int check_for_error_and_warnings(Key *errorKey);

#endif
