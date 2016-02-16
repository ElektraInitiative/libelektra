/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif


#include <langinfo.h>

#include <tests.h>

#include "validation.h"

#define NR_KEYS 4

void test_lookupre()
{
	KeySet *ks = ksNew (5,
			keyNew ("user/a", KEY_VALUE, "a", KEY_COMMENT, "does not match", KEY_END),
			keyNew ("user/b", KEY_VALUE, "  a  ", KEY_COMMENT, "does not match", KEY_END),
			keyNew ("user/c", KEY_VALUE, "\t\t", KEY_COMMENT, "match", KEY_END),
			keyNew ("user/d", KEY_VALUE, " \t \t ", KEY_COMMENT, "match", KEY_END),
			KS_END);

	Key *match = 0;
	regex_t regex;

	regcomp(&regex,"^[ \t]*$",REG_NOSUB);

	// we start from the first key
	ksRewind(ks);

	// show the key that match this string
	match=ksLookupRE(ks,&regex);
	succeed_if (!strcmp(keyName(match), "user/c"), "Key did not match");

	match=ksLookupRE(ks,&regex);
	succeed_if (!strcmp(keyName(match), "user/d"), "Key did not match");

	regfree(&regex); // free regex resources
	ksDel (ks);
}

void test_extended()
{
	KeySet *ks = ksNew (5,
			keyNew ("user/a", KEY_VALUE, "la", KEY_COMMENT, "match", KEY_END),
			keyNew ("user/b", KEY_VALUE, "lalala", KEY_COMMENT, "match", KEY_END),
			keyNew ("user/c", KEY_VALUE, "jump", KEY_COMMENT, "does not match", KEY_END),
			keyNew ("user/d", KEY_VALUE, "lalalala", KEY_COMMENT, "match", KEY_END),
			KS_END);

	Key *match = 0;
	regex_t regex;

	regcomp(&regex,"^(la)+$", REG_NOSUB|REG_EXTENDED);

	// we start from the first key
	ksRewind(ks);

	// show the key that match this string
	match=ksLookupRE(ks,&regex);
	succeed_if (!strcmp(keyName(match), "user/a"), "Key did not match");

	match=ksLookupRE(ks,&regex);
	succeed_if (!strcmp(keyName(match), "user/b"), "Key did not match");

	match=ksLookupRE(ks,&regex);
	succeed_if (!strcmp(keyName(match), "user/d"), "Key did not match");

	regfree(&regex); // free regex resources
	ksDel (ks);
}

int main(int argc, char** argv)
{
	printf("   ICONV   TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_lookupre();
	test_extended();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

