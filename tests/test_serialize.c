/***************************************************************************
 *          test_serialize.c  - serializing test suite
 *                  -------------------
 *  begin                : Mon 26 Nov 2007
 *  copyright            : (C) 2007 by Markus Raab
 *  email                : elektra@markus-raab.org
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests.h>

#define MAX_SIZE 1000

void output_serialize (void* data, size_t length)
{
	size_t i;
	for (i=0; i<length; i++)
	{
		printf ("%o %c ", ((char*)data)[i], ((char*)data)[i]);
	}
	printf ("\n");
}

void test_emptykey ()
{
	char buffer[MAX_SIZE+1];
	Key *key;
	Key *get;

	printf("Test empty key\n");

	key = keyNew (0);
	// printf ("%d\n", keyGetSerializedSize(key));
	// succeed_if( keyGetSerializedSize(key) == 68, "empty key size wrong (32bit)");
	keySerialize (key, buffer, MAX_SIZE);
	// output_serialize (buffer, keyGetSerializedSize (key));
	get = keyUnserialize (buffer);
	// keyOutput (get);
	// keyGenerate (get);
	succeed_if (!compare_key (key,get,0), "did not get the same key after serializing");
	keyDel (key);
	keyDel (get);

	key = keyNew (0);
	// succeed_if( keyGetSerializedSize(key) == 68, "empty key size wrong (32bit)");
	keySerialize (key, buffer, MAX_SIZE);
	get = keyCompose (buffer);
	// keyOutput (get);
	// keyGenerate (get);
	succeed_if (!compare_key (key,get,0), "did not get the same key after serializing");
	keyDel (key);
}

void test_allkey ()
{
	char buffer[MAX_SIZE+1];
	Key *key;
	Key *get;

	printf("Test key with value/comment\n");

	key = keyNew ("user",
		KEY_VALUE, "value",
		KEY_COMMENT, "comment",
		KEY_END);
	// printf ("%d\n", keyGetSerializedSize(key));
	// succeed_if( keyGetSerializedSize(key) == 94, "empty key size wrong (32bit)");
	keySerialize (key, buffer, MAX_SIZE);
	// output_serialize (buffer, keyGetSerializedSize (key));
	get = keyUnserialize (buffer);
	// keyOutput (get);
	// keyGenerate (get);
	succeed_if (!compare_key (key,get,0), "did not get the same key after serializing");
	keyDel (key);
	keyDel (get);

	key = keyNew ("user",
		KEY_VALUE, "value",
		KEY_COMMENT, "comment",
		KEY_END);
	// succeed_if( keyGetSerializedSize(key) == 94, "empty key size wrong (32bit)");
	keySerialize (key, buffer, MAX_SIZE);
	get = keyCompose (buffer);
	// keyOutput (get);
	// keyGenerate (get);
	succeed_if (!compare_key (key,get,0), "did not get the same key after serializing");
	keyDel (key);
}

void test_xml(char *file)
{
	char buffer[MAX_SIZE+1];
	Key *cur;
	Key *key;
	Key *get;
	KeySet *ks = ksNew (0);

	exit_if_fail( ksFromXMLfile(ks, file) == 0, "ksFromXMLfile failed.");

	printf("Test key from xml file %s\n", file);

	ksRewind(ks);
	ksSort(ks);
	while ( (cur = ksNext(ks)) )
	{
		key = keyDup (cur);
		keySerialize (key, buffer, MAX_SIZE);
		// output_serialize (buffer, keyGetSerializedSize (key));
		get = keyUnserialize (buffer);
		// keyOutput (get);
		// keyGenerate (get);
		succeed_if (!compare_key (key,get,0), "did not get the same key after serializing");
		keyDel (get);
		keyDel (key);

		key = keyDup (cur);
		keySerialize (key, buffer, MAX_SIZE);
		get = keyCompose (buffer);
		// keyOutput (get);
		// keyGenerate (get);
		succeed_if (!compare_key (key,get,0), "did not get the same key after serializing");
		keyDel (key);
	}
	ksDel (ks);
}

int main(int argc, char** argv)
{
	printf("SERIALIZE TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_emptykey();
	test_allkey();
	test_xml(srcdir_file("key.xml"));
	test_xml(srcdir_file("keyset.xml"));
	printf("\ntest_serialize RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
