/*************************************************************************** 
 *           test_mount.c  - Test suite for testing backend mounting
 *                  -------------------
 *  begin                : Thu Nov 6 2007
 *  copyright            : (C) 2007 by Patrick Sabin
 *  email                : patricksabin@gmx.at
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

KeySet *get_hosts ()
{
	return
	ksNew( 25 ,
	keyNew ("user/tests/hosts"
		, KEY_DIR
		, KEY_TYPE, 0
	, KEY_END),
	keyNew ("user/tests/hosts/gateway.markus-raab.org"
		, KEY_DIR
		, KEY_VALUE, "192.168.0.1"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/gateway.markus-raab.org/alias00"
		, KEY_VALUE, "gateway"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/gateway.markus-raab.org/alias01"
		, KEY_VALUE, "gw"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/home.markus-raab.org"
		, KEY_DIR
		, KEY_VALUE, "81.52.51.112"
		, KEY_COMMENT, "comment"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/home.markus-raab.org/alias00"
		, KEY_VALUE, "home"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/kirabyte.markus-raab.org"
		, KEY_DIR
		, KEY_VALUE, "192.168.0.5"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/kirabyte.markus-raab.org/alias00"
		, KEY_VALUE, "kirabyte"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/kirabyte.markus-raab.org/alias01"
		, KEY_VALUE, "kira"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/kirabyte.markus-raab.org/alias02"
		, KEY_VALUE, "k"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/localhost"
		, KEY_VALUE, "127.0.0.1"
		, KEY_COMMENT, "will lose that comment"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/markusbyte"
		, KEY_DIR
		, KEY_VALUE, "192.168.0.3"
		, KEY_COMMENT, "this is my home"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/markusbyte/alias00"
		, KEY_VALUE, "markus"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/printer"
		, KEY_VALUE, "192.168.1.5"
		, KEY_COMMENT, " thats for dynamic cups printing"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/superbyte"
		, KEY_VALUE, "192.168.0.2"
		, KEY_COMMENT, " my server at home!"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),KS_END);
}

#define BUFFER_SIZE 256

void test_failhosts(const char * file)
{
	KeySet *ks = ksNew( 25 ,
	keyNew ("user/tests/hosts"
		, KEY_DIR
		, KEY_TYPE, 0
	, KEY_END),
	keyNew ("user/tests/hosts/gateway.markus-raab.org"
		, KEY_DIR
		, KEY_VALUE, "192.168.0.1"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/gateway.markus-raab.org/ERROR/DEEP"
		, KEY_VALUE, "errorvalue"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/superbyte"
		, KEY_VALUE, "192.168.0.2"
		, KEY_COMMENT, " my server at home!"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),KS_END);
	KDB *kdb = kdbOpen();
	Key *mnt;
	KeySet *conf;


	printf("Test fail writehosts\n");

	succeed_if (kdbMount(kdb,mnt=keyNew("user/tests/hosts",KEY_VALUE,"hosts", KEY_END),
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, file, KEY_END), KS_END)) == 0,
		"could not mount hosts");
	succeed_if (kdbSet(kdb,ks,keyNew("user/tests/hosts",KEY_END),KDB_O_DEL) == -1, "setting keys did not fail");

	/*keyOutput (ksCurrent(ks), stdout, KEY_VALUE|KEY_COMMENT);*/
	succeed_if (strcmp(keyName(ksCurrent(ks)), "user/tests/hosts/gateway.markus-raab.org/ERROR/DEEP") == 0, "name not equal");
	succeed_if (strcmp(keyValue(ksCurrent(ks)), "errorvalue") == 0, "value not equal");
	succeed_if (!keyIsDir (ksCurrent(ks)), "key is a directory");
	succeed_if (keyIsString (ksCurrent(ks)), "key is not a string");
	succeed_if (keyGetType(ksCurrent(ks)) == KEY_TYPE_STRING, "key is not exactly a string");

	ksDel (conf);
	keyDel(mnt);

	/*
	ksOutput (ks, stdout, 0);
	ksGenerate (ks, stdout, 0);
	*/

	ksDel (ks);
	kdbClose (kdb);
}

void test_readhosts(const char * file)
{
	char *keys[]={	"user/tests/hosts/markusbyte",
			"user/tests/hosts/printer",
			"user/tests/hosts/superbyte"};
	KDB *kdb = kdbOpen();
	Key *mnt;
	Key *key;
	KeySet *ks=ksNew(0);
	KeySet *conf;
	KeySet *hosts_ks = get_hosts();
	char buffer[BUFFER_SIZE+1];
	int i, c;

	printf("Test mount readhosts\n");

	succeed_if (kdbMount(kdb,mnt=keyNew("user/tests/hosts",KEY_VALUE,"hosts", KEY_END),
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, file, KEY_END), KS_END)) == 0,
		"could not mount hosts");
	succeed_if (kdbGet(kdb,ks,keyNew("user/tests/hosts",KEY_END),KDB_O_DEL) == 15, "could not get keys");
	/*
	ksGenerate (hosts_ks, stdout, KDB_O_HEADER);
	ksGenerate (ks, stdout, KDB_O_HEADER);
	*/
	compare_keyset (hosts_ks, ks, 0, 0);

	key = keyNew ("user/tests/hosts/gateway.markus-raab.org", KEY_END);
	succeed_if (kdbGetKey (kdb, key) == 0, "could not get single key");
	succeed_if (strcmp(keyName(key), "user/tests/hosts/gateway.markus-raab.org") == 0, "name not equal");
	succeed_if (strcmp(keyValue(key), "192.168.0.1") == 0, "value not equal");
	succeed_if (keyIsDir (key), "key is not a directory");
	succeed_if (keyIsString (key), "key is not a string");
	succeed_if (keyGetType(key) == KEY_TYPE_STRING, "key is not exactly a string");
	keyDel (key);

	key = keyNew ("user/tests/hosts/kirabyte.markus-raab.org/alias01", KEY_END);
	succeed_if (kdbGetKey (kdb, key) == 0, "could not get single key");
	succeed_if (strcmp(keyName(key), "user/tests/hosts/kirabyte.markus-raab.org/alias01") == 0, "name not equal");
	succeed_if (strcmp(keyValue(key), "kira") == 0, "value not equal");
	succeed_if (!keyIsDir (key), "key is a directory");
	succeed_if (keyIsString (key), "key is not a string");
	succeed_if (keyGetType(key) == KEY_TYPE_STRING, "key is not exactly a string");
	keyDel (key);

	key = keyNew ("user/tests/hosts/localhost", KEY_END);
	succeed_if (kdbGetKey (kdb, key) == 0, "could not get single key");
	succeed_if (strcmp(keyName(key), "user/tests/hosts/localhost") == 0, "name not equal");
	succeed_if (strcmp(keyValue(key), "127.0.0.1") == 0, "value not equal");
	succeed_if (strcmp(keyComment(key), "will lose that comment") == 0, "comment not equal");
	succeed_if (!keyIsDir (key), "key is a directory");
	succeed_if (keyIsString (key), "key is not a string");
	succeed_if (keyGetType(key) == KEY_TYPE_STRING, "key is not exactly a string");
	keyDel (key);

	succeed_if (kdbGetString (kdb, "user/tests/hosts/localhost", buffer, BUFFER_SIZE) != -1, "could not get value");
	succeed_if (strcmp (buffer, "127.0.0.1") == 0, "value not correct");


	for (c=0; c<3; c++) {
		succeed_if (kdbGetString(kdb,keys[c],buffer,sizeof(buffer)) != -1, "could not GetString");
		switch (c)
		{
			case 0:	succeed_if (strcmp(buffer, "192.168.0.3") == 0, "value not correct");
				break;
			case 1:	succeed_if (strcmp(buffer, "192.168.1.5") == 0, "value not correct");
				break;
			case 2:	succeed_if (strcmp(buffer, "192.168.0.2") == 0, "value not correct");
				break;
			default:succeed_if (0, "default branch");
		}
	}

	for (i=0; i< BUFFER_SIZE; i++) buffer[i] = 0;
	succeed_if (kdbGetString (kdb, "user/tests/hosts/localhost", buffer, 3) == -1, "could get value");
	/*succeed_if (errno == KDB_ERR_TRUNC, "error not correct");*/
	/*printf ("%s\n", buffer);*/
	succeed_if (strcmp (buffer, "") == 0, "value not correct");

	ksDel (conf);
	keyDel(mnt);

	/*ksOutput (ks, stdout, 0);
	ksGenerate (ks, stdout, 0);*/

	ksDel (hosts_ks);
	ksDel (ks);
	kdbClose (kdb);
}

void test_writehosts(const char *file)
{
	KDB *kdb = kdbOpen();
	Key *mnt;
	KeySet *conf;
	KeySet *ks = get_hosts();


	printf("Test mount writehosts\n");

	succeed_if (kdbMount(kdb,mnt=keyNew("user/tests/hosts",KEY_VALUE,"hosts", KEY_END),
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, file, KEY_END), KS_END)) == 0,
		"could not mount hosts");
	succeed_if (kdbSet(kdb,ks,keyNew("user/tests/hosts",KEY_END),KDB_O_DEL) == 15, "could not set keys");
	ksDel (conf);
	keyDel(mnt);

	/*ksOutput (ks, stdout, 0);
	ksGenerate (ks, stdout, 0);*/

	ksDel (ks);
	kdbClose (kdb);
}

void test_passwd(const char *file)
{
	KDB *kdb = kdbOpen();
	KeySet *ks;

	printf("Test mount passwd\n");

	/* get passwd*/
	printf("=====TEST passwd backend ====\n");
	ks=ksNew(0);
	kdbGet(kdb,ks,keyNew("system/users",KEY_END),KDB_O_DEL);
	/* ksOutput(ks, stdout, 0); */
	ksDel(ks);

	ks=ksNew(0);
	kdbGet(kdb,ks,keyNew("system/users/patrick",KEY_END),KDB_O_DEL);
	/* ksOutput(ks,stdout, 0); */
	ksDel(ks);

	kdbClose (kdb);

}

void test_readfstab(const char *file)
{
	KDB *kdb = kdbOpen();
	KeySet *ks;
	KeySet *conf;
	Key* mnt;

	printf("Test mount readfstab\n");

	succeed_if (kdbMount (kdb, mnt=keyNew("user/tests/filesystems",KEY_VALUE, "fstab", KEY_END),
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, file, KEY_END), KS_END)) == 0,
		"could not mount fstab");
	keyDel(mnt);
	ksDel (conf);

	ks=ksNew(0);
	int i;
	succeed_if ((i=kdbGet(kdb,ks,keyNew("user/tests/filesystems",KEY_END),KDB_O_DEL)) == 15, "could not get keys");

	/*
	printf ("%d", i);
	ksOutput (ks, stdout, 0);
	ksGenerate (ks,stdout, 0);
	*/
	Key *key = ksLookupByName(ks, "user/tests/filesystems/rootfs/device",0);
	succeed_if (key, "rootfs device not found");
	succeed_if (strcmp( "/dev/sda6", keyValue(key)) == 0, "rootfs device not correct");

	ksDel (ks);
	kdbClose (kdb);
}


void test_writefstab(const char * file)
{
	KDB *kdb = kdbOpen();
	KeySet *ks;
	KeySet *conf;
	Key* mnt;

	FILE *fstab_writer = fopen (file, "w");
	fprintf (fstab_writer, "\n"); /* make empty */
	fclose (fstab_writer);

	printf("Test mount writefstab\n");

	succeed_if (kdbMount (kdb, mnt=keyNew("user/tests/filesystems",KEY_VALUE, "fstab", KEY_END),
		conf=ksNew (2,keyNew("system/path", KEY_VALUE, file, KEY_END), KS_END)) == 0,
		"could not mount fstab");
	keyDel(mnt);
	ksDel (conf);

	ks = ksNew( 22 ,
			keyNew ("user/tests/filesystems"
				, KEY_VALUE, "filesystems"
				, KEY_COMMENT, ""
			, KEY_END),
			keyNew ("user/tests/filesystems/rootfs"
				, KEY_VALUE, "non-rootfs"
				, KEY_COMMENT, "pseudo name"
			, KEY_END),
			keyNew ("user/tests/filesystems/rootfs/device"
				, KEY_VALUE, "/dev/sda6"
				, KEY_COMMENT, "Device or Label"
			, KEY_END),
			keyNew ("user/tests/filesystems/rootfs/dumpfreq"
				, KEY_VALUE, "0"
				, KEY_COMMENT, "Dump frequency in days"
			, KEY_END),
			keyNew ("user/tests/filesystems/rootfs/mpoint"
				, KEY_VALUE, "/"
				, KEY_COMMENT, "Moint point"
			, KEY_END),
			keyNew ("user/tests/filesystems/rootfs/options"
				, KEY_VALUE, "defaults,errors=remount-ro"
				, KEY_COMMENT, "Fileuser/tests specific options. See mount(8)"
			, KEY_END),
			keyNew ("user/tests/filesystems/rootfs/passno"
				, KEY_VALUE, "1"
				, KEY_COMMENT, "Pass number on parallel fsck"
			, KEY_END),
			keyNew ("user/tests/filesystems/rootfs/type"
				, KEY_VALUE, "jfs"
				, KEY_COMMENT, "Fileuser/tests type. See fs(5)"
			, KEY_END),
			keyNew ("user/tests/filesystems/swap00"
				, KEY_VALUE, "non-swapfs"
				, KEY_COMMENT, "pseudo name"
			, KEY_END),
			keyNew ("user/tests/filesystems/swap00/device"
				, KEY_VALUE, "/dev/sda10"
				, KEY_COMMENT, "Device or Label"
			, KEY_END),
			keyNew ("user/tests/filesystems/swap00/dumpfreq"
				, KEY_VALUE, "0"
				, KEY_COMMENT, "Dump frequency in days"
			, KEY_END),
			keyNew ("user/tests/filesystems/swap00/mpoint"
				, KEY_VALUE, "none"
				, KEY_COMMENT, "Moint point"
			, KEY_END),
			keyNew ("user/tests/filesystems/swap00/options"
				, KEY_VALUE, "sw"
				, KEY_COMMENT, "Fileuser/tests specific options. See mount(8)"
			, KEY_END),
			keyNew ("user/tests/filesystems/swap00/passno"
				, KEY_VALUE, "0"
				, KEY_COMMENT, "Pass number on parallel fsck"
			, KEY_END),
			keyNew ("user/tests/filesystems/swap00/type"
				, KEY_VALUE, "swap"
				, KEY_COMMENT, "Fileuser/tests type. See fs(5)"
			, KEY_END),
			KS_END);
	succeed_if (kdbSet (kdb, ks, keyNew ("user/tests/filesystems",0) ,KDB_O_DEL) == 15, "could not set keys");
	/*printf ("%d\n", kdbSet (kdb, ks, keyNew ("user/tests/filesystems",0) ,KDB_O_DEL));*/
	ksDel (ks);
	kdbClose (kdb);
}



int main(int argc, char** argv)
{
	printf("MOUNT       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_writehosts(".kdb/hosts_mount");
	test_readhosts(".kdb/hosts_mount");
	// test_failhosts(".kdb/hosts_mount");

	// test_writefstab(".kdb/fstab_mount");
	// test_readfstab(".kdb/fstab_mount");

	// test_passwd();

	printf("\ntest_mount RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

