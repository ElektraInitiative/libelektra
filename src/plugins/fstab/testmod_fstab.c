/*************************************************************************** 
 *           testmod_fstab.c  - Test suite for fstab
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

#if 0

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

#endif

int main(int argc, char** argv)
{
	printf("MOUNT       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	/*
	test_writefstab(".kdb/fstab_mount");
	test_readfstab(".kdb/fstab_mount");
	*/

	printf("\ntest_mount RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

