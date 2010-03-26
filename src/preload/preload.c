/***************************************************************************
               preload.c - Preload your system with configuration
                             -------------------
    begin                : Mon 12 Nov 2007
    copyright            : (C) 2007 by Markus Raab
    email                : registry@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include <kdb.h>
#include <config.h>
#include <string.h>
#include <stdio.h>

int preload_user (KDB *handle);
int preload_system (KDB *handle);
int preload_tests (KDB *handle);
int preload_mountpoint(KDB *handle, char* name, char *mountpoint, char *backend, char *path);

int preload_user (KDB *handle)
{
	KeySet *ks = ksNew (100,
		keyNew(	"user",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "User configuration",
			KEY_END),
		keyNew(	"user/env",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "User environment",
			KEY_END),
		keyNew(	"user/env/PATH",
			KEY_VALUE, "/usr/local/bin:/usr/bin:/bin",
			KEY_COMMENT, "User environment",
			KEY_END),
		keyNew(	"user/bindkey",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "User bindkey configuration",
			KEY_END),
		keyNew(	"user/bindkey/menu",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Menu shortcuts",
			KEY_END),
		keyNew(	"user/bindkey/menu/open",
			KEY_VALUE, "CTRL_O",
			KEY_COMMENT, "Open a new file",
			KEY_END),
		keyNew(	"user/bindkey/menu/quit",
			KEY_VALUE, "CTRL_Q",
			KEY_COMMENT, "Open a new file",
			KEY_END),
		keyNew(	"user/bindkey/menu/find",
			KEY_VALUE, "CTRL_F",
			KEY_COMMENT, "Open a new file",
			KEY_END),
		keyNew(	"user/bindkey/clipboard",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Clipboard configuration",
			KEY_END),
		keyNew(	"user/bindkey/clipboard/paste",
			KEY_VALUE, "CTRL_V",
			KEY_COMMENT, "Paste clipboard content",
			KEY_END),
		keyNew(	"user/bindkey/clipboard/copy",
			KEY_VALUE, "CTRL_C",
			KEY_COMMENT, "Copy clipboard content",
			KEY_END),
		keyNew(	"user/bindkey/clipboard/cut",
			KEY_VALUE, "CTRL_C",
			KEY_COMMENT, "Cut clipboard content",
			KEY_END),
		keyNew(	"user/sw",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "User software configuration",
			KEY_END),
		keyNew(	"user/sw/lib",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Library configuration",
			KEY_END),
		keyNew(	"user/sw/sound",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Sound configuration",
			KEY_END),
		keyNew(	"user/sw/gnome",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Gnome configuration",
			KEY_END),
		keyNew(	"user/sw/kde",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Kde configuration",
			KEY_END),
		keyNew(	"user/sw/science",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Science configuration",
			KEY_END),
		keyNew(	"user/sw/shells",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Shells configuration",
			KEY_END),
		keyNew(	"user/sw/shells/zsh",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Zsh configuration",
			KEY_END),
		keyNew(	"user/sw/shells/zsh/env",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Environment specific"
			"for zsh which will be additional to user/env for zsh",
			KEY_END),
		keyNew(	"user/sw/shells/bash",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Bash configuration",
			KEY_END),
		KS_END);
	printf ("Set user configuration... ");
	if (kdbSet (handle, ks, keyNew("user",0), KDB_O_DEL) == -1)
	{
		printf ("failure\n");
		return 1;
	} else {
		printf ("done\n");
		return 0;
	}
}


int preload_system (KDB *handle)
{
	KeySet *ks = ksNew (100,
		keyNew(	"system",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "System configuration",
			KEY_END),
		keyNew(	"system/hosts",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Hosts configuration for this system.\n"
			"Setup hosts, ip adresses and aliases.",
			KEY_END),
		keyNew(	"system/filesystems",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Filesystem configuration for this system.\n"
			"Setup mountpoints, types and options.",
			KEY_END),
		keyNew(	"system/users",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "User configuration for this system.\n"
			"Add users and set passwords.",
			KEY_END),
		KS_END);
	printf ("Set system configuration... ");
	if (kdbSet (handle, ks, keyNew("system",0), KDB_O_DEL) == -1)
	{
		printf ("failure\n");
		return 1;
	} else {
		printf ("done\n");
		return 0;
	}
}


int preload_tests (KDB *handle)
{
	KeySet *ks = ksNew (100,
		keyNew(	"user/tests",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Place for tests.\n"
			"Used by elektra testframework.",
			KEY_END),
		keyNew(	"user/benchmarks",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Place for benchmarks.\n"
			"Used by elektra benchmarkframework.",
			KEY_END),
		KS_END);
	printf ("Set test directories... ");
	if (kdbSet (handle, ks, keyNew("user",0), KDB_O_DEL) == -1)
	{
		printf ("failure\n");
		return 1;
	} else {
		printf ("done\n");
		return 0;
	}
}


int preload_configuration(KDB *handle)
{
	KeySet *ks = ksNew (100,
		keyNew(	"system/elektra",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Configuration for libelektra.",
			KEY_END),
		keyNew(	"system/elektra/xml",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Configuration for libelektratools.",
			KEY_END),
		/*
		keyNew(	"system/elektra/xml/schemapath",
			KEY_VALUE, KDB_SCHEMA_PATH,
			KEY_COMMENT, "Schemapath to elektra xml-schema.\n"
			"The schema is needed to validate xml files for in- and export.",
			KEY_END),
		*/
		keyNew(	"system/elektra/mountpoints",
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "Mountpoints for elektra.\n"
			"Place a directory here to make a mountpoint.\n"
			"A mountpoint decides to use a different backend for that path and below.\n"
			"See system/mountpoints/template how to create it.",
			KEY_END),
		KS_END);
	printf ("Set mountpoints... ");
	if (kdbSet (handle, ks, keyNew("system",0), KDB_O_DEL) == -1)
	{
		printf ("failure\n");
		return 1;
	} else {
		printf ("done\n");
		return 0;
	}
}

char * location (char *buffer, char *name, char *point)
{
	buffer[0] = 0;
	strcat (buffer, "system/elektra/mountpoints/");
	strcat (buffer, name);
	strcat (buffer, point);
	return buffer;
}

int preload_mountpoint(KDB *handle, char* name, char *mountpoint, char *backend, char *path)
{
	char buffer [MAX_PATH_LENGTH];

	KeySet *ks = ksNew (100,
		keyNew(	mountpoint,
			KEY_DIR,
			KEY_VALUE, backend,
			KEY_COMMENT, "This is a mounted backend.",
			KEY_END),
		keyNew(	location (buffer, name, ""),
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "This is a mounted backend, see subkeys for more information",
			KEY_END),
		keyNew(	location (buffer, name, "/mountpoint"),
			KEY_VALUE, mountpoint,
			KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
			"It must be a valid, canonical elektra path. There are no ., .. or multiple slashes allowed.\n"
			"You are not allowed to mount inside system/elektra.",
			KEY_END),
		keyNew(	location (buffer, name, "/backend"),
			KEY_VALUE, backend,
			KEY_COMMENT, "The name of the backend library.\n"
			"This name describes which .so should be loaded for that backend.\n"
			"You are allowed to mount the same backend multiple times.",
			KEY_END),
		keyNew(	location (buffer, name, "/config"),
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "The configuration for the specific backend.\n"
			"All keys below that directory will be passed to backend.\n"
			"These keys have backend specific meaning.\n"
			"See documentation http://www.libelektra.org for which keys must or can be set.\n"
			"Here the most important keys should be preloaded.",
			KEY_END),
		keyNew(	location (buffer, name, "/config/path"),
			KEY_VALUE, path,
			KEY_COMMENT, "The path where the config file is located."
			"This item is often used by backends using configuration in a filesystem"
			"to know there relative location of the keys to fetch or write.",
			KEY_END),
		KS_END);
	printf ("Set %s mountpoint %s in path %s... ", name, mountpoint, path);
	if (kdbSet (handle, ks, keyNew("system",0), KDB_O_DEL) == -1)
	{
		printf ("failure\n");
		return 1;
	} else {
		printf ("done\n");
		return 0;
	}
}


/**
 * See http://www.libelektra.org/Preloading for more information.
 *
 * Preloading is a technique to make the first start without
 * build-in values possible.
 *
 * This program will preload the configuration for elektra itself.
 * It should be executed by the administrator for system and
 * configuration.
 *
 * But every user should also execute it to have a minimum skeleton
 * to make it more easy for the programs and to have full pathes
 * when having mountpoints.
 *
 */
int main(int argc, char ** argv)
{
	KDB *handle = kdbOpen();
	int rc=0;

	if (argc == 1)
	{
		printf ("Use %s <mode>\n", argv[0]);
		printf ("\n");
		printf ("Modes are:\n");
		printf ("system ... creates the system hierarchy\n");
		printf ("user   ... creates the user hierarchy\n");
		printf ("tests  ... creates the skeleton for tests and benchmarks\n");
		printf ("conf   ... creates the skeleton for configuration for elektra\n");
		printf ("mount <name> [<root>] [<backend>] [<path>]\n");
		printf ("\n");
		printf ("Mount options are:\n");
		printf ("name   ... test for some predefined mountpoints\n");
		printf ("           root for the root backend\n");
		printf ("           otherwise any unique name for personal use\n");
		printf ("other arguments are empty for test and root\n");
		printf ("root   ... Where to mount the backend\n");
		printf ("backend... Name of the backend\n");
		printf ("path   ... Where the files should be located on the disk\n");
		printf ("\n");
		printf ("examples:\n");
		printf (" preload system\n");
		printf (" preload mount root ... mounts root backend\n");
		printf (" preload mount test ... mounts some backends\n");
		printf (" preload mount fstab system/filesystems/ fstab /tmp/fstab\n");
		printf (" preload mount passwd system/users/ passwd /tmp/passwd\n");
		return 0;
	} else if (argc > 1) {
		if (!strcmp (argv[1], "system")) rc=preload_system(handle);
		else if (!strcmp (argv[1], "user")) rc=preload_user(handle);
		else if (!strcmp (argv[1], "tests")) rc=preload_tests(handle);
		else if (!strcmp (argv[1], "conf")) rc=preload_configuration(handle);
		else if (!strcmp (argv[1], "mount"))
		{
			if (argc == 3)
			{
				if (!strcmp (argv[2], "root"))
				{
					rc=preload_mountpoint(handle, "root", "", "filesys", "/etc/kdb");
				} else if (!strcmp (argv[2], "test")) {
					preload_mountpoint(handle, "hosts1", "system/hosts/", "hosts", "/etc/hosts");
					preload_mountpoint(handle, "hosts2", "user/hosts/", "hosts", "/tmp/hosts");
					preload_mountpoint(handle, "passwd1", "system/users/", "passwd", "/tmp/passwd");
					preload_mountpoint(handle, "passwd2", "user/users/", "passwd", "/etc/passwd");
					preload_mountpoint(handle, "fstab1", "system/filesystems/", "fstab", "/etc/fstab");
					rc=preload_mountpoint(handle, "fstab2", "user/filesystems/", "fstab", "/tmp/fstab");
					/*
					preload_mountpoint(handle, "template1", "user/template/", "template", "");
					preload_mountpoint(handle, "template2", "system/template/", "template", "");
					*/
				}
				else printf ("unkown mount options: give more arguments or use root or test\n");
			}
			else if (argc == 6)
			{
				rc=preload_mountpoint(handle, argv[2], argv[3], argv[4], argv[5]);
			}
			else printf ("uncorrect number of arguments\n");
		} else printf ("unknown mode: use system, user, tests, conf or mount\n");
	}
	kdbClose (handle);
	return rc;
}
