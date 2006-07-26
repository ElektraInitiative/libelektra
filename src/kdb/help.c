/***************************************************************************
                          help.c  -  Functions used by 'kdb help' command
                             -------------------
    begin                : Sat Jul 22 2006
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id: kdb.c 802 2006-06-16 09:34:46Z mraab $

*/




#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>

#define CMD_GET       1
#define CMD_SET       2
#define CMD_REMOVE    3
#define CMD_LIST      4
#define CMD_LINK      5
#define CMD_EDIT      6
#define CMD_LOAD      7
#define CMD_SAVE      8
#define CMD_MONITOR   9
#define CMD_MOVE      10
#define CMD_INFO      25
#define CMD_HELP      30



/**
 * @defgroup libexample  The kdb Command Source Code: Example of Full Library Utilization
 * @{
 */



/**
 * Prints out help to the kdb (1) command.
 *TODO: should be same as man page
 * help2man?
 * --help does not work
 *
 * @par Example:
 * @code
 * bash$ kdb help
 * bash$ kdb -h
 * @endcode
 */
int commandHelp() {
	printf("Usage: kdb [OPTION] <command> [<key> [value ...]]\n");
	printf("Use kdb to manipulate the Key Database.\n");
	printf("\n");

	printf("ARGUMENTS\n");
	printf("Commands are explained with kdb -h command\n");
	printf("<key> is the name of the key. It can be prefixed\n");
	printf(" with environment KDB_ROOT. The slash between will\n");
	printf("be inserted.\n");
	printf(" export KDB_ROOT=\"user/test/dir\"\n");
	printf(" kdb get file/key ... will expand to user/test/dir/file/key\n");
	printf("[value ...] hold the value which should be set\n");
	printf("\n");
	
	printf("COMMANDS\n");
	printf(" kdb get [-dlfs] key/name\n");
	printf(" kdb set [-t type] [-c \"A comment about this key\"] [-m mode] [-u uid]\n");
	printf("         [-g gid] key/name \"the value\"\n");
	printf(" kdb set [-t type] [-m mode] [-c \"A comment\"] key/name -- \"the value\"\n");
	printf(" kdb set [-t type] [-b file] key/name\n");
	printf(" kdb ls [-lRfvs] [key/dir | key/name]\n");
	printf(" kdb ls [-lRfvx] [key/dir | key/name] > keys.xml\n");
	printf(" kdb edit [-R] [key/dir | key/name]\n");
	printf(" kdb rm key/name\n");
	printf(" kdb mv key/src key/dest\n");
	printf(" kdb ln key/src key/dest\n");
	printf(" kdb export system/some/tree.root > file.xml\n");
	printf(" kdb import < file.xml\n");
	printf(" kdb import file.xml\n");
	printf(" kdb monitor some/key/name\n");
	printf(" kdb info\n");
	printf("\n");
	
	return 0;
}

void optionr() {
	printf("-R -r\n");
	printf(" Causes to work recursively. In ls, will list recursively. \n");
	printf("\n");
}

void optionx() {
	printf("-x\n");
	printf(" Makes ls output an XML representation of the keys, instead of an ls-compatible output. \n");
	printf("\n");
}

void optionl() {
	printf("-l\n");
	printf(" Causes to display long results. With ls, will generate lists similar to ls -l. With get, will show also the key name. \n");
	printf("\n");
}

void optiona() {
	printf("-a\n");
	printf(" Causes ls to display also inactive keys. Generate lists similar to ls -a. Inactive keys are keys which basename begins with a '.' (dot). An example of inactive key: system/sw/XFree/current/Monitor/.Monitor1 \n");
	printf("\n");
}

void optionf() {
	printf("-f\n");
	printf(" Causes to work with full key names. A full key name makes sense only on user/* keys, and differentiate from the regular key names in specifying the owner user. If the current user is someuser, the user/some/key full name is user:someuser/some/key. Makes effect in ls, export and get subcommands. \n");
	printf("\n");
}

void optiond() {
	printf("-d\n");
	printf(" Causes get to work descriptivelly. When requesting a key it will show the comment, key name and its value in a fancy format \n");
	printf("\n");
}

void options() {
	printf("-s\n");
	printf(" Causes get and ls to be more friendly to Shell scripts. For example, when requesting user/env/env2/PATH, the output will be PATH=\"the value\", that is, only the basename of the key will be showed and the value will be surrounded by  \".\n");
	printf("\n");
}

void optiont() {
	printf("-t type\n");
	printf(" When setting a key's value, you can specify the type with this switch. Currently accepted types are string for plain text, bin for binary as-is values, dir to create folder keys and link to create symbolic links between keys. Plain text are always stored as UTF-8(7) in Elektra, regardeless of your current encoding ($LANG). If you want to force a value to be stored without the UTF-8(7) encoding (a bad idea), you can set it as binary. Binary values should be avoided, because they are black boxes for system administrators. \n");
	printf("\n");
}

void optionb() {
	printf("-b filename\n");
	printf(" Set the key value as the content of file filename. This option is more usefull when setting binary keys. \n");
	printf("\n");
}

void optionm() {
	printf("-m mode\n");
	printf(" For the set command. Will set the key access permission to mode, which must be an octal number as for chmod(1). \n");
	printf("\n");
}

void optionu() {
	printf("-u uid\n");
	printf(" Create the key with uid user ID. It can be a user name or a uid number. \n");
	printf("\n");
}

void optiong() {
	printf("-g gid\n");
	printf(" Create the key with gid group ID. It can be a group name or a gid number \n");
	printf("\n");
}

void optionc() {
	printf("-c comment\n");
	printf(" When setting keys, you can use this argument to set a descriptive comment for it. This comment is exactly as a comment in a plain text configuration file. The comment is stored as UTF-8(7) regardeless of your current encoding ($LANG). \n");
	printf("\n");
}

void optionv() {
	printf("-v\n");
	printf(" With the ls subcommand, will make it show also the value stored in the key. \n");
	printf("\n");
}

void commandGetHelp () {
	printf("get\n");
	printf(" Get the value from the specified key. Accepts options: -d, -l, -f, -s \n");
	printf("\n");
	optiond();
	optionl();
	optionf();
	options();
}

void commandSetHelp () {
	printf("set\n");
	printf(" Set the value to the specified key. Accepts options: -c, -t, -m, -b \n");
	printf("\n");
	optionc();	
	optiont();	
	optionm();	
	optionb();	
}

void commandListHelp () {
	printf("ls\n");
	printf(" As the ls(1) command, list key names for the specified key, or children keys, if specified a folder key. The -v argument will make it show also the values of each key. The -d (descriptive) will make it show the comment, key name and its value, as you are watching a plain text file. Accepts options: -x, -d, -l, -f, -v, -R, -s \n");
	printf("\n");
	optionx();
	optiond();
	optionl();
	optionf();
	optionv();
	optionr();
}

void commandLinkHelp () {
	printf("ln\n");
	printf("Creates a key that is a symbolic links to another key. \n");
	printf("\n");
}

void commandMoveHelp() {
	printf("mv\n");
	printf("Move, or renames a key. Currently it can't move keys across different filesystems.\n");
	printf("\n");
}

void commandRemoveHelp() {
	printf("rm\n");
	printf("As the rm(1) command, removes the key specified. \n");
	printf("\n");
}

void commandEditHelp() {
	printf("edit\n");
	printf("A very powerfull subcommand that lets you edit an XML representation of the keys. The parameters it accepts is usually a parent key, so its child keys will be gathered. Can be used with the -R flag to work recursively. The editor used is the one set in the $EDITOR environment variable, or vi. After editing the keys, kdb edit will analyze them and commit only the changed keys, remove the keys removed, and add the keys added. \n");
	printf("\n");
}

void commandExportHelp() {
	printf("export, save \n");
	printf("Export a subtree of keys to XML. If no subtree is defined right after the export command, system and current user trees will be exported. Output is written to standard output. The output encoding will allways be UTF-8, regardeless of your system encoding. UTF-8 is the most universal charset you can get when exchanging data between multiple systems. Accepts -f. \n");
	printf("\n");
	optionf();	
}

void commandImportHelp() {
	printf("import, load \n");
	printf("Import an XML representation of keys and save it to the keys database. If no filename is passed right after the import command, standard input is used. \n");
	printf("\n");
}

void commandInfoHelp() {
	printf("info\n");
	printf("Displays some information about the Elektra library being used, version, backends, etc.\n");
	printf("\n");
}

void commandMonitorHelp() {
	printf("monitor, mon, \n");
	printf("Monitor a key for some value change. It will block your command line until a change in the key value is detected, then return its new value.\n");
	printf("\n");
}


int helpCommand(int command) {
	switch (command) {
		case CMD_SET:             commandSetHelp();     break;
		case CMD_LIST:            commandListHelp();    break;
		case CMD_LINK:            commandLinkHelp();    break;
		case CMD_GET:             commandGetHelp();     break;
		case CMD_REMOVE:          commandRemoveHelp();  break;
		case CMD_EDIT:            commandEditHelp();    break;
		case CMD_LOAD:            commandImportHelp();  break;
		case CMD_SAVE:            commandExportHelp();  break;
		case CMD_MONITOR:         commandMonitorHelp(); break;
		case CMD_MOVE:            commandMoveHelp();    break;
		case CMD_INFO:            commandInfoHelp();    break;
	}
	exit (0);
}

/**
 * @}
 */
