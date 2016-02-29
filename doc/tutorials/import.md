# How-To: kdb import #

## Introduction ##

The kdb tool allows users to interact with Elektra's Key Database via the command line.
This tutorial explains the import function of kdb. This command lets you import Keys from
the Elektra Key Database.

The command to use kdb import is:

    kdb import [options] destination [format]

In this command, destination is where the imported Keys should be stored below. For
instance, `kdb import system/imported` would store all the keys below
system/imported. This command takes Keys from stdin to store them into the Elektra
Key Database. Typically, it is used with a pipe to read in the Keys from a file.

### Format ###

The format argument can be a very powerful option to use with kdb import.
The format argument allows a user to specify which plug-in is used to import the
Keys into the Key Database. The user can specify any storage plug-in to serve as the
format for the Keys to be imported. For instance, if a user wanted to import a /etc/hosts
file into KDB without mounting it, they could use the command `cat /etc/hosts | kdb import system/hosts hosts`.
This command would essentially copy the current hosts file into KDB, like mounting it. Unlike mounting it,
changes to the Keys would not be reflected in the hosts file and vise versa.

#### Dump Format ####

If no format is specified, the format `dump` will be used instead. The dump format is the standard way
of expressing Keys and all their relevant information. This format is intended to be used only within Elektra.
The dump format is a good means of backing up Keys from the Key Database for use with Elektra later
such as reimporting them later. As of this writing, dump is the only way to fully preserve all parts of the
KeySet.

It is very important to note that the dump does not rename keys by design. If a user exports a KeySet using dump
using a command such as `kdb export system/backup > backup.ecf`, they can only import that keyset back into
system/backup using a command like `cat backup.ecf | kdb import system/backup`.

## Options ##

The kdb import command only takes one special option:

	-s --strategy <name>		which is used to specify a strategy

### Strategies ###

For kdb import, you can specify a strategy to use if Keys already exist in the specified destination

The current list of strategies are:

	preserve					any keys already in the destination will not be overwritten

	overwrite					any keys already in the destination will be overwritten if a new key has the same name

	cut							all keys already in the destination will be removed, then new keys will be imported

If no strategy is specified, the command defaults to the preserve strategy as to not be destructive to any previous keys.

## Example ##

	cat backup.ecf | kdb import system/backup

This command would import all keys stored in the file backup.ecf into the Key Database under system/backup.

In this example, backup.ecf was exported from the KeySet using the dump format by using the command:

	kdb export system/backup > backup.ecf

backup.ecf contains all the information about the keys below system/backup:

	$cat backup.ecf
	kdbOpen 1
	ksNew 3
	keyNew 19 0
	system/backup/key1
	keyMeta 7 1
	binary
	keyEnd
	keyNew 19 0
	system/backup/key2
	keyMeta 7 1
	binary
	keyEnd
	keyNew 19 0
	system/backup/key3
	keyMeta 7 1
	binary
	keyEnd
	ksEnd

Before the import command, system/backup does not exists and no keys are contained there.
After the import command, running the command `kdb ls system/backup` prints:

	system/backup/key1
	system/backup/key2
	system/backup/key3
