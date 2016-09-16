- infos = Information about dump plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = productive maintained conformant unittest tested nodep -1000
- infos/metadata =
- infos/description = Dumps into a format tailored for complete KeySet semantics

## Introduction ##

This plugin is a storage plugin that supports full Elektra
semantics. Combined with a resolver plugin it already assembles a fully
featured backend. No other plugins are needed.

## Format ##

The file format consists of a simple command language with
arguments. When an argument is binary or string data the length needs to be
passed first. Because the size is known in advance, any binary dump is
accepted. Terminating characters present no problem. The commands are
assembled similar to the ones present in Elektra’s API.

The file starts with the magic word `kdbOpen` followed by a version
number. Processing can be stopped immediately when it is not in
Elektra’s dump format at all. A wrong version number most likely
indicates that the version of the plugin is too old to recognise all
commands in the file. The basic idea of the dump plugin is to write
out the way that the KeySet needs to be constructed. The dump plugin
interprets such a file. The file also looks similar to C code that
would create the KeySet. Keys can contain any binary values and arbitrary
metadata and are still stored and parsed correctly. The dump plugin can
even reconstruct pointers to metadata to save memory. When a pointer
to the same region of memory is found, a special command `keyCopyMeta`
is written out that is able to reconstruct the data structure the way
it was before. The commands were designed to make parsing of the file
an easy task.

### Format Examples ###

The serialised configuration can look like (0 bytes at end of strings are
omitted):

	kdbOpen 1 		
	ksNew 207 		
	keyNew 27 1 		
	system/elektra/mountpoints		 
	keyMeta 8 27		
	commentBelow are the mountpoints.		 
	keyEnd 		
	keyNew 32 19		 
	system/elektra/mountpoints/dbusserialised Backend 		
	keyEnd keyNew 39 1 		
	system/elektra/mountpoints/dbus/config 		
	keyMeta 8 72 		
	commentThis is a configuration for a backend, see subkeys for more information 		
	keyEnd 		
	keyNew 53 1 		
	system/elektra/mountpoints/fstab/config/struct/FStab 		
	keyCopyMeta 59 11 		
	system/elektra/mountpoints/file 		
	systems/config/struct/FStabcheck/type 		
	keyEnd		
	ksEnd		


## Limitations ##

(status -1000)

- It is quite slow
- Files cannot easily edited by hand

## Examples ##

Export a KeySet using `dump`:

	kdb export system/example dump > example.ecf

Import a KeySet using `dump`:

	cat example.ecf | kdb import system/example dump

Using grep/diff or other unix tools on the dump file. Make sure that you
treat it as text file, e.g.:

	grep --text mountpoints example.ecf

