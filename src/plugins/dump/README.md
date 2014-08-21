- infos = Information about dump plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs = 
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = Dumps into a format tailored for complete KeySet semantics

## Introdcution ##

This plugin is a storage plugin that supports full Elektra semantics. Combined with a resolver plugin it already assembles a fully featured backend. No other plugins are needed. 

## Format ##

The ﬁle format consists of a simple command language with arguments. When an argument is binary or string data the length needs to passed ﬁrst. Because the size is known in advance, any binary dump is accepted. Terminating characters present no problem. The commands are assembled similar to the ones present in Elektra’s API.

The ﬁle starts with the magic word `kdbOpen` followed by a version number. Processing can be stopped immediately when it is not in Elektra’s dump format at all. A wrong version number most likely indicates that the version of the plugin is too old to recognise all commands in the ﬁle. The basic idea of the dump plugin is to write out the way that the KeySet needs to be constructed. The dump plugin interprets such a ﬁle. The ﬁle also looks similar to Ccode that would create the KeySet. Keys can contain any binary values and arbitrary metadata and are still stored and parsed correctly. The dump plugin can even reconstruct pointers to metadata to save memory. When a pointer to the same region of memory is found, a special command keyCopyMeta is written out that is able to reconstruct the data structure the way it was before. The commands were designed to make parsing of the ﬁle an easy task.

### Format Examples ###
The serialised conﬁguration can look like:

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


## Examples ##

Export a KeySet using `dump`:
	kdb export system/example dump > example.ecf

Import a KeySet using `dump`:
	cat example.ecf | kdb import system/example dump
