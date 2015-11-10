- infos = Information about struct plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = apply
- infos/ordering = check
- infos/placements = presetstorage
- infos/description = Copies meta data to keys using structbing

## Introduction ##

This plugin is a check plugin which checks the structure and
interrelations of Keys in order the verify that they represent a valid
configuration.

## Purpose ##

The glob plugin together with the check plugins create a good combination
to check keys which are present in the KeySet. For some storage plugins
like fstab, however, missing keys are as fatal as not validating keys
– it is not possible to write a valid configuration file without them.

The problem can be described as a structure built with lists and
other structures that must match with the key names of a KeySet. If a
structure must have an element, contrary to glob, the plugin can yield
an error if it is missing. During the matching, the plugin also applies
metadata to the individual keys. Such plugins that check the structure and
interrelations of keys are called structure checker. They can require that
various subkeys have to or must not exist. This can happen recursively
to specify any structure.

The struct plugin implements such a behaviour. It allows enforcement of
a strong consistency within the keys of one backend.

## Usage ##

In order for the `struct` plugin to do its job, it needs a plugin
configuration to know which structure it should check for. This
configuration can be passed from a storage plugin’s `config/needs`
clause.

## Example ##

The `fstab` plugin uses the `struct` plugin to verify the correct
structure. Here is a snippet from it's contract:

	keyNew ("system/elektra/modules/fstab/config/needs/struct", 
	KEY_VALUE, "list FStab",
	KEY_END), 
	keyNew ("system/elektra/modules/fstab/config/needs/struct/FStab",
	KEY_META, "check/type", "null empty", 
	KEY_END), 
	keyNew ("system/elektra/modules/fstab/config/needs/struct/FStab/device", 
	KEY_META, "check/type", "string", 
	KEY_META, "check/path", "device", 
	KEY_END), 
	keyNew ("system/elektra/modules/fstab/config/needs/struct/FStab/mpoint", 
	KEY_META, "check/type", "string", 
	KEY_META, "check/path", "directory", 
	KEY_END), 
	keyNew ("system/elektra/modules/fstab/config/needs/struct/FStab/type", 
	KEY_META, "check/type", "FSType", 
	KEY_END), 
	keyNew ("system/elektra/modules/fstab/config/needs/struct/FStab/options", 
	KEY_META, "check/type", "string", 
	KEY_END), 
	keyNew ("system/elektra/modules/fstab/config/needs/struct/FStab/dumpfreq", 
	KEY_META, "check/type", "unsigned_short", 
	KEY_END), keyNew ("system/elektra/modules/fstab/config/needs/struct/FStab/passno", 
	KEY_META, "check/type", "unsigned_short",
	KEY_END),

The key value of `needs/struct` within the plugin configuration marks the
starting point. `list` describes the first structure to be generated. It
is a built-in structure of the struct plugin that supports all subkeys
in one level. It applies to every direct subkey the structure check
received by the template parameter. The template parameter is, in this
case, FStab. The rest of the configuration specifies how entries of
FStab must look.

The information applied to the keys is given through metadata. This
metadata is copied to each key during the structure check. If,however,a
key is missing,the structure check will terminate with a failure. Any
additional key will also lead to an error.

The metadata may be evaluated by subsequent checks. In the situation of
fstab a typechecker and a path checker are both useful.

This approach for defining the structure works recursively. Every
element can have a value with a new structure check.

