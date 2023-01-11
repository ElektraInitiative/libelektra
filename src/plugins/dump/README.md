- infos = Information about the dump plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>, Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/dump
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = productive maintained tested/unit nodep -1000
- infos/metadata =
- infos/description = Dumps into a format tailored for complete KeySet semantics

## Introduction

This plugin is a storage plugin that supports full Elektra
semantics. Combined with a resolver plugin it already assembles a fully
featured backend. No other plugins are needed.

## Format

The file starts with the magic word `kdbOpen` followed by a version number (currently `2`) and a newline.
The plugin can read files of version `1` and `2`, but it only writes version `2`.

After this first line, the format consists of a series of commands.
The supported commands are `$key`, `$meta` and `$copymeta`.
We use the `$` prefix to make the commands standout more.
However, `$` does not always mean command.
Keynames and values could also start with `$`, but since the plugin always knows whether a command or something else comes next, we do not need any kind of escaping.

The `$key` command creates a new key.
It takes 5 arguments.
The first 3 are on the same line separated by a space.
The other two are on separate lines:

```
$key <type> <nsize> <vsize>
<name>
<value>

```

`<type>` is either `string` or `binary` and indicates what kind of value the key contains.
`<nsize>` and `<vsize>` are the name size and value size respectively.
For the name and for string values, the size does not include the null-terminator present in C strings.
For binary keys all bytes (including any null-terminators) are counted.
`<name>` and `<value>` are the keyname and key value.
Because we know their length, they can contain arbitrary characters.
Even newlines are allowed.
The newlines between `<name>` and `<value>`, and after `<value>` are just to make the file more readable.
They must be present, but they do not determine where `<name>` or `<value>` end.

The `$meta` command adds a new metakey to the last key.
It is very similar to the `$key` command, but it only takes 4 arguments.
There is no type argument, because metakeys always have string values.

```
$meta <nsize> <vsize>
<name>
<value>

```

The arguments work just like they do for `$key`.

Finally, there is `$copymeta`.
It is needed, because a `keyCopyMeta` call results in two keys with the same metakey (equal pointers).
To achieve this, we indicate which metakey should be copied from which key.
The `$copymeta` command also takes 4 arguments:

```
$copymeta <knsize> <mnsize>
<keyname>
<metaname>

```

`<keyname>` is the name of the key from which the metadata is copied and `<knsize>` is its size (without the null-terminator).
Similarly, `<metaname>` is the name of the metakey that is copied and `<mnsize>` is its size (without the null-terminator).

There is also `$end`.
It is used to signal the end of the data to the plugin.
The `$end` command is completely optional.
Without it, the plugin will just read the file until the end.
However, in streaming use `$end` is needed, because there is no end of the "file".

### Format Examples

The following is an example `dump` file that was mounted at `system:/elektra/mountpoints`:

```
kdbOpen 2
$key binary 0 0


$meta 6 0
binary

$meta 7 27
comment
Below are the mount points.
$key string 4 18
dbus
serialized Backend
$key string 11 0
dbus/config

$meta 7 71
comment
This is a configuration for a backend,
see subkeys for more information
$key string 12 0
fstab/config

$copymeta 11 7
dbus/config
comment
$end
```

A few things you might have noticed:

- The first key has an empty name, because it is the root key of this mountpoint.
- The value size of `0` for the first key makes it a `NULL` key, but only because it is `binary`.
  The third key (`$key string 11 0`) also has value size 0, but is a `string` key.
  This means its value is an empty string `""`.
- The empty lines after `$key binary 0 -1` and `dbus/config` are because the respective names/values are empty.
- The comment above `$key string 25 0` shows that newlines in key values are completely fine, because we know what size the value has to be.

## Limitations

(status -1000)

- It is quite slow

## Examples

Export a KeySet using `dump`:

```sh
kdb export system:/example dump > example.ecf
```

Import a KeySet using `dump`:

```sh
cat example.ecf | kdb import system:/example dump
```

Using grep/diff or other Unix tools on the dump file. Make sure that you
treat it as text file, e.g.:

```sh
grep --text 'mount points' example.ecf
```
