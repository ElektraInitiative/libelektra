- infos = Information about ni plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/ini
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = maintained unittest libc nodep
- infos/metadata =
- infos/description = Reads and writes the nickel ini format

## Introduction

This plugin uses the nickel library in order to read/write
[metakeys](/doc/help/elektra-metadata.md) in the nickel ini format. Its purpose is to be
used in the `spec`-namespace or when any metadata should be
stored.

For configuration itself you should prefer the [toml plugin](/src/plugins/toml).

## Usage

To mount the ni plugin you can simply use:

```bash
kdb mount file.ini spec:/ni ni
```

The strength of this plugin is that it supports arbitrary meta
data and the file format is still human-readable.
For example the following lines:

```ini
[key]
meta=foo
```

specify that `key` has a metadata key `meta` containing the metavalue `foo`:

```bash
kdb meta get user:/ni/key meta
#> foo
```

For the metadata of the parent key use the following syntax:

```ini
[]
meta=foo
```

Line continuation works by ending the line with `\\` (a single backslash).
If you want a line break at the end of the line, use `\\n\\`.

To export a `KeySet` in the nickel format use:

```bash
kdb export spec:/ni ni > example.ni
```

For in-detail explanation of the syntax
(nested keys are not supported by the plugin)
[see /src/plugins/ni/nickel-1.1.0/include/bohr/ni.h](/src/plugins/ni/nickel-1.1.0/include/bohr/ni.h)

## Examples

```sh
# Mount the `ni` plugin at `spec:/tests/ni`
sudo kdb mount file.ini spec:/tests/ni ni

# Add some metadata
kdb meta set spec:/tests/ni/key metakey metavalue
kdb meta get spec:/tests/ni/key metakey
#> metavalue

kdb meta set spec:/tests/ni/key check/type char
kdb meta get spec:/tests/ni/key check/type
#> char

# Retrieve metadata
kdb meta ls spec:/tests/ni/key
#> check/type
#> metakey
kdb meta get spec:/tests/ni/key metakey
#> metavalue

# Add and retrieve key values
kdb get spec:/tests/ni/key
#>
kdb set spec:/tests/ni/key value
kdb set spec:/tests/ni/key/to nothing
kdb get spec:/tests/ni/key
#> value
kdb get spec:/tests/ni/key/to
#> nothing

# Undo modifications
kdb rm -r spec:/tests/ni
sudo kdb umount spec:/tests/ni
```

## Limitations

- Supports most KeySets, but `kdb test` currently reports some errors
  (likely because of the UTF-8 handling happening within ni).
- Keys have a random order when written out.
- Comments are not preserved, they are simply removed.
- Parse errors simply result in ignoring (and removing) these parts.

## Nickel

This plugin is based on the Nickel Library written by
author: charles@chaoslizard.org

Nickel (Ni) has its strength in building up a hierarchical
recursive Node structure which is perfect for parsing and
generating ini files. With them arbitrary deep nested hierarchy
are possible, but limited in a keyname of a fixed size.

The API of nickel is very suited for elektra, it can use
`FILE*` pointers (using that elektra could open and lock
files), the node-hierarchy can be transformed to
keysets, but it lacks many features like comments
and types.

The format is more general than the kde-ini format, it can
handle their configuration well, when the section names
do not exceed the specified length. Nesting is only required
in the first depth, any deeper is not understood by kde config
parser.

The memory footprint is for a 190.000 (reduced to 35.000 when
rewrote first ) line ini file with 1.1MB size is 16.88 MB.
The sort order is not stable, even not with the same file
rewritten again.
