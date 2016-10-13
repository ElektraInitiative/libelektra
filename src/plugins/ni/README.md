- infos = Information about ni plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage ini
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = maintained libc nodep
- infos/description = Reads and writes the nickel ini format

## Introduction ##

This plugin uses the nickel library in order to read/write
 [metakeys](/doc/help/elektra-meta-data.md) in the nickel ini format. It's purpose is to be
used in the `spec`-namespace or when any metadata should be
stored.

For ini files for applications, e.g. smb.conf you should prefer the
[ini plugin](/src/plugins/ini).

## Usage


To mount a ni plugin you can simply use:

    kdb mount file.ini spec/ni ni

The strength and usage of this plugin is that it supports arbitrary meta
data and is still human readable.
E.g.

    [key]
    meta=foo

creates the key with metadata key `meta` and metadata value `foo`:

    $ kdb getmeta user/ni/key meta
    foo

the metadata for the parent key has following syntax:

    []
    meta=foo

Line continuation works by ending the line with `\\`.

Exporting a KeySet to the nickel format:

    kdb export spec/ni ni > example.ni


For in-detail explanation of the syntax
(nested keys are not supported by the plugin, however)
[see /src/plugins/ni/nickel-1.1.0/include/bohr/ni.h](/src/plugins/ni/nickel-1.1.0/include/bohr/ni.h)


## Limitations ##

- Supports most KeySets, but `kdb test` currently reports some errors
  (likely because of the UTF-8 handling happening within ni).
- Keys have a random order when written out.
- No comments are preserved, they are simply removed.
- Parse errors simply result to ignoring (and removing) these parts.


## Nickel ##

This plugin is based on the Nickel Library written by
author: charles@chaoslizard.org

Nickel (Ni) has its strength in building up a hierarchical
recursive Node structure which is perfect for parsing and
generating ini files. With them arbitrary deep nested hierarchy
are possible, but limited in a keyname of a fixed size.

The API of nickel is very suited for elektra, it can use
`FILE*` pointers (using that elektra could open and lock
files), the node-hierarchy can be transformed to
keysets, but it lacks of many features like comments
and types.

The format is more general then the kde-ini format, it can
handle their configuration well, when the section names
do not exceed the specified length. Nesting is only required
in the first depth, any deeper is not understood by kde config
parser.

The memory footprint is for a 190.000 (reduced to 35.000 when
rewrote first ) line ini file with 1.1MB size is 16.88 MB.
The sort order is not stable, even not with the same file
rewritten again.

[bohr libraries](https://github.com/chazomaticus/bohr)
