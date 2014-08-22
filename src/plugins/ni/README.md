- infos = Information about ni plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = Reads and writes the nickel ini format

## Introduction ##

This plugin uses the nickel library in order to read/write configuration data in the nickel ini format.

## Sepecial Values ##

Nickel (Ni) has its strength in building up a hierarchical
recursive Node structure which is perfect for parsing and
generating ini files. With them arbitrary deep nested hierarchy
are possible, but limited in a keyname of a fixed size.

The format is more general then the kde-ini format, it can
handle their configuration well, when the section names
do not exceed the specified length. Nesting is only required
in the first depth, any deeper is not understood by kde config
parser.


## Restrictions ##

The random order of the files and the cutting out of comments
is a large flaw in general but does not bother for kde config
because it removes comments too (?) and can restore the same
order easily.

The API of nickel is very suited for elektra, it can use
`FILE*` pointers (using that elektra could open and lock
files), the node-hierarchy can be transformed to
keysets, but it lacks of many features like comments
and types.

The memory footprint is for a 190.000 (reduced to 35.000 when
rewrote first ) line ini file with 1.1MB size is 16.88 MB.
The sort order is not stable, even not with the same file
rewritten again.

The error handling is
very spartanic.

## More Information ##

author: charles@chaoslizard.org
comments for sections/keys

http://chaoslizard.sourceforge.net/nickel
http://www.chaoslizard.org/devel/bohr/wiki/Docs/Ni

## Example ##

Exporting a KeySet to the nickle format:
	kdb export system/example ni > example.ini
