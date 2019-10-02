- infos = Information about the tcl plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/tcl
- infos/needs = code null binary
- infos/placements = setstorage getstorage
- infos/status = unfinished concept
- infos/description = Serialize tcl lists

## Introduction

This plugin is a storage plugin which writes keys to lists _in the style of_
the Tcl programming language.

## Format

The format does not have significant spaces. The advantage of TCL style
lists is that also arbitrary metadata can be embedded in a natural and
distinguish-able style. It looks like:

```
{
    {
        key=val
        {
            metakey = b
        }
        {
            comment = huhu
        }
    }
}
```

## Basic Usage

```sh
# Mount Tcl plugin to namespace `user/examples/tcl`
# We add the required plugins (instead of the plugin providers)
# for `tcl` manually, since otherwise this command leaks memory.
sudo kdb mount config.tcl user/tests/tcl tcl ccode null base64

# Add a key value pair to the database
kdb set user/tests/tcl/key value
# The Tcl plugin also supports metadata
kdb meta-set user/tests/tcl/key comment "This key contains example data."
# A known limitation of the plugin is that it discards whitespace characters
kdb meta-get user/tests/tcl/key comment
#> Thiskeycontainsexampledata.

kdb export user/tests/tcl tcl
#> {
#> 	{
#> 		key = value
#> 		{
#> 			comment = Thiskeycontainsexampledata.
#> 		}
#> 	}
#> }

# Undo modifications
kdb rm -r user/tests/tcl
sudo kdb umount user/tests/tcl
```

## Binary Data

The plugin also supports binary data via the [base64 plugin](../base64/) and null keys via the [null plugin](../null/).

```sh
# Mount plugin
sudo kdb mount config.tcl user/tests tcl ccode null base64

# Import some data
kdb import user/tests/dump xmltool < src/plugins/xmltool/xmltool/dump.xml

# Take a look at imported data
kdb ls user/tests/dump
#> user/tests/dump/.HiddenBinaryKey
#> user/tests/dump/.HiddenDirectoryKey
#> user/tests/dump/.HiddenStringKey
#> user/tests/dump/PerfectBinaryKey
#> user/tests/dump/PerfectDirectoryKey
#> user/tests/dump/PerfectStringKey
#> user/tests/dump/Ug.ly:Bin@a€ryKey
#> user/tests/dump/Ug.ly:Dir@ect€oryKey
#> user/tests/dump/Ug.ly:St@ri€n.gKey

# The plugin supports binary data…
kdb get user/tests/dump/PerfectBinaryKey
#> \x42\x69\x6e\x61\x72\x79\x56\x61\x6c\x75\x65\x0

# … and empty keys
kdb get user/tests/dump/Ug.ly:Bin@a€ryKey

# Undo modifications
kdb rm -r user/tests
sudo kdb umount user/tests
```

## Limitations

- whitespaces are discarded
- no comments

## Dependencies

- `libboost-dev`
