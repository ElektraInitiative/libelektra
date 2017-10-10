- infos = Information about the tcl plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/tcl
- infos/needs = code
- infos/placements = setstorage getstorage
- infos/status = unfinished concept
- infos/description = Serialize tcl lists

## Introduction

This plugin is a storage plugin which writes keys to lists *in the style of*
the Tcl programming language.

## Format

The format does not have significant spaces.  The advantage of TCL style
lists is that also arbitrary metadata can be embedded in a natural and
distinguish-able style. It looks like:

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

## Example

```sh
# Mount Tcl plugin to namespace `user/examples/tcl`
sudo kdb mount config.tcl user/examples/tcl tcl

# Add a key value pair to the database
kdb set user/examples/tcl/key value
# The Tcl plugin also supports metadata
kdb setmeta user/examples/tcl/key comment "This key contains example data."
# A known limitation of the plugin is that it discards whitespace characters
kdb getmeta user/examples/tcl/key comment
#> Thiskeycontainsexampledata.

kdb export user/examples/tcl tcl
#> {
#> 	{
#> 		user/examples/tcl/key = value
#> 		{
#> 			comment = Thiskeycontainsexampledata.
#> 		}
#> 	}
#> }

# Undo modifications
kdb rm -r user/examples/tcl
sudo kdb umount user/examples/tcl
```

## Limitations

- empty and null keys not supported
- whitespaces are discarded
- no comments

## Dependencies

- `libboost-dev`


