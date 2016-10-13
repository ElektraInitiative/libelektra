- infos = Information about the tcl plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage
- infos/needs = code
- infos/placements = setstorage getstorage
- infos/status = unfinished nodoc concept
- infos/description = Serialize tcl lists

## Introduction ##

This plugin is a storage plugin which write keys to lists *in the style of*
the Tcl programming language.

## Format

The format does not have significant spaces.  The advantage of TCL style
lists is that also arbitrary meta data can be embedded in a natural and
distinguish-able style. It looks like:

```
{     {key=val {metakey=b} {comment  =  huhu  }   }  }
```

## Limitations

- empty and null keys not supported
- whitespaces are discarded
- no comments


## Dependencies ##

- `libboost-dev`


