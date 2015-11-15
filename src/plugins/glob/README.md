- infos = Information about glob plugin is in keys below
- infos/author = Felix Berlakovich <elektra@berlakovich.net>
- infos/licence = BSD
- infos/needs =
- infos/ordering = check keytometa
- infos/stacking = no
- infos/provides = 
- infos/placements = presetstorage postgetstorage
- infos/description = copies metadata to keys with the help of globbing

## INTRODUCTION ##

This plugin adds metadata to keys identified by globbing expressions. The plugin copies the metadata of the 
corresponding globbing keys in its configuration. Globbing can be applied in get and set direction.



## GLOBBING KEYS ##

The plugin is configured with globbing keys in its configuration. Each key below the configuration is
interpreted as a globbing key. The value of the key contains the globbing expression. When a key matching 
the glob expression contained in one of the globbing keys is found, the metakeys of the corresponding 
globbing key are copied.

### GLOBBING DIRECTION ###

Globbing keys located directly below the configuration (e.g `config/glob/#1`) are applied in both directions
(get and set). Keys below "get" (e.g. `config/glob/get/#1`) are applied only in the get direction and keys below set
(e.g. `config/glob/set/#1`) are applied only in the set direction. 

### GLOBBING FLAGS ###

Globbing keys may contain a subkey named "flags". This optional key contains the flags to be passed to the
globbing function (currently fnmatch). If the key does not exist or if the value of the key cannot be
converted into a number, FNM_PATHNAME is used as a default (see fnmatch(3) for more details). 
 
