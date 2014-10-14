- infos = Information about ini plugin is in keys below
- infos/author = Felix Berlakovich <elektra@berlakovich.net>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = ini file backend

## INTRODUCTION ##

This plugin allows reading of INI files. INI files consist of simple
key value pairs of the form "key = value". Additionally keys can be
categorised into different sections. Sections must be enclosed in "[]",
for example "[section]". Each section is converted into a directory key
(without value) and keys below the section are located below the section
key. If the same section appears multiple times, the keys of all sections
with the same name are merged together under the section key.



## COMMENTS ##

The ini plugin supports the use of comments. Comment lines start with
a ';' or a '#'. Adjacent comments are merged together (separated by
"\n") and are put into the comment metadata of the key following the
comment. This can be either a section key or a normal key.



## MULTILINE SUPPORT ##

The ini plugin supports multiline ini values. Continuations of previous values
have to start with whitespace characters. 

For example consider the following ini file:

				key1 = value1
				key2 = value2
					with continuation
					lines

This would result in a KeySet containing two keys. One key named `key1` with the value `value1` and 
another key named `key2` with the value `value2\nwith continuation\nlines`.

By default the support for multiline values is disabled because formatting 
(whitespaces before keynames) may be accidentially mixed with value continuations. 
The multiline support can be enabled by creating a key named `multiline` in the configuration 
of the plugin. Note that only the existence of the key is relevant, not its value. As soon as this
key exists, the plugin treats lines starting with whitespaces as continuations of the previous key value.
Keep in mind, that writing multiline values is also only supported if the multiline support is turned on.
Otherwise the plugin will refuse to write key values with newlines.



## RESTRICTIONS ##

Currently the plugin has the following shortcomings:

- formatting newlines are not restored on write
- regardless of which comment was used originally, it is always written
  back as ';'

The plugin is still work in progress.
