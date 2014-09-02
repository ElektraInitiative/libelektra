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



## RESTRICTIONS ##

Currently the plugin has the following shortcomings:

- newlines are not restored on write
- regardless of which comment was used originally, it is always written
  back as ';'
- support for multiline INI is a compiletime option and is currently
  disabled

The plugin is still work in progress.
