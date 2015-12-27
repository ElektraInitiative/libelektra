- infos = Information about ini plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org> Felix Berlakovich <elektra@berlakovich.net> 
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = 

## INTRODUCTION ##

This plugin allows read/write of INI files. INI files consist of simple
key value pairs of the form "key = value". Additionally keys can be
categorised into different sections. Sections must be enclosed in "[]",
for example "[section]". Each section is converted into a directory key
(without value) and keys below the section are located below the section
key. If the same section appears multiple times, the keys of all sections
with the same name are merged together under the section key.

## USAGE ##

If you want to add a ini file to the global key database, simply use mount:

    kdb mount file.ini /example ini

Then you can modify the contents of the ini file using set:

    kdb set user/example/key value
    kdb set user/example/section
    kdb set user/example/section/key value

Find out which file you modified:

    kdb file user/example

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

By default this feature is enabled.

## Array ##

`array`
repeating keys are treated as an array
```
[sec]
a = 1
a = 2
a = 3
a = 4
```
will be interpreted as
```
/sec
/sec/a
/sec/a/#0
/sec/a/#1
/sec/a/#2
/sec/a/#3

```

## Section ##

3 different sectioning modes are supported

`NONE`
sections wont be printed as `[Section]` but as part of the key name `section/key`
`NULL`
only binary keys will be printed as `[Section]`
`ALWAYS`
sections will be created automatically. This is the default setting.

## Merge sections ##
if the key `mergeSections` is set, duplicated sections will be merged into one.

