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


## ARRAY ##

The ini plugin handles repeating keys by turning them into an elektra array when the `array` config is set.

For example a ini file looking like:
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

## SECTION ##

The ini plugin supports 3 different sectioning modes:

* `NONE`
sections wont be printed as `[Section]` but as part of the key name `section/key`
* `NULL`
only binary keys will be printed as `[Section]`
* `ALWAYS`
sections will be created automatically. This is the default setting.

## MERGE SECTIONS ##

The ini plugin supports merging duplicated section entries when the `mergesections` config is set.
The keys will be appended to the first occurrence of the section key. 


## ORDERING ##

The ini plugin preserves the order.
Inserted subsections get appended to the corresponding parent section and new sections by name.

Example:

```
% cat test.ini

[Section1]
key1 = val1
[Section3]
key3 = val3


% kdb set system/test/Section1/Subsection1/subkey1 subval1
% kdb set system/test/Section2/key2 val2
% cat test.ini

[Section1]
key1 = val1
[Section1/Subsection1]
subkey1 = subval1
[Section2]
key2 = val2
[Section3]
key3 = val3


```
