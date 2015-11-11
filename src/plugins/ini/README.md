- infos = Information about ini plugin is in keys below
- infos/author = Felix Berlakovich <elektra@berlakovich.net>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = ini file backend

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

    kdb mount file.ini /test ini

Then you can modify the contents of the ini file using set:

    kdb set user/test/key value
    kdb set user/test/section
    kdb set user/test/section/key value

Find out which file you modified:

    kdb file user/test


## SECTIONS ##

When converting a KeySet to an INI file it is important to differentiate between
regular keys and section keys. The ini plugin treats all keys with a binary NULL
value as section key. Note that binary NULL is not the same as an empty value (i.e. "").

For example consider the following ini file:

				[section1]
				key1 =
				key2 = value2
			
This would result in a KeySet containing three keys. The section key `section1` would be
a binary key with value NULL (i.e. 0). `section1/key1` would be a regular key with value "".
`section1/key2` would be a regular key with value "value2". In the other direction this requires
section keys to be manually created with a binary NULL value.  

For example consider the following KeySet:

				section1 = ""
				section1/subkey = "value1"
				
Although section1 might look like a section, it would result in the following ini file:

				section1 =
				section1\/subkey = value1

### AUTOSECTIONS ###

Creating the section keys manually can be cumbersome. This is especially true because
KeySets resulting in INI files usually do not contain keys with a depth greater than
1 relative to the parent key. For that reason a good guess can be made what should be
sections and what not. This is done by activanting the autosections option. 

The autosections feature can be enabled by creating a key named `autosections` 
in the configuration of the plugin. Note that only the existence of the key is relevant, not its value.

As soon as this key exists, the plugin will automatically create section keys for keys
with a depth greater than 1 relative to the parent key.	

For example consider the following ini file:

				[section1]
				key1 =
				key2 = value2
				[section2]
				key3 = value3
				
Without the autosections option the following KeySet is required to create this ini file:

				parent/section1 = NULL
				parent/section1/key1 = ""
				parent/section1/key2 = "value2"
				parent/section2 = NULL
				parent/section2/key3 = "value3"

The section keys have to be inserted manually. With the autosections option the KeySet can
be reduced to the following:

				parent/section1/key1 = ""
				parent/section1/key2 = "value2"
				parent/section2/key3 = "value3"

All three keys have a depth greater than 1 relative to the parent key `parent`. Therefore
the key name part directly below the parent key is considered to be the section name.
For example, for the keys `parent/section1/key1` and `parent/section1/key2` `section1` is considered
to be the section and is automatically created in the INI file.


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
