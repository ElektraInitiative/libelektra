- infos = Information about keytometa plugin is in keys below
- infos/author = Felix Berlakovich <elektra@berlakovich.net>
- infos/licence = BSD
- infos/provides = filter
- infos/needs =
- infos/placements = presetstorage postgetstorage
- infos/status = maintained unittest nodep libc configurable
- infos/metadata = rename/to rename/toupper rename/tolower rename/cut
- infos/description = renaming of keys

# INTRODUCTION #


This plugin can be used to perform rename operations on keys. This is useful if a backend does not provide keys
with the required names or case.

If keys are renamed, their original name is stored in the `origname` MetaKey.

There are 2 types of transformations: 
* basic
* advanced


# BASIC TRANSFORMATIONS #

are applied before and after the advanced transformations.

## GET ##

first transformation to be applied to all keys on kdbGet.

`get/case`
* toupper
* tolower
* unchanged    // this is the default value if no configuration is given

converts the whole keyname below the parentKey to upper- or lowercase. if no configuration or `unchanged` is used no transformation is done here.

## SET ##

last transformation to be applied to all keys on kdbSet.

`set/case`
* toupper
* tolower
* keyname
* unchanged   // this is the default value if no configuration is given

`toupper` or `tolower` tells the rename plugin to convert the whole keyname below below the parentKey to lower or uppercase.

`unchanged` returnes the key to it's original name

`keyname` tells the plugin to keep the name of the advanced transformation

# ADVANCED TRANSFORMATIONS #

## CUT ##

### OPERATION ###

The cut operation can be used to strip parts of a keys name. The cut operation is able to cut anything below the path
of the parent key. A renamed key may even replace the parent key. For example consider a KeySet with the
parent key `user/config`. If the KeySet contained a key with the name `user/config/with/long/path/key1`, the cut operation
would be able to strip the following key name parts:

* with
* with/long
* with/long/path
* with/long/path/key1

### CONFIGURATION ####

The cut operation takes as its only configuration parameter the key name part to strip. This configuration can be supplied in two
different ways. First, the global configuration key `cut` can be used. Second, keys to be stripped can be tagged with the MetaKey `rename/cut`.
If both options are given, the MetaKey takes precedence. For example, consider the following setup:

	config/cut = will/be
	parent key = user/config

	user/config/will/be/stripped/key1		<- meta rename/cut = will/be/stripped
	user/config/will/be/stripped/key2		<- meta rename/cut = will/be/stripped
	user/config/will/be/stripped/key3
	user/config/will/not/be/stripped/key4

The result of the cut operation would be the following KeySet:

	user/config/key1
	user/config/key2
	user/config/stripped/key3
	user/config/will/not/be/stripped/key4

The cut operation is agnostic to a single trailing slash in the configuration. This means that it makes no difference whether `cut = will/be/stripped`
or `cut = will/be/stripped/`. However, the cut operation refuses cut paths with leading slash. This is to clarify that key name parts can only be stripped
after the parent key path.

If an invalid configuration is given or the cut operation would cause a parent key duplicate, the affected keys are simply skipped and not renamed.


## REPLACE ##

Using the `/replacewith` global key or `rename/to`  MetaKey the rename plugin will replace the part removed by `cut` with the supplied String


### TO UPPER / LOWER ###

Using the `/toupper` or `/tolower` global configuration key, or the `rename/toupper` or `rename/tolower` metakey the rename plugin will
convert the keynames to uppercase or lowercase.
The supplied values tell the plugin how many levels starting from the right will be converted. `toupper` and `tolower` can be combined.
When no value or "0" is supplied with the keys the whole name below the parentkey will be converted.

The toupper/tolower conversions are applied after cut/replace.

Note that the names refer to the representation as KeySet. For example, if you have a configuration file where every name is uppercase:
```
KEY=value
OTHER/KEY=otherval
```
you can use `tolower=0` to get the keys `key` and `other/key`.

### EXAMPLE ###

```
kdb mount caseconversion.ini /rename ini rename toupper=1,tolower=3

kdb set /rename/MIXED/CASE/conversion 1

kdb ls /rename
user/rename
user/rename/mixed/case/CONVERSION
```

```
% cat renameTest.ini
test/removed/key = test

% kdb mount renameTest.ini /rename ini rename get/case=toupper,set/case=keyname,/cut=REMOVED
% kdb ls /rename
user/rename/TEST/KEY

% kdb set /rename
Using name user/rename
Create a new key user/rename with null value
% cat renameTest.ini
TEST/KEY = test
```

If you always want the keys in the configuration file upper case,
but for your application lower case you would use:
```
$ kdb mount caseconversion.ini /rename ini rename get/case=tolower,set/case=toupper
$ kdb set user/rename/section/key valu
$ cat ~/.config/caseconversion.ini
[SECTION]
KEY = value
```



## PLANNED OPERATIONS ##


Additional rename operations are planned for future versions of the rename plugin:
* trim: remove spaces in the name (that are not part of parentKey)
* ranges for toupper, tolower
* allow one to specify the case in configuration files (#485)
