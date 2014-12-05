Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).


# 0.8.11 #

Also see [githubs issues](https://github.com/ElektraInitiative/libelektra/issues)
for other activities of the current release.

initial warnings 22, 93 when no /etc/kdb
test racing


## fix relative ##

plugins should use relative pathes so that import/export/remount works

- dump
- ni

## simplify cmake ##

c++11
DEFAULT_STORAGE/RESOLVER


## powerful cascading ##

make / as logical root

cascading for kdbGet/Set:
	read in spec and get/set needed subtrees

applications should only need to use:
kdbGet("/path/to/my/application")
ksLookup("/path/to/my/application/dir/key")
	(and even the strings can be avoided by code generation)

defaults are hardcoded (for system without /etc)
	just for information in spec


## meta data ##

fix comments:
	all plugins should use new comment-approach

fix types:
	type checker should check like defined in schema
	let json use same types (double, boolean, nothing for string)
