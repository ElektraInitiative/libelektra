Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).


# 0.8.11 #

Also see [githubs issues](https://github.com/ElektraInitiative/libelektra/issues)
for other activities of the current release.

genopt.hpp + include enforcement in template_dynamic?

install kdb gen properly
	search by default in installed pathes

initial warnings 22, 93 when no /etc/kdb
test racing

KEY_CASCADING_NAME must die

document METADATA.ini

## fix broken tests ##

shell:
	test resolver (dir, spec)
	test distribution (nesting+double slash issues)

ctest:
	test_mountsplit
	test_splitset
	order
	kdb


## Backend ##

unserialize
streaming

## simplify cmake ##

c++11
DEFAULT_STORAGE/RESOLVER





# 0.8.12

better errnostore solution?

C representation (ksNew(..keyNew(...))) is the *most* common representation, still no plugin
exists for it

## meta data ##

fix comments:
	all plugins should use new comment-approach

fix types:
	type checker should check like defined in schema
	let json use same types (double, boolean, nothing for string)

## fix relative ##

plugins should use relative pathes so that import/export/remount works

- dump
- ni
