Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).




# 0.8.13

Also see [githubs issues](https://github.com/ElektraInitiative/libelektra/issues)
for other activities of the current release.

run_all really should use dump
(xdg) variable for spec?

cascading export nickel?

document METADATA.ini

genopt.hpp + include enforcement in template_dynamic?

install kdb gen properly
	search by default in installed pathes

better errnostore solution?
	reset errno in user functions and avoid code in every plugin
	always provide last errno in ADD_WARNING+SET_ERROR
	use safe implementation see
	https://github.com/fish-shell/fish-shell/commit/c70e92e98d34e14b1e1310a10677b7c0f6e2b54c
	(even strerrno_r seems to deadlock from time to time because of translations?)

C representation (ksNew(..keyNew(...))) is the *most* common representation, still no plugin
exists for it

system/env should be array
kdb tool should work with arrays (add_entry, remove_entry)

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


## Backend ##

generic simpleini (customizable printf/scanf format strings)

## simplify cmake ##

c++11
DEFAULT_STORAGE/RESOLVER
to one cmake variable:
	verbose/debug -> logging
	pdf/on/off -> docu

fix dependency problem with kdberrors.h

## bug hunting

all plugins + kdb shell with afl

