Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).




# 0.8.13

Also see [githubs issues](http://git.libelektra.org/issues)
for other activities of the current release.

## start

Start with:
	api! (Namespaces, encoded, versioning)
	version rules ELEKTRA_ABI_13 ...
	Cleanups

highlevel API: getenv()
	ld_preload
	--wrap=symbol __libc_start_main for argc /sw/argv[0]

genopt.hpp + include enforcement in template_dynamic?
	use spec namespace
	generic command-line parsing

link fetcher plugin: kdbGet() of all parts where links point to
	test with ipe

## global plugins

design decisions

Fix Race bug: with lock in global plugins

list plugin (for global plugins): takes array of plugins
	processes all plugins in a row

accumulate split info (nr changed, added, removed keys and at which mountpoints)
needed for logging, notif plugins

## mount specification

improve support for "provide"
give defaults for provide using specification
default storage, encode,...

## tools

global mountpoints with unserialize

## testing

cmd execution testing framework
	execute commands, capture stdout, stderr and kdb
	replay and see if same side-effects

execute all examples to see if they do not crash or memleak
	assert test cases -> test cases (rename succeed_if)

full jessie build+script+external
copy debian/ from official

(xdg) variable for spec namespace?

cascading export nickel?

more docu in METADATA.ini

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

kdb edit
	+markdown converter also for console tools?

roresolver: check if something is modified
	for version, constants, uname

## meta data ##

provide information in spec/elektra/metadata

fix comments:
	all plugins should use new comment-approach

fix types: (also in 0.8.14)
	type checker should check like defined in schema
	let json use same types (double, boolean, nothing for string)

## qt-gui

dbus-listener
auto-completion for metadata

"kdb check -a" rewrite file?

## fix relative ##

plugins should use relative pathes so that import/export/remount works

- dump
- ni
- tcl

## Backend ##

generic simpleini (customizable printf/scanf format strings)

## simplify cmake ##

c++11
DEFAULT_STORAGE/RESOLVER
to one cmake variable:
	verbose/debug -> logging
	pdf/on/off -> docu

fix dependency problem with kdberrors.h


# 0.8.14

specification checker+application
	replaces struct+glob
	type inference with type classes
	stacking: apply links for whole hierarchy
	+ vendor overrides (apply additional data to specification)
	(beware of featuritis, only if adapts nicely in rest)

tooling:
	mounting with specification
	remove config without specification

redo type checker plugin: take care of simplicity + working together with others
	set of types (min, max as 1-20, enums as user-defined types,...), space separated as now
	copy from thesis
	look into haskell type classes
