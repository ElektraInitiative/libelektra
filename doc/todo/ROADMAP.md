Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).


# 0.8.14

md_src_plugins_keytometa_README.3elektra
malloc -> elektraMalloc

# 0.8.15

Also see [githubs issues](http://git.libelektra.org/issues)
for other activities of the current release.

## start with

api:
	add keyGetNamespace
	version rules ELEKTRA_ABI_13 ...

DEFAULT_STORAGE/RESOLVER

to one cmake variable:
	verbose/debug -> logging
	pdf/on/off -> docu

split elektra:
	Libelektra-core (only key+keyset)
	libelektra-kdb
	libelektra-meta
	libelektra-ease
	libelektra-proposal
	libelektra links everything together


## docu

more docu in METADATA.ini

checking for broken links

API docu improvements+pdf export

generate graphic for overview of plugins

# further

genopt.hpp + include enforcement in template_dynamic?
	use spec namespace
	generic command-line parsing
	%% context evaluation?

link fetcher plugin: kdbGet() of all parts where links point to

single file plugins for /proc

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

kdb edit
	+markdown converter also for console tools?

kdb tool should work with arrays (add_entry, remove_entry)

cascading export, e.g. for nickel?

install kdb gen properly
	search by default in installed pathes

## testing

execute all examples to see if they do not crash or memleak
	assert test cases -> test cases (rename succeed_if)

cmd execution testing framework
	execute commands, capture stdout, stderr and kdb
	replay and see if same side-effects

full jessie build+script+external
copy debian/ from official

better errnostore solution?
	reset errno in user functions and avoid code in every plugin
	always provide last errno in ADD_WARNING+SET_ERROR
	use safe implementation see
	https://github.com/fish-shell/fish-shell/commit/c70e92e98d34e14b1e1310a10677b7c0f6e2b54c
	(even strerrno_r seems to deadlock from time to time because of translations?)

roresolver: check if something is modified
	for version, constants, uname
	and/or remove value comparision in those plugins
	kdbGet should return 0 if nothing modified, too

## meta data ##

provide information in spec/elektra/metadata

fix comments:
	all plugins should use new comment-approach
	(also iconv)

fix types: (also in 0.8.14)
	type checker should check like defined in schema
	let json use same types (double, boolean, nothing for string)

## qt-gui

dbus-listener + notify single events, maybe zeromq
auto-completion for metadata

## fix relative ##

plugins should use relative pathes so that import/export/remount works

- dump
- ni
- tcl

## Backend ##

generic simpleini (customizable printf/scanf format strings)


## Announce goal

We shifted our [goals](http://git.libelektra.org/blob/master/doc/GOALS.md) a bit:
We want to prefer simplicity to flexibility!
	use it also for normal plugins (pre, postfilter)
but every way is optional, if you want you can use it, otherwise you can leave it out:
- as primitive key/value storage
- with specification
- with code generation
- ...

but no flexibility regarding:
- namespaces are only useful for configuration (not for arbitrary key/value)
- mounting and contracts functionality
- error code meanings are fixed, if a resolver detects a conflict, error #30 must be used
- of course ABI, API



# 0.8.16

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
