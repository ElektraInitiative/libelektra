Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).



# 0.8.16

Also see [githubs issues](http://git.libelektra.org/issues)
for other activities of the current release.

## start with

RPATH only where needed (+ where should elektraModulesLoad be?)

api:
	version rules ELEKTRA_ABI_13 ...
	include rules to allow elektra/kdb.h

to one cmake variable:
	verbose/debug -> logging
	pdf/on/off -> docu

remove "resolver" resolver
symlink resolver/storage?

## fixes

compiler warnings:
	key hash warning

add all plugins mem-leak test

check meta-data plugins with spec

export/import/editor should use KDB_DEFAULT_STORAGE as default

check: run reformat and check if something changed


#kdb

kdb setmeta with 2 args to remove meta data!
kdb set --file -F (read from file) reuse import/export?



## docu

traceability

make elektra-hierarchy reality:
	generate errors for spec/elektra/error
	install and mount-script for ini files in spec/elektra/metadata spec/elektra/modules

install METADATA.ini and CONTRACT.ini

provide information in:
	spec/elektra/metadata (METADATA.ini)
	spec/elektra/errors (libs/error/specification)
	spec/elektra/modules (CONTRACT.ini)


docu provide option in contract

docu specification mount

minimal generic description of commandline-options in kdb
	long specific description in man pages


## INI

as default backend (meta?)
ordering
comments

## Lua plugin

value transformations

## tools

kdb --profile for its own config

bookmark (+) feature



## cleanup

core, kdb.. remove useless symbols

## decisions

metastorage:
	keytometa?
	different plugins?
spec:
	black/whitelist
	removal of meta data
	fix hooks for validation
	abort on errors?

## cmake

make name(s) of variant and in which folder it is independent

autoadd plugins to PLUGINS? (avoid duplication in ElektraCache)

build all tests also with shared

remove ENABLE_TESTING or BUILD_TESTING


## lazy mountpoints

using list plugin
so that everything is lazy+arbitrary number of plugins

## types

consistent type vs. check/type
different type systems?
let json use same types (double, boolean, nothing for string)

## testing

create new test cases with shell script recorder

execute all examples to see if they do not crash or memleak
	assert test cases -> test cases (rename succeed_if)


## other stuff

debian package from upstream 3h

specification checker+application
	replaces struct+glob
	type inference with type classes
	stacking: apply links for whole hierarchy
	+ vendor overrides (apply additional data to specification)
	(beware of featuritis, only if adapts nicely in rest)
	conflict plugin

tooling:
	mounting with specification
	remove config without specification

type checker plugin redesign: take care of simplicity + working together with others
	set of types (min, max as 1-20, enums as user-defined types,...), space separated as now
	copy from thesis
	look into haskell type classes

