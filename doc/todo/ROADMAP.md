Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).



# 0.8.19

Also see [githubs issues](http://git.libelektra.org/issues)
for other activities of the current release.

## tests

helper function for searching symbol
lazy kdb mount+new imp/exp
kdb mount -1 -2 options?

use shell recorder for README.md
	make examples more consistent

## lazy mountpoints

using list plugin
so that everything is lazy+arbitrary number of plugins

## fixes

compiler warnings:
	key hash warning

## docu

add traceability

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

## cleanup

core, kdb.. remove useless symbols

elektraRemoveOneLevel
keyGetParentName

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

to one cmake variable:
	verbose/debug -> logging
	pdf/on/off -> docu

build all tests also with shared

remove ENABLE_TESTING or BUILD_TESTING


## types

consistent type vs. check/type
different type systems?
let json use same types (double, boolean, nothing for string)

type checker plugin redesign: take care of simplicity + working together with others
	set of types (min, max as 1-20, enums as user-defined types,...), space separated as now
	look into haskell type classes


## other stuff

specification checker+application
	type inference with type classes
	stacking: apply links for whole hierarchy
	+ vendor overrides (apply additional data to specification)
	(beware of featuritis, only if adapts nicely in rest)
	conflict plugin

tooling:
	remove config without specification






