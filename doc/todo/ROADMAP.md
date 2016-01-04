Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).


# 0.8.15

Also see [githubs issues](http://git.libelektra.org/issues)
for other activities of the current release.

## docu

more docu in METADATA.ini

make elektra-hierarchy reality:
	generate errors for spec/elektra/error
	install and mount-script for ini files in spec/elektra/metadata spec/elektra/modules


docu provide option in contract

docu specification mount


## mount specification

improve support for "provide"
give defaults for provide using needs/recommends specification
default storage, encode,...

## Lua plugin

value transformations

## list plugin

use it for mounting
	so that everything is lazy+arbitrary number of plugins


## global plugins

design decisions

Fix Race bug: with lock in global plugins

list plugin (for global plugins): takes array of plugins
	processes all plugins in a row

accumulate split info (nr changed, added, removed keys and at which mountpoints)
needed for logging, notif plugins

## tools

global mountpoints with unserialize

--profile

bookmark (+) feature

## meta data ##

install METADATA.ini and CONTRACT.ini

provide information in:
	spec/elektra/metadata (METADATA.ini)
	spec/elektra/errors (libs/error/specification)
	spec/elektra/modules (CONTRACT.ini)

fix types: (also in 0.8.14)
	type checker should check like defined in schema
	let json use same types (double, boolean, nothing for string)

## testing

execute all examples to see if they do not crash or memleak
	assert test cases -> test cases (rename succeed_if)

create new test cases with shell script recorder

## packaging

full jessie build+script+external
copy debian/ from official





# 0.8.16

## start with

api:
	version rules ELEKTRA_ABI_13 ...
	include rules to allow elektra/kdb.h

to one cmake variable:
	verbose/debug -> logging
	pdf/on/off -> docu

## other stuff

md_src_plugins_keytometa_README.3elektra

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

