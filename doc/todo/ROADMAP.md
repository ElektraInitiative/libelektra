Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).

Also see [githubs issues](http://git.libelektra.org/issues)
for other activities of the current release.




# 0.8.19

import/editor validate strategy

improvements in tutorials!

use shell recorder for README.md
	make examples more consistent

fix ENABLE_DEBUG


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





# 0.8.20

## tests

helper function for searching symbol

kdb mount:
	-1 -2 options?
	reject same files!
	lazy mount (with list plugin)
	arbitrary number of plugins (list plugin)
	new import/export


## fixes

compiler warnings:
	key hash warning


## types

consistent type vs. check/type
different type systems?
let json use same types (double, boolean, nothing for string)

type checker plugin redesign: take care of simplicity + working together with others
	set of types (min, max as 1-20, enums as user-defined types,...), space separated as now
	look into haskell type classes




