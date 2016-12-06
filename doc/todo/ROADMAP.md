Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).

Also see [githubs issues](http://git.libelektra.org/issues)
for other activities of the current release.






# 0.8.20

## kdb tool

global-umount

kdb set should always validate
	(no matter how namespace is given)

mount:
	-1 -2 options?
	reject same files!
	lazy mount (with list plugin) -> later?
	arbitrary number of plugins (list plugin) -> later?
	new import/export -> later?

import/editor validate strategy


## tuts

further improvements in tutorials!

update global-mount (not needed at most parts anymore)

add more traceability

use shell recorder for further README.md


## tests

helper function for searching symbol


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




