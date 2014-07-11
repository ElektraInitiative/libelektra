Add items here which should be implemented within the next release.
Release often, so keep this list very short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks)
Items are e.g. BUGS, OPTIMIZE, PROBLEMS, TESTING, PLUGINS, TODO,..


## 0.8.7 ##

gen:
	Contextual
	cleanup: utilities for cheetha+python
	Write tutorial

Starting points:
	add small Tutorial for gen
	add docu to examples

allow key names starting with / and meta keynames?

announcements: docu
	gen
	kdb <tool>
	tools

fix types:
	type checker should check like defined in schema
	let json use same types (double, boolean, nothing for string)

template naming conventions
refactor template code a bit (util class)


### test ###

have a single add_test place
install and search for all test_data in the same way

test libelektratools:
	addPlugin(Plugin) tests
	usePath functionality?
	codeReview

have icc on buildserver


### packaging ###

man pages:
- symlink kdb-full and kdb-static

tests package (depends on all other packages)
python+lua bindings (need #11)
