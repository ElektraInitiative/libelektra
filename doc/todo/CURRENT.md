Add items here which should be implemented within the next release.
Release often, so keep this list very short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks)
Items are e.g. BUGS, OPTIMIZE, PROBLEMS, TESTING, PLUGINS, TODO,..


## 0.8.8 ##

allow more key names
	starting with /
	meta keynames
	more powerful cascading (ksLookup + kdbGet)
	deprecate ksLookupByName

fix resolver:
	add sync

fix types:
	type checker should check like defined in schema
	let json use same types (double, boolean, nothing for string)


### test ###

test libelektratools:
	addPlugin(Plugin) tests
	usePath functionality?
	codeReview

have icc on buildserver

