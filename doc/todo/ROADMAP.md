Here are items that should be implemented within the next releases.
We release often, so this list is short.
During implementation phase no new items may be added here (only
those which are necessary to fulfil the initial tasks).
A complete list of ideas what could be done can be found in the
[todo folder](.).


# 0.8.10 #

Also see [githubs issues](https://github.com/ElektraInitiative/libelektra/issues)
for other activities of the current release.

## powerful cascading ##

make / as logical root

arbitrary cascading
	ksLookup uses search folders (supplied by meta data)
	(e.g. /system/keybindings /user/keybindings /system/sw/myapp/keybindings...)
	allow fallback/override for contextual values

cascading for kdbGet/Set:
	read in spec and get/set needed subtrees

cascading for ksLookup:
	lookup in spec and use search folders and all requested domains

terminology:
	use consistent name for "user", "system", "local" and "spec"
	e.g. domains or namespaces?

getRootKeys vs. fixed

applications should only need to use:
kdbGet("/path/to/my/application")
ksLookup("/path/to/my/application/dir/key")
	(and even the strings can be avoided by code generation)

defaults are hardcoded (for system without /etc)
	just for information in spec

## mount specification ##


## test ##

test libelektratools:
	addPlugin(Plugin) tests
	usePath functionality?

full coverage of all plugins in end-to-end test
	create a directory value rewriter for ini+yajl


## OpenICC ##

rebase plugin
xdg resolver

fix types:
	type checker should check like defined in schema
	let json use same types (double, boolean, nothing for string)
