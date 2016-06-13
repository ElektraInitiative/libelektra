elektra-plugins-framework(7) -- Background about plugins framework
==================================================================

Many component systems pass information between the various components
by calling methods of each other.  This is not the way Elektra's plugin
system works.  Instead, the core passes a `KeySet` object in one direction
from plugin to plugin. So they form a so called pipes-and-filter.  Each of
the plugins can modify the configuration or add any other information
using metakeys.  While this approach is in general less flexible, this
information flow still allows powerful chaining.  Because plugins do
not have to bother to call other plugins, the plugin development is
also easier.  The ordering of plugins in backends is controlled using
contracts.


## Contracts

Every plugin should provide a full contract to give information how it
will work with other plugins.  Most parts of the contract are obligatory.
Plugins cannot be loaded without this information.  For example, plugins
must provide the clause `infos/version`.  It is vital so that the plugin
loader knows which version of Elektra the plugin was built for.

## Conditions

It is, however, up to the plugin not to have every clause of the contract.
For example, the plugin might not tell what it provides or needs.  It can
also leave any description out.  In this situation it is unclear what
the plugin will do.  Such a plugin can add or remove keys, and changes
values and metadata regardless what other plugins expect.  If only such
plugins existed there would be chaos.  It would be impossible to determine
the behaviour of a backend which uses a multiple of such plugins.

To avoid this situation, every plugin exports a contract describing how
the plugin modifies the `KeySet` `returned`.  Most often it is enough
to state that it is a *storage* plugin or that it will *filter* keys.

The data structures, however, are already responsible for most of the
pre- and postconditions.  Every condition the data structure guarantees,
takes away a concern for the plugins.  All the parts that are already
guaranteed by data structures do not need to be stated in the contract.

Plugins should not be burdened to check too many postconditions.  Instead,
plugins focus on their task.  The plugin does not need to check the sync
flag of keys or if the keys are below the mount point. The core already
guarantees correct behaviour as described
in [algorithm](/doc/help/elektra-algorithm.md).

To sum up, contracts give the information how a plugin interacts with
others.  It describes if, and how, the `KeySet` `returned` is changed.
Using contracts, we get a predictable behaviour, but still support every
kind of plugin.


## Exporting Contracts

As already stated, some parts of the contracts are obligatory.
`kdb mount` needs to know which symbols the plugin exports.  Only the
`elektraPluginGet()` symbol is mandatory - it is used to yield this
information.  Elektra's core also uses the functions `elektraPluginSet()`,
`elektraPluginError()`, `elektraPluginOpen()` and `elektraPluginClose()`
if available.  Other functions like `serialise`, `unserialise` or
`lookup` which implement special features can be supported, but are
ignored by the core.  For the user of the library these functions can
be very useful.  These functions shall either belong to the concern of
the plugin or be implemented within the plugin because of the dependences.

As described in [infos/provides](/doc/CONTRACT.ini), the plugin can
also provide descriptive information, for example about the author and
the licence.  Advanced plugins can also export plugin configuration for
other plugins so that the overall backend works properly.  Last, but not
least, as enumerated in [infos/placement](/doc/CONTRACT.ini) dependency
and placement information makes the system reliable and robust.  With that
information, plugins can be placed into a backend in an automatic and
secure way.

`system/elektra/modules` provides for every module the information
described above.  The entry exists once a plugin of that module is loaded.
For each module a special *module backend* is generated and mounted at
`system/elektra/modules/<pluginname>`.  The `elektraPluginGet()` function
generates this described contract on requests.

For example, the ccode plugin, implements:

	int elektraCcodeGet(Plugin *handle, KeySet *returned, Key *parentKey)
	{
		if (!strcmp (keyName(parentKey), "system/elektra/modules/ccode"))
		{
			KeySet *contract = ksNew (30,
				keyNew ("system/elektra/modules/ccode",
					KEY_END),
				keyNew ("system/elektra/modules/ccode/exports",
					KEY_END),
				//...
				KS_END);
			ksAppend (returned, contract);
			ksDel (contract);
			return 1;
		}
		// implementation of elektraCcodeGet

We see in the listing above that the plugin generates and returns
the contract if, and only if, the name of the `parentKey` is
`system/elektra/modules/ccode`.  The user and the contract checker can
access the contract of ccode below the key `system/elektra/modules/ccode`
in the same way other configuration is accessed.  Note that we also
have to `return 1` at the end of the contract to not execute the regular
functionality of the plugin.


## Changing Plugins

This configuration is static and contains the contract information.
In theory, the contract can be changed without any problems in ways that
it provides more and obligates less.  But the problem is that it will
not be checked if this is the case because a recheck of the contracts
of a backend is very expensive. The contract checker doing this, only
runs once during mount time.  Changing contracts in an incompatible way
forces the user to remove all mount points where the plugin is and mount
it again.  Such actions are only sustainable in a development phase and
not in a productive environment.

But the plugin's implementation is allowed to change without being
remounted if it is a subtype of the earlier version.  Only in this
situation it can be a drop-in replacement.  With a good testing framework
the behaviour can be checked to some extent.

We also see in the listing above that the code responsible for generating
the contract and the code for the implementation are next to each other.
Plugins need to satisfy those self-imposed obligations that are described
in contracts.  They ensure that plugins interact in predictable ways.
So the process of writing individual plugins and composing them together
can be described as Component-Based Software Engineering.

Plugins can also be viewed as framework extensions.  A component abstracts
plugins.  But this term is misleading in our case, because components
usually can choose which interfaces they implement.  Elektra's plugins,
however, are restricted to implement one specific interface.  Without
contracts, plugins could not interact as described in this chapter.


## SEE ALSO

- [elektra-plugins-ordering(7)](elektra-plugins-ordering.md)
- [elektra-contracts(7)](elektra-contracts.md)
