# Plugin Variants

## Issue

Some plugins are generic in the sense that they cannot fully
define their contract statically.
(Note: if they can decide it statically, you should prefer
 [compilation variants](/doc/tutorials/compilation-variants.md).)
Instead their contract is based on their
configuration. We will call every combination of plugins+configuration
where we get a different contract **plugin variant**.

The current issue is that there is no way to enumerate
possible plugin variants as needed to list all functionality
of Elektra.


## Constraints

- User Customizability:
  - Users must be able to add new plugin variants
  - Users should be able to suppress plugin variants
- Modularity: 
  - The plugins themselves yield a list of plugin variants
  - The code defining variants is part of the plugin's code
  - To get a full list of all variants of all plugins, one
    simply has to iterate over all plugins and ask the plugins
    for possible variants.
- libtool should provide a convenience layer for easy access of all
  plugin variants. In this case there should also be a performance
  optimization, that plugins do not need to be reopened required
  multiple times.


## Assumptions

- The number of variants needs to be bounded:
  plugin variants need to be fully enumerable


## Considered Alternatives

A specification of the plugin's configuration and a tool that
can enumerate all possible essential configurations. Issues here
are:

- Plugins might need to self-reference (a validation plugin
  might have plugin variants, too)
- Elektra's specification language was not complete nor consistent at point
  of writing
- Does not fit with the `checkconf` ([see here](http://git.libelektra.org/issues/559))
  approach.


## Decision

Provide a function `int genconf (KeySet * ks, Key * errorKey)` where `ks`
contains a list of all variants with the essential configuration
and the changed parts of the contract as subkeys.

E.g. for augeas:
```
access
access/config
access/config/lens = Access.lns
access/infos
access/infos/format = access
aliases
aliases/config
aliases/config/lens = Aliases.lns
aliases/infos
aliases/infos/format = aliases
```

Which would enlist `augeas#access`

E.g. for python:
```
configparser/config
configparser/config/script = python_configparser.py
```


The same keys can be enhanced or overwritten in
`system/elektra/plugins/variants`.

The keys defined in `system/elektra/plugins/variants` have precedence.


The absence of parts mean:
- `config`: The default variant (given no parameter), if it makes sense
  (has useful functionality)
- `infos`: It is not possible to determine the changes of the contract,
  the user need to instantiate the plugin and enquiry the contract.
- `config` and `infos`: To manually suppress a plugin variant to not
  enlist it and do not try to instantiate it.



## Argument

- The `genconf` API was chosen to be consistent with `checkconf`.
  The pluginhandle might be missing for some scenarios.
  If it is needed, we should change both APIs for consistency reasons.
- The very verbose (deeply nested) subkeys (and names instead of arrays) provide
  better customizability and extensibility.

## Implications

- `genconf` needs to be implemented for the plugins:
 - augeas
 - jni
 - lua
 - python
 - python2
- `PluginDatabase` needs an extension to list all plugin variants
- `kdb list` should be able to list all variants, e.g. like:
  `augeas lens=Access.lns`
  `augeas format=access`
  so that a user can copy and paste this for the `kdb mount` command.


## Related decisions

- [Global Plugins](global_plugins.md)


## Notes

Discussions took place [here](http://git.libelektra.org/issues/1006).
