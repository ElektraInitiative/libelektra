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

1. Provide a function `int genconf (KeySet * ks, Key * errorKey)` where `ks`
   is filled with a list of all variants with the essential configuration (subkeys `config`)
   and the changed parts of the contract (subkeys `infos`).
2. Keys defined in `system/elektra/plugins/<plugin>/variants/<variantname>` have the same content,
   but take precedence. If a variant with the same name is defined, only `config` or `infos`
   from `genconf` are considered if they are not mentioned in `system/elektra/plugins/variants`.
3. If the bool key `override` (for a plugin or a variant) is true, it will be overwritten (content
   of `genconf` ignored, but instead a plugin or variant as given is created).
4. If the bool key `disable` (for a plugin or a variant) is true the plugin or a variant of the
   plugin will not be available.
5. Empty `config` and `infos` mean:
 - `config`: The "default variant" (without any parameter) should be also available
   (has useful functionality)
 - `infos`: It is not possible to determine the changes of the contract,
   the user need to instantiate the plugin and enquiry the contract.

### Example

`genconf` for augeas yields:
```
system/access
system/access/config
system/access/config/lens = Access.lns
system/access/infos
system/access/infos/provides = storage/access
system/aliases
system/aliases/config
system/aliases/config/lens = Aliases.lns
system/aliases/infos
system/aliases/infos/provides = storage/aliases
```

`genconf` for python might yield:
```
user/configparser/config
user/configparser/config/script = python_configparser.py
```

The user/admin specifies:
```
system/elektra/plugins/augeas/variants/access/disable = 1
system/elektra/plugins/jni/disable = 1
system/elektra/plugins/python/variants/configparser/override = 1
system/elektra/plugins/python/variants/configparser/config/script = my_better_configparser.py
```

As result we get:

1. `access` variant of augeas is not available
2. `aliases` as defined from `genconf` (provides storage `aliases`)
3. `configparser` is completely redefined (result from `genconf` will not be considered)
   but it will be considered as specified.
4. the plugin `jni` will not be available


To have a space-separated simpleini one would use:
```
system/elektra/plugins/simpleini/variants/space
system/elektra/plugins/simpleini/variants/space/config
system/elektra/plugins/simpleini/variants/space/config/format = "% %"
```


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
  so that a user can copy and paste this for the `kdb mount` command.


## Related decisions

- [Global Plugins](global_plugins.md)


## Notes

Discussions took place [here](http://git.libelektra.org/issues/1006).
