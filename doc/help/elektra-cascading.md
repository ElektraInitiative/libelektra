# elektra-cascading(7) -- of key names

**Cascading** is the triggering of secondary actions.
For configuration it means that first the user configuration is read
and if this attempt fails, the system configuration is used as fallback.

The idea is that the application installs a configuration storage
with default settings that can only be changed by the administrator.
But every user has the possibility to override parts of this _system
configuration_ regarding the user's needs in the _user configuration_.
To sum up, besides system configuration, users have their own key
databases that can override the settings according to their preferences.

Thus when a key starts with `/` such cascading will automatically
be performed.

## SPEC

Keys in `spec` allow us to specify which keys are read by the application,
which fallback it might have and which is the default value using
metadata. The implementation of these features happened in `ksLookup`.
When cascading keys (those starting with `/`) are used following features
are available (in the metadata of respective `spec`-keys):

- `override/#`: use these keys _in favor_ of the key itself (note that
  `#` is the syntax for arrays, e.g. `#0` for the first element,
  `#_10` for the 11th and so on)
- `namespace/#`: instead of using all namespaces in the predefined order,
  one can specify which namespaces should be searched in which order
- `fallback/#`: when no key was found in any of the (specified) namespaces
  the `fallback`-keys will be searched
- `default`: this value will be used if nothing else was found

They can be used like this:

```sh
kdb set /overrides/test "example override"
sudo kdb meta set spec:/test override/#0 /overrides/test
```

## CASCADING

When cascading keys (those starting with `/`) the lookup will work in the
following way (it can be debugged with `kdb get -v`):

1. In the `spec`-key the `override/#` keys will be considered.
2. If, in the `spec`-key, a `namespace/#` exist, those namespaces
   will be used.
3. Otherwise, all namespaces will be considered, see
   [here.](/doc/help/elektra-namespaces.md)
4. In the `spec`-key the `fallback/#` keys will be considered.
5. In the `spec`-key the `default` value will be returned.

See [application integration](/doc/tutorials/application-integration.md)
for how to use cascading names in the context of applications.

[Read more about namespaces.](/doc/help/elektra-namespaces.md)
