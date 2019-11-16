# elektra-spec(7) -- spec namespace

## INTRODUCTION

spec is a special namespace that describes via metadata the
semantics of individual keys.

Most importantly it:

1. describes which keys are of interest to the application
2. describes the metadata to be copied to every key
3. describes how the cascading lookup works
4. describes the mount points including the plugins needed for them

It is, however, not limited to this but can express any other
key database semantics (new plugins might be necessary, though).

## Application

The most simple use is to enlist all keys that will be used by an application
and maybe give a description for them (we use ini syntax in this document):

```ini
[mykey]

[folder/anotherkey]
description = set this key if you want another behavior
```

So Keys in `spec` allow us to specify which keys are read by the application.
The description key will be copied (referred to) to `folder/anotherkey` of
any namespace, so that it can easily be accessed.

## Cascading Lookup

Other features are directly implemented in `ksLookup`.
When cascading keys (those starting with `/`) are used following features
are now available (in the metadata of respective `spec`-keys):

- `override/#`: use these keys _in favor_ of the key itself (note that
  `#` is the syntax for arrays, e.g. `#0` for the first element,
  `#_10` for the 11th and so on)
- `namespace/#`: instead of using all namespaces in the predefined order,
  one can specify which namespaces should be searched in which order
- `fallback/#`: when no key was found in any of the (specified) namespaces
  the `fallback`-keys will be searched
- `default`: this value will be used if nothing else was found

E.g.

```ini
[promise]
default=20
fallback/#0=/somewhere/else
namespace/#0=user
```

1. When this file is mounted to `spec:/sw/app/#0` we specify, that
   for the key `/sw/app/#0/promise` only the namespace `user` should be
   used.
2. If this key was not found, but `/somewhere/else` is present, we will use
   this key instead. The `fallback` technique is very powerful: it allows
   us to have (recursive) links between applications. In the example above,
   the application is tricked in receiving e.g. the key `user:/somewhere/else`
   when `promise` was not available.
3. The value `20` will be used as default, even if no configuration file
   is found.

Note that the fallback, override and cascading works on _key level_,
and not like most other systems have implemented, on
configuration _file level_.

## Validation

You can tag any key using the `check` metadata so that it will be validated.

For example:

```ini
[folder/anotherkey]
check/validation = abc.*
check/validation/message = def does not start with abc
```

## Mounting

In the spec namespace you can also specify mount points.
First you need the metakey `mountpoint` and a configuration file name.
Otherwise, it basically works in the same way as the contracts
in plugins using `infos` and `config`:

```ini
[]
mountpoint=file.abc
config/plugin/code/escape = 40
config/plugin/lua#abc/script = abc_storage.lua
infos/author = Markus Raab
infos/needs = resolver_abc rename code lua#abc
infos/recommends = hexcode
```

## SEE ALSO

- [see application integration tutorial (towards end)](/doc/tutorials/application-integration.md)
- [see namespaces tutorial](/doc/tutorials/namespaces.md)

- [elektra-namespaces(7)](elektra-namespaces.md)
- [elektra-cascading(7)](elektra-cascading.md)
