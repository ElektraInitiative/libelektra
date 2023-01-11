- infos = Information about glob plugin is in keys below
- infos/author = Felix Berlakovich <elektra@berlakovich.net>
- infos/licence = BSD
- infos/provides = apply
- infos/needs =
- infos/recommends =
- infos/ordering = check keytometa
- infos/stacking = no
- infos/placements = presetstorage postgetstorage
- infos/status = tested/unit nodep configurable experimental
- infos/description = copies metadata to keys with the help of globbing

## Introduction

The glob plugin provides coping metadata given by the plugin's configuration
to keys identified using _glob expressions_.
Globbing resembles regular expressions.
They do not have the same expressive power, but are easier to use.
The semantics are more suitable to match path names:

- `*` matches any key name of just one hierarchy. This means it
  complies with any character except slash or null.
- `?` satisfies single characters with the same exclusions.
- Additionally, there are ranges and character classes. They can also be inverted.

So this plugin adds metadata to keys identified by globbing expressions.
The plugin copies the metadata of the corresponding globbing keys in its configuration.
Globbing can be applied in get and set direction or both.

## Globbing Keys

The plugin is configured with globbing keys in its configuration. Each key below the configuration is
interpreted as a globbing key. The value of the key contains the globbing expression. When a key matching
the glob expression contained in one of the globbing keys is found, the metakeys of the corresponding
globbing key are copied. Once a match is found, no further keys will be considered for globbing. The reason
for this are catch all globbing keys that can be used to match all keys that have not been matched by a
preceding globbing key.

### Globbing Direction

Globbing keys located directly below the configuration (e.g `config/glob/#1`) are applied in both directions
(get and set). Keys below "get" (e.g. `config/glob/get/#1`) are applied only in the get direction and keys below set
(e.g. `config/glob/set/#1`) are applied only in the set direction.

So the glob plugin iterates over a list of glob expressions for every key.
Metadata is applied only for the first expression that matches.
So later expressions can be used as default values.

### Globbing Flags

Globbing keys may contain a subkey named "flags". This optional key contains the flags to be passed to the
globbing function (currently fnmatch) as a comma separated list. Unknown flag names will be ignored. The allowed flag names are

- "noescape" which enables the FNM_NOESCAPE flag
- "pathname" which enables the FNM_PATHNAME flag
- "period" which enables the FNM_PERIOD flag

If the flag key does not exist, FNM_PATHNAME is used as a default (see fnmatch(3) for more details).
An empty string disables all flags (i.e. also the default flag).

## Contracts

Glob statements are very useful together with contracts.
Storage plugins can request the glob plugin to fill up metadata before
they receive the keys in `elektraPluginSet()`.
In `config/needs`, the plugin declares which keys should obtain which
metadata.
If the glob expression starts
with a slash, the contract checker will automatically prepend the mount point.

For example, the hosts plugin contract contained:

```c
ksNew (30,
  // …
  keyNew ("system:/elektra/modules/hosts/config/needs/glob/#1",
      KEY_VALUE, "/*",
      KEY_META, "check/ipaddr", "", /* Preferred way to check */
          /* Can be checked additionally */
      KEY_META, "check/validation", "^[0-9.:]+$",
      KEY_META, "check/validation/message",
          "Character present not suitable for ip address",
      KEY_END),
  keyNew ("system:/elektra/modules/hosts/config/needs/glob/#2",
      KEY_VALUE, "/*/*",
          /* Strict character validation */
      KEY_META, "check/validation", "^[0-9a-zA-Z.:]+$",
      KEY_META, "check/validation/message",
          "Character present not suitable for host address",
      KEY_END),
  // …
);
```

We see that the `hosts` plugin added two glob statements with the clause
`config/needs`.
The first one matches with hostnames, the second with aliases.

The glob plugin only fills the metadata in `kdbSet()`.
This makes a difference compared with
adding the metadata already in `kdbGet()`.
Using the glob plugin, the user will not
see the metadata, but later plugins in `kdbSet()` will.

To sum up,
the glob plugin replenishes the keys with metadata.
The plugin applies metadata in a flexible way.
This metadata can be used for later checks.
Limited configuration storage plugins, like the `hosts`
plugin, use this feature.
They need it because they are not able to store metadata themselves.
It is obviously not possible to apply values to non-existing keys.
