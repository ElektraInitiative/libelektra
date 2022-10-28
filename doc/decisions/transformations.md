# Transformations

## Problem

Certain plugins, e.g. `rename`, perform extensive transformations on keys and keysets within Elektra.
Those transformations include, but are not limited to:

- Changing the names of the keys inbetween `kdbGet` and `kdbSet` operations
- Changing values back and forth for normalization, e.g. `true` -> `1`, `1` -> `true`
- Adding and removing metadata (e.g. the `spec` plugin)

While these features are useful, they do create feature-interaction problems.
More specifically, problems have been observed in conjunction with the following (overlapping) types of plugins:

- notification plugins
- plugins that do change tracking

The problem, in general, can be described as: Which version of the KDB should be used for notifications/change tracking?

### Observed problems with changing key names

For example, the `rename` plugin supports different transformations for `get` and `set`.
The plugin is executed in the post-storage phase for the `get` operation and the pre-storage phase for the `set` operation.
We can instruct it to convert the keynames to lowercase in the `get` operation, and to UPPERCASE in the `set` operation.
This results in the keys being in UPPERCASE in the configuration files, but they are presented in lowercase to other plugins and applications using the Elektra API.

For example, here is a configuration file with a hypothetical format:

```
/DISPLAY/BRIGHTNESS = 100
/DISPLAY/COLOR = #FFFFFF
``` 

As can be seen, the keys are in UPPERCASE within the configuration file.
In Elektra keys are case-sensitive. 
As operations on keysets such as `ksLookup` operate after the post-storage phase, `kdb get /DISPLAY/BRIGHTNESS` will fail.
For Elektra, the key `/DISPLAY/BRIGHTNESS` does not exist, as the `rename` plugin transformed this into the lowercase `/display/brightness`.

This leads to problems with the notification plugins.
As notification plugins are executed after the post-storage phase of the `set` operation, they will receive a keyset with the already transformed keys.
In this example, the notification plugins will receive all-UPPERCASE keys, and send out notifications with those all-UPPERCASE keys. 
An application listening to those notifications will not be able to query Elektra for those keys, as for Elektra those UPPERCASE keys do not exist.

Apart from the problems with notifications, the way the `rename` plugin works also breaks change tracking in plugins like `dbus`.
This is because the `rename` plugin does the transformation by deleting the original key and adding a new key with the transformed name.
The `dbus` plugin implements change tracking by checking the 'key needs sync' flag instead of comparing the values.
As new keys by design have the 'key needs sync' flag set, the plugins that implement change tracking via the flag will always erroneously detect transformed keys as changed.

### Observed problems with normalization

TBD ... 

In theory the same issue with the 'key needs sync' flag based change tracking approach as above.

### Observed problems with adding and removing metadata

TBD ... 

In theory the same issue with the 'key needs sync' flag based change tracking approach as above.

## Constraints

1.
2.
3.

## Assumptions

1.
2.
3.

## Considered Alternatives

1.
2.
3.

## Decision

## Rationale

## Implications

-
-
-

## Related Decisions

- [Hooks](hooks.md)
- [Change Tracking](https://github.com/ElektraInitiative/libelektra/pull/4554)
- [Internal Cache](internal_cache.md)

## Notes

- [Issue #404](https://issues.libelektra.org/404) - dbus and rename plugin do not work together
- [Issue #955](https://issues.libelektra.ort/955) - dbus: non-UTF8 key names 
