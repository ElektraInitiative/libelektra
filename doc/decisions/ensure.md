# Ensure

## Problem

Applications want to ensure that some functionality (=global plugin)
is present in Elektra.

## Constraints

## Assumptions

## Considered Alternatives

- Keep `kdbEnsure` (Rejected because it is too flexible and can be called many times. Furthermore,
  `kdbOpen` would build up configurations that get removed afterwards.)
  - reduce for only global plugins, as the partial other functionality is confusing
    and not needed
  - find solution that list plugin is not needed
  - only as implementation detail below libraries (e.g. like done for notification)
- Specific APIs per plugin, Rejected because:
  - difficult for application developers
  - every plugin would need to design new APIs
- Have a new API: `KDB * kdbOpen (Key * parent);`
  `int kdbConfigure (KDB * handle, KeySet * contract, Key * parentKey);`
  `KDB * kdbOpenDefault (Key * parent);`
  The new kdbOpen only does the absolute minimum work, in particular it doesn't set up any global plugins.
  If you use kdbOpen you must call kdbConfigure otherwise kdbGet will fail. kdbConfigure configures global
  plugins (basically just a renamed kdbEnsure). Lastly, kdbOpenDefault does more or less what the old kdbOpen
  does. It sets up the default case and you can call kdbGet immediately. But you cannot call kdbConfigure after
  kdbOpenDefault.
  Rejected because of API bloat and introduction of further state in `kdb`.

## Decision

Integrate `kdbEnsure` in `kdbOpen(Key *errorKey, KeySet *contract)` but only allow global plugins.

## Rationale

- can immediately build up correct plugin positioning
- does not allow to start applications if the contract cannot be fulfilled
- simplest and minimalistic solution

## Implications

`elektraNotificationOpen` needs to be removed and instead we need a new API:

```c
KeySet * contract = ksNew (0, KS_END);
elektraNotificationGetContract (contract);
KDB * kdb = kdbOpen (key, contract); // contract contains notification config
elektraIoSetBinding (kdb, binding);
```

Similar contract getters need to be introduced for other functionality, like gopts:

```c
KeySet * contract = ksNew (0, KS_END);
elektraGOptsGetContract (contract, argc, argv, environ);
KDB * kdb = kdbOpen (key, contract);
```

The high-level API can make this more pretty, though.

## Related Decisions

- [Global Plugins](global_plugins.md)

## Notes
