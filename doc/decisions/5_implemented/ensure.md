# Ensure

## Problem

Applications want to ensure that some functionality (hooks) is present in Elektra.

## Constraints

## Assumptions

## Considered Alternatives

- Keep `kdbEnsure` (Rejected because it is too flexible and can be called many times.
  Furthermore, `kdbOpen` would build up configurations that get removed afterwards.)
  - reduce for only global plugins, as the partial other functionality is confusing and not needed
  - find solution that list plugin is not needed
  - only as implementation detail below libraries (e.g. like done for notification)
- Specific APIs per plugin, Rejected because:
  - difficult for application developers
  - every plugin would need to design new APIs
- Have a new API: `KDB * kdbOpen (Key * parent);`
  `int kdbConfigure (KDB * handle, KeySet * contract, Key * parentKey);`
  `KDB * kdbOpenDefault (Key * parent);`
  The new kdbOpen only does the absolute minimum work, in particular it doesn't set up any global plugins.
  If you use kdbOpen you must call kdbConfigure otherwise kdbGet will fail. kdbConfigure configures global plugins (basically just a renamed kdbEnsure).
  Lastly, kdbOpenDefault does more or less what the old kdbOpen does.
  It sets up the default case and you can call kdbGet immediately.
  But you cannot call kdbConfigure after kdbOpenDefault.
  Rejected because of API bloat and introduction of further state in `kdb`.

## Decision

Integrate `kdbEnsure` in `kdbOpen(Key *errorKey, KeySet *contract)` but only allow hooks.

## Rationale

- can immediately build up correct plugin positioning
- does not allow starting applications if the contract cannot be fulfilled
- simplest and minimalistic solution

## Implications

`elektraNotificationOpen` will be renamed and only return a contract KeySet:

```c
KeySet * errorKey = keyNew ("/", KEY_END);
KeySet * contract = ksNew (0, KS_END);
elektraNotificationContract (contract, iobinding, errorKey);
```

The same for gopts:

```c
elektraGOptsContract (contract, argc, argv, environ, errorKey);
```

Finally, we create `KDB` with the contracts we got before:

```c
KDB * kdb = kdbOpen (contract, parentKey);
```

Opening `KDB` will fail if any of the contracts cannot be ensured.

As the `contract` gets copied, at any point after `kdbOpen` the contract can be safely deleted:

```
ksDel (contract);
```

The cleanup of the global plugins happens within:

```c
kdbClose (kdb, errorKey);
```

It is safe to use the contract `KeySet` also for `kdbGet` and `kdbSet` invocations.
Contract `KeySet`s only contain `Key`s below `system:/elektra/contract`.
Therefore, normal `KeySet`s should not interfere.

## Related Decisions

- [Hooks](../4_partially_implemented/hooks.md)
- [Notifications](../0_drafts/notifications.md)

## Notes

- Issue [#2764](https://issues.libelektra.org/2764)
- Implemented in [#3651](https://pull.libelektra.org/3651)
