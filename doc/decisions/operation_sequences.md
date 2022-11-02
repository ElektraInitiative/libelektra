# Operation sequences

## Problem

The current public API allows various kind of sequences of `kdbGet` and `kdbSet` calls.
There is no real guideline of allowed and/or prohibited sequences, other than `kdbGet` for a certain parent key must be called before `kdbSet`:

- initially after `kdbOpen`
- after conflict errors in `kdbSet`

We discovered that some sequences are currently legal but problematic for change tracking and maybe also other use cases.

Currently, notification plugins assume that a `kdbSet` operation for a certain parent key always directly follows the `kdbGet` operation for that same key.
However, this is not enforced in any way within Elektra, nor is it documented that it should.

In applications, `kdbGet` and `kdbSet` of different parent keys might be interwoven in any way.
This is problematic, as plugins may store the result of `kdbGet` and use the stored data in the following `kdbSet` to calculate a changeset.
Plugins have to do this, as there isn't any other Elektra-provided mechanism currently to do change tracking for them.

Note, that erroneous behaviour only occurs if plugins are used globally, i.e. as `notification-send` hook plugins.
If they are mounted for specific backends (via `kdb mount`), this behaviour will not occur as plugins from different backends are isolated.

### Reproducible Example

The following example will demonstrate the problematic sequence as real, runnable code.
This example will show the erroneous behaviour for the `dbus` and `logchange` plugins, if used as `notification-send` hooks.
These both plugins just serve as a general demonstration, and there may be more plugins that do change tracking this way.

First, run some setup steps:

```sh
# enable the dbus plugin as a notification-send hook
kdb set system:/elektra/hook/notification/send/plugins/#0 dbus

# enable the logchange plugin as a notification-send hook
kdb set system:/elektra/hook/notification/send/plugins/#1 logchange

# mount user:/a and user:/b as two backends
kdb mount a.ecf user:/a
kdb mount b.ecf user:/b

# populate user:/a and user:/b with values
kdb set user:/a/brightness 1
kdb set user:/a/saturation 1
kdb set user:/b/foreground 1
kdb set user:/b/background 1
```

Then, run the following command to monitor the notifications by the `dbus` plugins:

```sh
dbus-monitor --session type='signal',interface='org.libelektra'
```

Finally, execute the following program:

```c
#include <kdb.h>
#include <stdio.h>

int main (void)
{
	Key * root = keyNew ("/", KEY_END);
	KDB * handle = kdbOpen (NULL, root);

	Key * keyA = keyNew ("user:/a", KEY_END);
	Key * keyB = keyNew ("user:/b", KEY_END);

	KeySet * a = ksNew (0, KS_END);
	KeySet * b = ksNew (0, KS_END);

	kdbGet (handle, a, keyA);  // (A)
	kdbGet (handle, b, keyB);  // (B)

	// we need to change *something*, otherwise the backend would assume nothing changed
	// and never even execute all of kdbSet
	ksAppendKey (a, keyNew ("user:/a/test", KEY_VALUE, "hi", KEY_END));

	kdbSet (handle, a, keyA);  // (C)

	ksDel (a);
	ksDel (b);
	keyDel (keyA);
	keyDel (keyB);
	keyDel (root);

	kdbClose (handle, 0);
	return 0;
}
```

- At point `(A)` the plugins internally store the data below the parent `keyA`.
- At point `(B)` the same thing happens for the data below the parent `keyB`.
  This **replaces** the data from `(A)`.
- At point `(C)` plugins that do change tracking need to generate a changeset.
  The changeset will be calculated between the stored data and the new data in the KeySet `a`.
  Because the stored data is now from `(B)` and **not** from `(A)`, this changeset will be wrong.
  The calculation will wrongly assume everything in KeySet `b` was removed and everything in KeySet `a` is newly added.

Right after `(B)`, the two keysets are filled with the following values:

| `a`                  | `b`                  |
| -------------------- | -------------------- |
| `user:/a/brightness` | `user:/b/background` |
| `user:/a/saturation` | `user:/b/foreground` |

If a plugin calculates a changeset during `kdbSet` by comparing to the `KeySet` from the last `kdbGet` (as described above), the plugin will wrongly calculate the following changeset at `(C)`:

| Added Keys           | Removed Keys         | Modified Keys |
|----------------------|----------------------| ------------- |
| `user:/a/brightness` | `user:/b/background` |               |
| `user:/a/saturation` | `user:/b/foreground` |               |
| `user:/a/test`       |                      |               |

This is obviously wrong. Looking at the example above, the changeset should only contain `user:/a/test`, as nothing else has been changed in keyset `a`.
So the correct changeset should look like:

| Added Keys     | Removed Keys | Modified Keys |
|----------------| ------------ | ------------- |
| `user:/a/test` |              |               |

The erroneous behaviour can be noticed by the output of `dbus-monitor`:

```
signal time=1666993066.749997 sender=:1.364 -> destination=(null destination) serial=2 path=/org/libelektra/configuration; interface=org.libelektra; member=KeyAdded
   string "user:/a/brightness"
signal time=1666993066.750161 sender=:1.364 -> destination=(null destination) serial=3 path=/org/libelektra/configuration; interface=org.libelektra; member=KeyAdded
   string "user:/a/saturation"
signal time=1666993066.750551 sender=:1.364 -> destination=(null destination) serial=4 path=/org/libelektra/configuration; interface=org.libelektra; member=KeyAdded
   string "user:/a/test"
signal time=1666993066.750628 sender=:1.364 -> destination=(null destination) serial=5 path=/org/libelektra/configuration; interface=org.libelektra; member=KeyDeleted
   string "user:/b/background"
signal time=1666993066.750866 sender=:1.364 -> destination=(null destination) serial=6 path=/org/libelektra/configuration; interface=org.libelektra; member=KeyDeleted
   string "user:/b/foreground"
```

The same behaviour is present in the `logchange` plugin. Notice its output onto `stdout`:

```
added key: user:/a/brightness
added key: user:/a/saturation
added key: user:/a/test
removed key: user:/b/background
removed key: user:/b/foreground
```

As can be seen, the change tracking within the `dbus` and `logchange` plugins wrongly calculates everything below `user:/a` are new keys that have been added, and everything below `user:/b` was removed.

## Constraints

1. As far as possible, we must check for illegal sequences and raise an error when an illegal sequence occurs

## Assumptions

1.
2.
3.

## Considered Alternatives

1. Document which sequences are allowed without raising errors in all illegal cases.
   Least-effort approach, but at least offers some transparency for developers.
   Developers utilizing Elektra can still ignore it, and may cause problems in certain setups without knowing.

   (Violates constraint 1)

2. Enforce that the `parentKey` used in `kdbSet` is below the one used in the last `kdbGet`.
   If this is not the case, `kdbSet` will abort and report an error in `parentKey`.
   Developers might still wrongly mix the sequences, but they will get an error and have to fix it.

   - Adding a `Key * lastGetParent` to `struct _KDB`
   - Doing `keyCopy (handle->lastGetParent, parentKey, KEY_CP_NAME)` in `kdbGet`
   - And checking `keyIsSameOrBelow (handle->lastGetParent, parentKey) == 1` in `kdbSet`

   ```c
   KDB * kdb = ...;
   
   Key * keyA = ...;
   Key * keyB = ...; 
   
   KeySet * a = ...;
   KeySet * b = ...;
   
   // These sequences will work fine
   assert (kdbGet (kdb, a, keyA) == 0);
   assert (kdbSet (kdb, a, keyA) == 0);
   assert (kdbGet (kdb, b, keyB) == 0);
   assert (kdbSet (kdb, b, keyB) == 0);
   
   // These will not
   assert (kdbGet (kdb, a, keyA) == 0);
   assert (kdbGet (kdb, b, keyB) == 0);
   assert (kdbSet (kdb, a, keyA) == -1); // Error! keyA is different from last used key in kdbGet (keyB)
   ```

3. Enforce that `ks` used in `kdbSet` is the same as in the last `kdbGet`
   Similar to (2), but we check for the pointer of the `KeySet` instead of having a copy of the parent key, which saves some memory.

   - Adding a `KeySet * lastGetKs` to `struct _KDB`
   - Doing `handle->lastGetKs = ks` in `kdbGet`
   - And checking `lastGetKs == ks` in `kdbSet`

4. Change the API so that a single instance of KDB can only contain a single instance of a `KeySet`.
   This solution completely eliminates problematic sequences.

   ```c
   // same as before but uses handle->data instead of the ks argument
   int kdbGet (KDB * handle, Key * parentKey);
   // same as before but uses handle->data instead of the ks argument
   int kdbSet (KDB * handle, Key * parentKey);

   KeySet * kdbData (KDB * handle) {
       return handle->data;
   }
   ```

5. Allow arbitrary sequences and let each plugin deal with it on a case-by-case basis.
   This alternative would put most of the burden onto the plugin authors.
   Depending on what the plugins do, every plugin may also need to deep-dup every keyset of every parent it ever receives via `kdbGet`.
   This will increase memory usage. 
   However, this could be paired with the [COW semantics](internal_cache.md) so the memory toll would not be that big of a deal.
   The biggest problem with this approach would be the unnecessary duplication of the non-trivial change tracking algorithm.

6. Don't restrict sequences further and provide a common framework to handle change tracking correctly.
   
   As the problem has only been observed with plugins doing their own change tracking, we could provide a general change tracking framework within Elektra.
   This way, we have only one such algorithm in a central place, and plugin authors don't have to think about the sequences their plugins are called by developers.
   
   This approach can also be paired with [COW semantics](internal_cache.md), so that memory toll will be kept low.
   A separate [decision for change tracking](change_tracking.md) is currently in progress.
   
   Should we observe this problem with use cases other than change tracking, we can provide general frameworks for those too.

## Decision

## Rationale

## Implications

-
-
-

## Related Decisions

- [Change Tracking](change_tracking.md)
- [Internal KeySet Cache](internal_cache.md)
- []()

## Notes

- Issue [#4514](https://issues.libelektra.org/4514) uncovered a problem in the current change tracking approach relating to sequences of `kdbGet` and `kdbSet`
- Issue [#4512](https://issues.libelektra.org/4520) explored some possible solutions
