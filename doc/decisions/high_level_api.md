# High-level API

## Issue

Projects usually do not want to use low-level APIs,
`KDB`, however is untyped and not really high-level.

## Constraints

1. should be extremely easy to get started with
2. should be very hard to use it wrong
3. all 3 APIs should work together very nicely
   - same principles
   - same API style
   - same error handling
   - can be arbitrarily intermixed

## Assumptions

- Thread-safety: a handle is the accepted better solution than having to
  care about whether it is reentrant, thread-safe, ...
- assumes that spec is available (either by compiled-in `KeySet` or exit after elektraOpen)
- many projects do not care about some limitations (no binary, no meta-data)
  but prefer a straight-forward way to get/set config
- When people hit limitations they fall back to direct use of ^KeySet^, ^Key^

## Considered Alternatives

- simple vs. recursive API: recursive only for advanced users, thus it is enough if it is on top of KeySet
- only provide generated API

## Decision

We provide 3 C APIs:

1. libelektra-highlevel (generic key/value getter/setter)
2. libelektra-hierarchy (generic hierarchical getter/setter)
3. code generator (specified key/value getter/setter)


### Basic

(needed for lcdproc)

```c
Elektra * elektraOpen (const char * application);
kdb_boolean_t elektraHasError (const Elektra * handle);
const char * elektraErrorMessage (const Elektra * handle);
void elektraErrorClear (const Elektra * handle); // ErrorClear vs. ClearError?
void elektraClose (Elektra * handle);
```

`elektraOpen` might fail, you need to check for error afterwards!
If it fails, you need to exit the program with an error.
If you want to avoid to exit, make sure to use pass a built-in `KeySet`,
that has your whole specification included (and defaults everywhere
needed).


```c
// getters
const char * elektraGetString (Elektra * elektra, const char * name);
kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * name);
kdb_char_t elektraGetChar (Elektra * elektra, const char * name);
kdb_octet_t elektraGetOctet (Elektra * elektra, const char * name);
kdb_short_t elektraGetShort (Elektra * elektra, const char * name);
kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * name);
kdb_long_t elektraGetLong (Elektra * elektra, const char * name);
kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * name);
kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * name);
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * name);
kdb_float_t elektraGetFloat (Elektra * elektra, const char * name);
kdb_double_t elektraGetDouble (Elektra * elektra, const char * name);
kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * name);
```

### Reload and Parse

(needed for lcdproc)

```c
// might fail, you need to check for error afterwards!
void elektraReload (Elektra * handle);
void elektraParse (Elektra * handle, int argc, char ** argv, char ** environ); // pass environ?
void elektraDefault (Elektra * handle, const KeySet * defaultConfig);
```

### Arrays

(needed for lcdproc)


```c
size_t elektraArraySize (Elektra * handle, const char * name);
kdb_long_t elektraArrayLong (Elektra * handle, const char * name, size_t elem);
// same types as above

```

## Code-Generation

For every entry in the specification, such as:

```
[key]
default=10
type=long
```

The code generator yields a `kdb_long_t elektraGetKey(Elektra * handle);`.

All spec keys together are part of the KeySet that is automatically applied
on failures in lower-level calls when using:
`elektraOpenOrgApplication()` where `OrgApplication` is the org and application name.

## Extensions

(not needed for lcdproc)

```c
// gives you a duplicate for other threads (same application+version), automatically calls elektraErrorClear
Elektra * elektraDup (Elektra * handle);

KDB * elektraGetKDB (Elektra * handle);
KeySet * elektraGetKeySet (Elektra * handle, const char * cutkey);
KeySet * elektraGetKeyHierarchy (Elektra * handle, const char * cutkey);

// enum, int, tristate
void elektraSetInt (Elektra * handle, const char * name, int value);
```

### Lower-level type API

(not needed for lcdproc)

```c
// will be used internally in elektraGetInt, are for other APIs useful, too
int keyGetInt (Key * key);

// and so on
```

### recursive KeyHierarchy

(not needed for lcdproc)

can be transformed from/to keysets

```c
keyhAdd (KeyHierarchy * kh, Key * key);

// TODO, add rest of API
```

### todos

- const char * vs. string type?
- merging on conflicts? (maybe libelektra-tools dynamically loaded?)
- warnings
- default config directly in `elektraOpen`? Or in `elektraParse`?
- some API calls have a question mark next to it

## Argument

1. Very easy to get started with, to get a key needs 3 lines of codes:

   ```c
   Elektra *handle = elektraOpen ("/sw/elektra/kdb/#0/current");
   printf ("number /mykey is " ELEKTRA_LONG_F "\n", elektraGetLong (handle, "/mykey"));
   elektraClose (handle);
   ```

2. It is also easier to get started with writing new bindings.
3. User can combine the different APIs.

## Implications

## Related decisions

## Notes

https://issues.libelektra.org/1359
