# High Level API

## Issue

Some projects do not want to use code-generation but prefer
an "old-school" key/value getter/setter approach.

Here we propose new libraries (libelektra-highlevel and libelektra-hierarchy)
with new high-level APIs in C.

## Constraints

1. should be extremely easy to get started with
2. should be very hard to use it wrong

Limitations:

- cannot compete with code generation

## Assumptions

- Thread-safety: a handle is the accepted better solution than having to
  care about whether it is reentrant, thread-safe, ...
- assumes that spec is available and installed correctly (fail in elektraOpen otherwise)
- lookups for non-specified keys yield errors (in particular if they are not present)
- many projects do not care about some limitations (no binary, no meta-data)
  but prefer a straight-forward way to get/set config
- When people hit limitations they fall back to ^KeySet^, ^Key^

## Considered Alternatives

- simple vs. recursive API: recursive only for advanced users, thus it is enough if it is on top of KeySet
- only provide generated API

## Decision

Provide a simple getter/setter API.

### API

First draft of API:

#### Basic

```c
// might fail, you need to check for error afterwards!
Elektra * elektraOpen (const char * application);

// getters
kdb_string_t elektraGetString (KDBHL * elektra, const char * name);
kdb_boolean_t elektraGetBoolean (KDBHL * elektra, const char * name);
kdb_char_t elektraGetChar (KDBHL * elektra, const char * name);
kdb_octet_t elektraGetOctet (KDBHL * elektra, const char * name);
kdb_short_t elektraGetShort (KDBHL * elektra, const char * name);
kdb_unsigned_short_t elektraGetUnsignedShort (KDBHL * elektra, const char * name);
kdb_long_t elektraGetLong (KDBHL * elektra, const char * name);
kdb_unsigned_long_t elektraGetUnsignedLong (KDBHL * elektra, const char * name);
kdb_long_long_t elektraGetLongLong (KDBHL * elektra, const char * name);
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (KDBHL * elektra, const char * name);
kdb_float_t elektraGetFloat (KDBHL * elektra, const char * name);
kdb_double_t elektraGetDouble (KDBHL * elektra, const char * name);
kdb_long_double_t elektraGetLongDouble (KDBHL * elektra, const char * name);

// arrays
kdb_long_t elektraGetArrayLong (Elektra * handle, const char * name, size_t elem);

size_t elektraGetArraySize (Elektra * handle, const char * name);

void elektraClose (Elektra * handle);
```

#### Needed

```c
// might fail, you need to check for error afterwards!
void elektraReload (Elektra * handle);

// to abort afterwards
int elektraHasError (Elektra * handle);
char * elektraGetErrorMessage (Elektra * handle);

// to inform the user (e.g. warning, to display help or version)
int elektraHasInfo (Elektra * handle);
char * elektraGetInfoMessage (Elektra * handle);

// clear error+info
void elektraClear (Elektra * handle);
```

#### To think about

```c
// maybe not needed: could be integrated in elektraOpen?
void elektraParse (Elektra * handle, int argc, char ** argv, char ** environ);

// gives you a duplicate for other threads (same application+version), automatically calls elektraClear
Elektra * elektraDup (Elektra * handle);

KDB * elektraGetKDB (Elektra * handle);

void elektraDefaultConfig (Elektra * handle, KeySet * defaultConfig);

KeySet * elektraGetKeySet (Elektra * handle, const char * cutkey);

KeySet * elektraGetKeyHierarchy (Elektra * handle, const char * cutkey);

// enum, int, tristate
void elektraSetInt (Elektra * handle, const char * name, int value);
```

#### Lower-level type API

```c
// will be used internally in elektraGetInt, are for other APIs useful, too
int keyGetInt (Key * key);

// and so on
```

#### recursive API (KeyHierarchy)

can be transformed from/to keysets

```c
keyhAdd (KeyHierarchy * kh, Key * key);

// TODO, add rest of API
```

#### todos

What is not so nice:

- error handling can be forgotten
- merging on conflicts? (maybe libelektra-tools dynamically loaded?)
- warning/error handling?
- should recursive + lower-level type API in a separate library, in ease, or in core?

## Argument

1. Very easy to get started with, to get a key needs 3 lines of codes:

   ```c
   Elektra *handle = elektraOpen ("/sw/elektra/kdb/#0/current");
   printf ("number /mykey is %d\n", elektraGetInt (handle, "/mykey"));
   elektraClose (handle);
   ```

2. It is also easier to get started with writing new bindings.

## Implications

Wrong API use possible (it is not generated after all):

- wrong names, unspecified names,...
- application can be wrong

## Related decisions

## Notes
