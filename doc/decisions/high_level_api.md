# High-level API

## Problem

Projects usually do not want to use low-level APIs.
`KDB` and `KeySet` is useful for plugins and to
implement APIs but not to be directly used in applications.

## Constraints

1. should be extremely easy to get started with
2. should be very hard to use it wrong
3. all high-level APIs should work together very nicely
   - same principles
   - same API style
   - same error handling
   - can be arbitrarily intermixed

## Assumptions

- Thread-safety: a handle is the accepted better solution than having to
  care about whether it is reentrant, thread-safe, ...
- assumes that spec is available (either by compiled-in `KeySet` or exit after elektraOpen)
- many projects do not care about some limitations (no binary, no metadata)
  but prefer a straightforward way to get/set config
- When people hit limitations they fall back to direct use of ^KeySet^, ^Key^

## Considered Alternatives

- storing errors in the handle:
 - Maintenance problem if error handling is added later
- only provide `KDB`, applications need to implement their own APIs:
 - reduces consistency of how the API is used
- only provide generated API
- assume type as `string` if not given
- force `default` to be part of parameters

## Decision

We provide 3 high-level C APIs:

1. libelektra-highlevel (generic key-value getter/setter)
2. libelektra-hierarchy (generic hierarchical getter/setter in a tree)
3. code generator (specified key-value getter/setter with function names,
   KeySets, or strings from specifications)

Furthermore, we will:

- have as goal that no errors in specified keys with default can occur
- if you use `elektraGetType` before getting a value, no error can occur when getting it later
- enforce that every key has a type
- use `elektraError` as extra parameter (for prototyping and examples you can pass 0)


### Basic

(needed for lcdproc)

```c
Elektra * elektraOpen (const char * application, const KeySet * defaultSpec, ElektraError ** error);
void elektraReload (Elektra * handle, ElektraError ** error);
void elektraParse (Elektra * handle, int argc, char ** argv, char ** environ); // pass environ?
void elektraDefault (Elektra * handle);
void elektraClose (Elektra * handle);
```

If `NULL` is passed as error, the API will try to continue if possible.
On fatal errors, i.e. on API misuse or when it is not safe to use the
handle afterwards, the API aborts with `exit`.

If you want to avoid `elektraOpen` or later `elektraReload` to fail, make
sure to use pass a `KeySet`, that has your whole specification included
(and defaults everywhere needed).

### Error Handling

```c
ElektraErrorCode elektraErrorCode (ElektraError * error);
const char * elektraErrorDescription (ElektraError * error);
ElektraErrorSeverity elektraErrorSeverity (ElektraError * error);
ElektraErrorGroup elektraErrorGroup (ElektraError * error);
ElektraErrorModule elektraErrorModule (ElektraError * error);

void elektraErrorFree (ElektraError * error);

```

`elektraErrorSeverity` tells you if you need to quit your application because of severe
issues that are permanent (severity == ELEKTRA_ERROR_SEVERITY_FATAL). Otherwise, developers can just print the message and
continue. (Default settings might be used then.)

### Simple Getters

(most of them needed for lcdproc)


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

### Arrays

(needed for lcdproc)


```c
size_t elektraArraySize (Elektra * handle, const char * name);
const char * elektraGetStringArrayElement (Elektra * elektra, const char * name, size_t index);
// same types as above

```

## Code-Generation

(needed for lcdproc)


For every entry in the specification, such as:

```
[key]
default=10
type=long
```

The code generator yields:

- `kdb_long_t elektraGetKey(Elektra * handle);`
- a `KeySet` to be used as defaultSpec
- `elektraOpenOrgApplication()`

All spec keys together are part of the KeySet that is automatically applied
on failures in lower-level calls when using:
`elektraOpenOrgApplication()` where `OrgApplication` is the org and application name.

## Enum

In the specification:

```
[server/serverScreen]
check/enum/#0=off
check/enum/#1=on
check/enum/#2=blank
```

The code generator emits:

```
typedef enum
{
	ELEKTRA_ENUM_SERVER_SERVERSCREEN_OFF = 0,
	ELEKTRA_ENUM_SERVER_SERVERSCREEN_ON = 1,
	ELEKTRA_ENUM_SERVER_SERVERSCREEN_BLANK = 2,
} ElektraEnumScreen;
```

Which can be used as:

```
ElektraEnumScreen elektraGetEnum(...);
```

```
[server]
define/type/serverscreenstatus
define/type/serverscreenstatus/check/enum/#0=off
define/type/serverscreenstatus/check/enum/#1=on
define/type/serverscreenstatus/check/enum/#2=blank
```

Then we can define:

```
[server/serverScreen]
type = serverscreenstatus
```

```
typedef enum
{
	ELEKTRA_ENUM_SERVER_SERVERSCREEN_OFF = 0,
	ELEKTRA_ENUM_SERVER_SERVERSCREEN_ON = 1,
	ELEKTRA_ENUM_SERVER_SERVERSCREEN_BLANK = 2,
} ElektraEnumScreenStatus;
```

## Extensions

(not needed for lcdproc)

```c
// gives you a duplicate for other threads (same application+version), automatically calls elektraErrorClear
Elektra * elektraDup (Elektra * handle);

enum ElektraType
{
	ELEKTRA_TYPE_BOOLEAN,
	ELEKTRA_TYPE_NONE,
	...
};

ElektraType elektraGetType (Elektra * handle);

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

(needed for lcdproc in a client)

A tree of `KeyHierarchy` elements, each element has a `Key` embedded.
Can be transformed from/to keysets.
With iterators to iterate over sub `KeyHierarchy` elements.

```c
keyhHasChildren (KeyHierarchy * kh);

// TODO, add rest of API
```

### todos

- const char * vs. string type?
- merging on conflicts? (maybe libelektra-tools dynamically loaded?)
- some API calls have a question mark next to it

## Rationale

1. Very easy to get started with, to get a key needs 3 lines of codes:

   ```c
   Elektra *handle = elektraOpen ("/sw/elektra/kdb/#0/current", 0);
   printf ("number /mykey is " ELEKTRA_LONG_F "\n", elektraGetLong (handle, "/mykey"));
   elektraClose (handle);
   ```

2. It is also easier to get started with writing new bindings.
3. User can combine the different APIs.

## Implications

## Related decisions

## Notes

https://issues.libelektra.org/1359
