# High Level API #

## Issue ##

Some projects do not want to use code-generation but prefer
an "old-school" key/value getter/setter approach.

Here we propose new libraries (libelektra-highlevel and libelektra-hierarchy)
with new high-level APIs in C.

## Constraints ##

1. should be extremely easy to get started with
2. should be very hard to use it wrong

Limitations:

- cannot compete with code generation

## Assumptions ##

- Thread-safety: a handle is the accepted better solution than having to
  care about whether it is reentrant, thread-safe, ...
- assumes that spec is available and installed correctly (fail in kdbhlOpen otherwise)
- lookups for non-specified keys yield errors (in particular if they are not present)
- many projects do not care about some limitations (no binary, no meta-data)
  but prefer a straight-forward way to get/set config
- When people hit limitations they fall back to ^KeySet^, ^Key^

## Considered Alternatives ##

- simple vs. recursive API: recursive only for advanced users, thus it is enough if it is on top of KeySet
- only provide generated API

## Decision ##

Provide a simple getter/setter API.

### API ###

First draft of API:

#### Basic ####

```c
// might fail, you need to check for error afterwards!
KDBHL * kdbhlOpen (const char * application);

// enum, int, tristate
int kdbhlGetInt (KDBHL * handle, const char * name);

char * kdbhlGetString (KDBHL * handle, const char * name);

// and so on.. (all types)


// are arrays already advanced functionality? (recursive API)

// enum, int, tristate
int kdbhlGetIntArray (KDBHL * handle, const char * name, int elem);

int kdbhlGetArraySize (KDBHL * handle, const char * name);

void kdbhlClose (KDBHL * handle);
```

#### Needed ####

```c
// might fail, you need to check for error afterwards!
void kdbhlReload (KDBHL * handle);

// to abort afterwards
int kdbhlHasError (KDBHL * handle);
char * kdbhlGetErrorMessage (KDBHL * handle);

// to inform the user (e.g. warning, to display help or version)
int kdbhlHasInfo (KDBHL * handle);
char * kdbhlGetInfoMessage (KDBHL * handle);

// clear error+info
void kdbhlClear (KDBHL * handle);
```

#### To think about ####

```c
// maybe not needed: could be integrated in kdbhlOpen?
void kdbhlParse (KDBHL * handle, int argc, char ** argv, char ** environ);

// gives you a duplicate for other threads (same application+version), automatically calls kdbhlClear
KDBHL * kdbhlDup (KDBHL * handle);

KDB * kdbhlGetKDB (KDBHL * handle);

void kdbhlDefaultConfig (KDBHL * handle, KeySet * defaultConfig);

KeySet * kdbhlGetKeySet (KDBHL * handle, const char * cutkey);

KeySet * kdbhlGetKeyHierarchy (KDBHL * handle, const char * cutkey);

// enum, int, tristate
void kdbhlSetInt (KDBHL * handle, const char * name, int value);
```

#### Lower-level type API ####

```c
// will be used internally in kdbhlGetInt, are for other APIs useful, too
int keyGetInt (Key * key);

// and so on
```

#### recursive API (KeyHierarchy) ####

can be transformed from/to keysets

```c
keyhAdd (KeyHierarchy * kh, Key * key);

// TODO, add rest of API
```

#### todos ####

What is not so nice:

- error handling can be forgotten
- merging on conflicts? (maybe libelektra-tools dynamically loaded?)
- warning/error handling?
- should recursive + lower-level type API in a separate library, in ease, or in core?

## Argument ##

1. Very easy to get started with, to get a key needs 3 lines of codes:
   
```c
KDBHL *handle = kdbhlOpen ("/sw/elektra/kdb/#0/current");
printf ("number /mykey is %d\n", kdbhlGetInt (handle, "/mykey"));
kdbhlClose (handle);
```
   
2. It is also easier to get started with writing new bindings.

## Implications ##

Wrong API use possible (it is not generated after all):

- wrong names, unspecified names,...
- application can be wrong

## Related decisions ##

## Notes ##
