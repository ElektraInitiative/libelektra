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
  care about whether it is reentrant, thread-safe,..
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

// basic stuff
KDBHL * kdbhlOpen (const char * application); // might fail, you need to check for error afterwards!

int kdbhlGetInt (KDBHL * handle, const char * name); // enum, int, tristate
char * kdbhlGetString (KDBHL * handle, const char * name);
// and so on.. (all types)


// are arrays already advanced functionality? (recursive API)
int kdbhlGetIntArray (KDBHL * handle, const char * name, int elem); // enum, int, tristate
int kdbhlGetArraySize (KDBHL * handle, const char * name);

void kdbhlClose (KDBHL * handle);

// needed stuff
void kdbhlReload (KDBHL * handle); // might fail, you need to check for error afterwards!

int kdbhlHasError (KDBHL * handle); // to abort afterwards
char * kdbhlGetErrorMessage (KDBHL * handle);

int kdbhlHasInfo (KDBHL * handle); // to inform the user (e.g. warning, to display help or version)
char * kdbhlGetInfoMessage (KDBHL * handle);

void kdbhlClear (KDBHL * handle); // clear error+info

### to think about ###

void kdbhlParse (KDBHL * handle, int argc, char ** argv, char ** environ); // maybe not needed: could be integrated in kdbhlOpen?
KDBHL * kdbhlDup (KDBHL * handle); // gives you a duplicate for other threads (same application+version), automatically calls kdbhlClear
KDB * kdbhlGetKDB (KDBHL * handle);
void kdbhlDefaultConfig (KDBHL * handle, KeySet * defaultConfig);
KeySet * kdbhlGetKeySet (KDBHL * handle, const char * cutkey);
KeySet * kdbhlGetKeyHierarchy (KDBHL * handle, const char * cutkey);
void kdbhlSetInt (KDBHL * handle, const char * name, int value); // enum, int, tristate

### Lower-level type API ###

int keyGetInt (Key * key); // will be used internally in kdbhlGetInt, are for other APIs useful, too
// and so on


### recursive API (KeyHierarchy) ###

can be transformed from/to keysets

keyhAdd (KeyHierarchy * kh, Key * key);

// TODO, add rest of API


### todos ###

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
