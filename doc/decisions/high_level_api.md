# High Level API #

## Issue ##

Some projects do not want to use code-generation but prefer
an old-school key/value getter approach.

## Constraints ##

- very easy to get started with
- hard to use it wrong

Limitations:

- should not compete with code generation

## Assumptions ##

- Thread-safety: a handle is the accepted better solution than having to
  care about reentry, thread-safety,..
- assumes that spec is installed correctly (fail in kdbhlOpen otherwise)
  and no lookup for non-specified keys is done
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


### recursive API (KeyHierarchy) ###

can be transformed from/to keysets

keyhAdd (KeyHierarchy * kh, Key * key);


### todos ###

What is not so nice:

- error handling can be forgotten
- merging? (maybe dynamically loaded?)
- warning/error handling?

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
