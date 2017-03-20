# CHANGES

These document describes the Changes from Elektra 0.7 to 0.8

## API

It was taken care that there are as little as possible,
but three API bugs had to be fixed:

 - the ksNeedsSort() API is no longer needed
   (keyset is always sorted)

   ksSort() removed
   ksNeedSort() removed

 - the metadata which was filesys only is removed

 - KeyMeta: newly introduced API
   remove old keyGetATime, keyGetUID, ..
   (They are still implemented with new metadata interface in the
    testcases: tests/test_meta.c)

 - keyset now describes the desired state of configuration (no removed keys)

 - keyNext: wrong documentation (subsequent calls will still return NULL ptr)

 - kdbSet() now requires you to use kdbGet() before.
   This is needed because elektra now can detect multi-process
   conflicts. That means, if another process does a kdbSet() during you
   work with the keyset, you will get the error/number:   30 "found
   conflict".
   Normally you also get this error if you did not do any kdbGet() with
   the handle you try to kdbSet(), too.
   This topic is discussed in
   http://www.markus-raab.org/ftp/elektra/thesis.pdf

 - The keyset now takes ownership of a key when you use ksAppendKey.
   This means that you do not need to keyDel() every single key you
   appended, but ksDel() will delete all keys.
   Because of reference counting you can also pass a key to multiple
   keysets.


### C++ Interface:

- always compare names (not identity) operator== affected


## Build system

 - Changed from autotools to cmake.

 - No more dependency to libtool.

 - C++ is now a build dependency.
   Rationale: C++ is used by cmake too.
   Only the "dump" plugin needs to be rewritten in C to remove that dependency.

 - No LD_LIBRARY_PATH or ld.so.conf needed anymore.
   Instead RPATH is used.

 - New build variant: libelektra-full
   It is a dynamic library with everything statically included.

## Languages

 -  Core is now Ansi C 99.

 -  C++ is now officially supported by plugins.
    Note that C++ interface is not yet API/ABI compatible.
