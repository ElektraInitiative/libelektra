This folder contains the core libraries of Elektra.

> **Note:** Some information in this document is outdated and will change before the release of Elektra 1.0.

# Content

Since 0.8.15 this folder
contains multiple libraries:

![Overview of Libraries](/doc/images/overview_libs.png)

## Libelektra

```
libelektra.so
```

Libelektra is now only a stub for legacy applications. It basically only links all previous libraries
together. It should _not_ be used for new applications or plugins.

## Libfull

```
libelektra-full.so
```

Contains all sources of Elektra linked to together in one large library.
Useful if you do not want dynamically loaded plugins.
Should only be used on embedded systems (where whole application stack is done by you) and for tests.

## Libstatic

```
libelektra-static.so
```

Contains all sources of Elektra linked to together in one large library.
Useful if you need your application to be linked statically against Elektra.
Should only be used on embedded systems (where whole application stack is done by you) and for tests.

## Libkdb

```
libelektra-kdb.so
<elektra/kdb.h> (kdb*)
```

Contains `kdb*` symbols and applications should link against it.
