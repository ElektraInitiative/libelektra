elektra-libs(7) -- libs overview
================================

## Libelektra

Since [0.8.15](/doc/decisions/library_split.md) libelektra is split in following libraries:

![Overview of Libraries](/doc/images/overview_libs.png)

### Libelektra

    libelektra.so

Libelektra is now only a stub for legacy applications. It basically only links all previous libraries
together. It should not be used for new applications or plugins.

### Libfull

    libelektra-full.so

Contains all sources of Elektra linked to together in one large library.
Useful if you do not want dynamically loaded plugins.
Should only used on embedded systems (where whole application stack is done by you) and for tests.

### Libstatic

    libelektra-static.so

Contains all sources of Elektra linked to together in one large library.
Useful if you need your application to be linked statically against Elektra.
Should only used on embedded systems (where whole application stack is done by you) and for tests.

### Libkdb

    libelektra-kdb.so

Contains kdb* symbols and applications should link against it.

### Libplugin

    libelektra-plugin.so

Contains elektraPlugin* symbols and plugins should link against it.

### Libease

    libelektra-ease.so

Contains data-structure operations on top of libcore which do not depend on internals.
Applications and plugins can choose to not link against it if they want to stay minimal.

### Libproposal

    libelektra-proposal.so

Contains functions that are proposed for libcore. Depends on internas of libcore and as
such must always fit to the exact same version.

### Libmeta

    libelektra-meta.so

Contains meta data operations as described in [METADATA.ini](/doc/METADATA.ini).
Will be code-generated in the future, so methods should be mechanical reflections
of the contents in [METADATA.ini](/doc/METADATA.ini).

### Libcore

    libelektra-core.so

Contains the fundamental data-structures every participant of Elektra needs
to link against.
