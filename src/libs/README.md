elektra-libs(7) -- libs overview
================================

## Libelektra

Since [0.8.15](/doc/decisions/library_split.md) [libelektra](/src/libs/elektra)
is split in following libraries:

![Overview of Libraries](/doc/images/overview_libs.png)


## Loader

[loader](/src/libs/loader) contains source files that implement the plugin
loader functionality. The files are linked to [libelektra](/src/libs/elektra).

## Libease

    libelektra-ease.so

[libease](/src/libs/ease) contains data-structure operations on top of libcore which do not depend on internals.
Applications and plugins can choose to not link against it if they want to stay minimal.

## Libplugin

    libelektra-plugin.so

[libplugin](/src/libs/plugin) contains elektraPlugin* symbols and plugins should link against it.

## Libproposal

    libelektra-proposal.so

[libproposal](/src/libs/proposal) contains functions that are proposed for libcore. Depends on internas of libcore and as
such must always fit to the exact same version.

## Libmeta

    libelektra-meta.so

[libmeta](/src/libs/meta/meta.c) contains meta data operations as described in [METADATA.ini](/doc/METADATA.ini).
Will be code-generated in the future, so methods should be mechanical reflections
of the contents in [METADATA.ini](/doc/METADATA.ini).

## Libcore

    libelektra-core.so
    <kdbhelper.h>
    <kdb.h> (key* and ks*)

Contains the fundamental data-structures every participant of Elektra needs
to link against. It should be the only part that access the internal
data structures.

## Libtools

[libtools](/src/libs/tools) is a high-level C++ shared-code for tools. It includes:

- plugin interface
- backend interface
- 3-way merge
