# Header File Structure

## Problem

kdb.h contains the public API for both libelektra-core and libelektra-kdb.
It is confusing and makes it hard to see what library a function actually belongs to.

The big problem is kdbprivate.h. It has two main problems:

1. It contains stuff from many different libraries. I found at least libelektra-core, libelektra-kdb and libelektra-highlevel.
2. It contains things that are different levels of "private".

## Constraints

## Assumptions

## Considered Alternatives

## Decision

1.  Some parts are truly private, i.e. shouldn't be used outside the library that defines them. These things are only there because a library was split into multiple .c files. This includes e.g. splitNew and struct \_Split.
    Here it would be better to use a local header file in the library's source folder.
    Symbols belonging to this category should not appear at all in the symbols.map file.
2.  Other things truly private, but must be tested. This includes e.g. most other split* functions or the elektraKeyName* functions.
    Here a local header could be used as well (requires longer relative includes), or we could create a src/include/testing folder (could be added to include path).
    Symbols belonging to this category should only appear in the private section of the symbols.map file (we could also create a second private section, if is just there for testing, or adjust some compiler configs for testing).
3.  Then there are things that I would call internal or pseudo-private. This includes the struct \_Key and struct \_KeySet, but also keyClearSync or elektraMalloc and the high-level functions needed for codegen. These are things that shouldn't be part of the public API, because we don't want to make all the guarantees associated with that, but nonetheless cannot truly be private. In other words, each of these functions/structs/symbols has a specific "target audience" that may use it.
    Here I would either create a separate kdbinternal.h or just keep kdbprivate.h, as long as it is a separate file from 4.
    Symbols belonging to this category should only appear in the private section of the symbols.map file.
4.  Finally, there is what I'd call unstable or pseudo-public. This includes e.g. ksFindHierarchy or elektraReadArrayNumber. These functions could be public, but for various reasons are not. Maybe they are not well-tested or maybe we just don't want to commit to the function yet. Many of these (but not all) can't be part of libelektra-ease, because the rely on something more private (e.g. directly access the array in a KeySet).
    Here I would either create a separate kdbinternal.h or just keep kdbprivate.h, as long as it is a separate file from 3.
    Symbols belonging to this category should only appear in the private section of the symbols.map file.

## Rationale

## Implications

## Related Decisions

## Notes

Initial text copied from @kodebach https://github.com/ElektraInitiative/libelektra/issues/4219
