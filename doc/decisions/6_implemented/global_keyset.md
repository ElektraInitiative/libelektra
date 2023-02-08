# Global KeySet

## Problem

Some plugins need to communicate more data than is possible to do with metadata.
This can limit the functionality of plugins, which need to exchange binary or very complex data.

## Constraints

## Assumptions

## Considered Alternatives

## Decision

To make the communication between plugins easier, plugins will additionally get a handle to a global keyset via `elektraPluginGetGlobalKeySet()`.
The global keyset is tied to a KDB handle, initialized on `kdbOpen()` and deleted on `kdbClose()`.

The global keyset handle is initialized and accessible for all plugins except manually created plugins (by calling e.g. `elektraPluginOpen()`).

This decision removes the need to exchange information between plugins via the parentKey.

## Rationale

The need for a global keyset arose when developing a global cache plugin.
A global cache plugin needs to store internal and binary information for the KDB, which is simply not possible with metadata.

## Implications

Plugins are responsible for cleaning up their part of the global keyset.

## Related Decisions

## Notes
