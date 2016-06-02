# GSettings values

## Issue

`GSettings` uses `Glibs` `GVariant` type for it values. The `GVariant` type can hold many
different variations of types but `Elektra` only distinguishes between strings
and binary data.

## Constraints

 - saved values have to be readable by `GSettings` but also trough `Elektra` itself
 - keep `Elektra` as `Glib` independent as possible
 - values should be human readable

## Assumptions

 - `Elektras` strength is its modularity to provide multiple backends that decide how to save values
 - `GVariants` can be printed as Strings and parsed back again if we know the type
 - `GVariants` can be also serialized and deserialized
 - representing `GVariants` values trough strings fulfills all three constraints

## Considered Alternatives

 - save serialized values as binarys,
 - create a mapping between GVariants types and `spec` types and transform them when saving

## Decision

 - Use GVariants string representation

## Argument

GVariants string representationat is the most useful for all parts involved. It is immediately human readable and usable in code trough string compares.
If a party involved knows how GVariant represents a type it can use its own parser to recreate a data type from it.
Serialized values would create a dependency on `GVariants` deserialization or would need a custom implementation, without they would not be human readable.
Mapping too `GVariant` would need spec to always be up to date and changes in types could break the mapping.
In addition it will be hard to match all `GVariant` types.

## Implications

It only solves the problem how to handle direct writes and reads from GSettings.
It does not solve the problem of defaults and constrains when an application accesses GSettings values in Elekra.

## Related decisions

This influences the future follow up decision on how to handle defaults and constrains of GSettings in Elektra.

## Notes

We could consider to save the GVariant type as metadata, but would prefer to keep metadata at a minimum.
GVariant offers to print additional type info included to the string but this defeats the purpose of having a
value that is easily human readable and the output seems to be only generated in special cases e.g. if an int is unsigned
