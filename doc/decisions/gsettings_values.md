# GSettings values

## Issue

`GSettings` uses `GLibs` `GVariant` type for it values. The `GVariant` type can hold many
different variations of types but `Elektra` only distinguishes between strings
and binary data.

## Constraints

 - saved values have to be readable by `GSettings` but also trough `Elektra` itself
 - keep `Elektra` and `GLib` as independent as possible
 - values should be human readable
 - values should be rather self explaining about their type
 - saved values should be as portable as possible

## Assumptions

 - `Elektras` strength is its modularity to provide multiple backends that decide how to save values
 - applications using `GSettings` are well aware about the types they use
 - `GVariant` types can be represented as strings
 - `GVariant` types are rather self explaining about their type to some degree e.g. (255, 144) is a tuple of int
 - `GVariant` string representation can be parsed back into a `GVariant` reliable if we know their type
 - `GVariant` string representation can be parsed back into a `GVariant` unreliable on type guessing by parsing
 - `GVariant` can be serialized and deserialized, meaning it is possible to save all types binary
 - `Elektra` offers types trough templates in its c++ api

## Considered Alternatives

 - save serialized `GVariant` values as binarys
 - create a mapping between GVariants types and `spec` types and transform them when saving
 - use metadata to always exactly know the `GVariant` type

## Decision

 - use GVariants string representation
 - do not use GVariants additional type information in the string
 - do not use type mapping in the `GSettings` backend layer
 - keep usage of metadata as minimal as possible in favor of portability

## Argument

GVariants string representation is the most useful for all parts involved. It is immediately human readable and usable in code trough string compares.
If a party involved knows how GVariant represents a type and its format it can use its own parser to recreate the data.

GVariant offers to print additional type info included to the string but this defeats the purpose of having a
value that is easily human readable and the output seems to be only generated in special cases e.g. if an int is unsigned.

Serialized values would either create a dependency on `GVariants` deserialization or need a custom implementation in Elektra.
Without one of both they would not be human readable.

Direct mapping to `GVariant` would need `spec` to always be up to date and changes in types could break the mapping.
In addition it will be hard to match all possible `GVariant` types.

The idea is also to keep the `GSettings` backend layer as thin as possible and handle further problems in `Elektra` itself.

We could consider to save the GVariant type as metadata, but would prefer to keep metadata at a minimum.

## Implications

An application has to know what type it wants to save and what type it expects.
It only solves the problem how to handle direct writes and reads from GSettings.
It does not solve the problem of defaults and constrains when an application accesses GSettings values in Elekra.

## Related decisions

This influences the future follow up decision on how to handle defaults and constrains of GSettings in Elektra.
Decisions on how to handle specific types in Elektra will have influence on this decission (e.g. handling of boolean values in string form)

## Notes
