# New API by-value

### Note on structs

The public structs `ElektraName` and `ElektraValue` defined here are not problematic or limiting in terms of forward compatibility.
This is because, they essentially define what a "keyname" and a "key value" are for Elektra.
That is why they are used both as part of the public API **and** in the definitions of the `*Cow` structs, which define how stuff is actually stored.

To further stress this fact, the structs are only used by-value.
This means they are more or less syntactic sugar.
Semantically the struct acts as if the fields it contains were passed separately.
Therefore, many internal changes that would cause breaking changes in the ["by-reference" variant](../byref/README.md), can be masked.

For example, changing how the name is stored inside `ElektraEntry` is possible without a breaking change.
The API doesn't require an addressable instance of `ElektraName` which can be returned by `elektraEntryGetName()`.
The API only requires that an `ElektraName` can be constructed inside `elektraEntryGetName()`, which would then be returned.
Of course constructing such an instance, if it is not stored as-is internally, may cause performance regressions.
However, it would still be possible.
