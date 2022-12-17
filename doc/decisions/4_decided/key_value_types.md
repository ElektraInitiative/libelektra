# `Key` value types

## Problem

A `Key` has a value, that much is clear.

But should we differentiate between different types of values?

## Constraints

## Assumptions

## Considered Alternatives

### No types

The simplest option: `libelektra-core` will not support any types.
Values are completely opaque and can only be correctly interpreted by the caller with the correct external knowledge.

### C-String vs. binary

Because string values are very common and, especially when `Key`s come from configuration files, most `Key`s will have string values, we differentiate between strings and anything else.
Specifically, we define a string as a byte-sequence that does not contain `\0` bytes, except a single one as the last character to mark the end of the string.

Anything that is not a string will be treated as opaque.
A `Key` knows whether its value is a string.
Therefore, we can safely access and manipulate the value of such `Key`s without any external knowledge.

The actual implementation would tag non-string values instead of strings, because we except strings to be the more common case.

### UTF-8 vs. binary

Same as above, but here a string is defined as a byte-sequence that does not contain a `\0` byte (but only has a single `\0` byte that marks the end of the string) **and** forms valid UTF-8 data.

By enforcing UTF-8 this is more compatible with the notion of strings that other languages have.
Most languages define a string as some form of Unicode encoding (likely UTF-8 or UTF-16).
That means bindings can safely convert the UTF-8 to other formats.
Similarly, many storage formats mandate UTF-8 for text data.

### Full type system

Introduce a full type system in `libelektra-core` that supports at least the types the `type` plugin knows.

This would introduce more type-safety and would mean callers need even less external knowledge to handle values properly.

However, this complicates:

- the implementation of `libelektra-core` .
- the code for any caller that does not care about the actual type of a `Key`'s value.
- writing of bindings, which somehow would need to map all Elektra's types to their type system.
- introduction of alternative type systems.

Storing integers directly instead of boxing them in a buffer, may allow memory savings, but it is another complication and because strings and other byte-sequences are still allowed, we still need all the buffer machinery anyway.

## Decision

Go with "No types" in `libelektra-core`, but provide functions for handling types in an extra library.

> **Note**: This also means that `libelektra-core` does not handle things like `meta:/binary`.
> This should instead be handled via the extra library, the `type` plugin, or replaced entirely with a new solution (e.g., assume binary unless `meta:/type = string`).

## Rationale

From `libelektra-core`'s perspective there is no need to complicate things with different types of values.
Internally, everything will just be treated as an allocated byte-sequence with a known length anyway.

The "Full type system" may have some advantages, but it is too complicated to be part of `libelektra-core`.
Where appropriate integers and other values can still be stored directly as binary data instead of converting them to ASCII.
Without the type system in `libelektra-core` we cannot fully avoid boxing, but there can still be some generic optimizations that may help to store short byte-sequences more efficiently.

By not caring about any types, we also don't need flags or knowledge about special metadata (e.g., `meta:/binary`) in `libelektra-core`.

## Implications

This decision has no direct implications on API.
However, it affects the [`Key` value API](../0_drafts/key_value.md) decision, in that is limits the API to use type-less, opaque byte-sequences.
Any implications on the API itself, will be decided and recorded in ["`Key` value API"](../0_drafts/key_value.md).

## Related Decisions

- [`Key` value API](../0_drafts/key_value.md)

## Notes
