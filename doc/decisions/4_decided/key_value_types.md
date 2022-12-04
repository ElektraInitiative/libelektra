# `Key` value types

## Problem

A `Key` has a value, that much is clear.

But should we differentiate between different types of values?

## Constraints

## Assumptions

- `libelektra-core` treats every type of value the same.
  Everything is stored as an allocated buffer of a known-length containing an arbitrary byte-sequence.

## Considered Alternatives

### No types

The simplest option: `libelektra-core` will not support any types.
Values are completely opaque and can only be correctly interpreted by the caller with the correct external knowledge.

### C-String vs. binary

Because string values are very common and, especially when `Key`s come from configuration files, most `Key`s will have a string values, we differentiate between strings and anything else.
Specifically, we define a string as a byte-sequence that does not contain a `\0`-byte.

Anything that is not a string will be treated as opaque.
A `Key` knows whether its value is a string.
Therefore, we can safely access and manipulate the value of such `Key`s without any external knowledge.

The actual implementation would tag non-string value instead of strings, because we except strings to be the more common case.

### UTF-8 vs. binary

Same as above, but here a string is defined as a byte-sequence that does not contain a `\0`-byte **and** forms valid UTF-8 data.

By enforcing UTF-8 this is more compatible with the notion of strings that other languages have.
Most languages define a string as some for of Unicode encoding (likely UTF-8 or UTF-16).
That means bindings can safely convert the UTF-8 to other formats.
Similarly, many storage formats mandate UTF-8 for text data.

### Full type system

Introduce a full type system in `libelektra-core` that supports at least the types the `type` plugin knows.

This would introduce more type-safety and would mean callers need even less external knowledge to handle values properly.

However, this complicates the implementation of `libelektra-core` and also complicates the code for any caller that does not care about the actual type of a `Key`'s value.

Storing integers directly instead of boxing them in a buffer, may allow memory savings, but it is another complication and because strings and other byte-sequences are still allowed, we still need all the buffer machinery anyway.

## Decision

Go with "No types", but provide functions for handling types in libraries other than `libelektra-core`.

## Rationale

From `libelektra-core`'s perspective there is no need to complicate things with different types of values.
Internally, everything will just be treated as an allocated byte-sequence with a known length anyway.

The "Full type system" may have some advantages, but it is too complicated to be part of `libelektra-core`.
Where appropriate integers and other values can still be stored directly as binary data instead of converting them to ASCII.
Without the type system in `libelektra-core` we cannot fully avoid boxing, but there can still be some generic optimizations that may help to store short byte-sequences more efficiently.

By not caring about any types, we also don't need flags or knowledge about special metadata (e.g., `meta:/binary`) in `libelektra-core`.

## Implications

- This decision affects the [`Key` value API](../0_drafts/key_value.md) decision.

## Related Decisions

- [`Key` value API](../0_drafts/key_value.md)

## Notes
