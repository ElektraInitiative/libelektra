# Namespace and Name of Keys

## Problem

A `Key` in Elektra is identified by its name, which consists of a namespace and a number of name parts.
There are two common representations for the name: escaped and unescaped.

The unescaped form is essentially a single namespace byte plus and arbitrary-length sequence of arbitrary bytes, in which a `\0` byte separates parts.
The escaped name is a more complicated representation as a `\0`-terminated string that maps 1:1 onto unescaped names.
More details can be found in [the relevant docs](/doc/KEYNAMES.md).

There is a conflict between these two forms in terms of API convenience and efficient execution.
Generally, the escaped form is more convenient to use, since it is entirely human-readable and just a "normal" string.
Implementing many tasks (like order comparisons) is, however, much simpler when using the unescaped name.
Additionally, using the unescaped name often results in more performant code as well.

A particularly common example that highlights the difficulties in handling escaped names is splitting the name into parts.
In the escaped name, this task requires correct handling of escape sequences, whereas in the unescaped name parts are always delimited by a `\0` byte.

Of course a naive way to solve this dilemma is to say "why not both" and store both versions.
However, this can result in a lot of memory use, so it is not immediately the best option.

The question now is, which representations should be used by `libelektra-core` and how.

## Constraints

- Because `KeySet` is ordered by name and stores `Key`, the order comparison between the name of two `Key`s must be fast.
- We need a single pointer to a single buffer that contains the entire name of a `Key`.
  While there are other options, some of which could even save memory (e.g., split into parts and deduplicate), much of the `KeySet` internals rely on the fact that the name is a single buffer.
  Changing this would require major redesigns.

## Assumptions

- In most cases the escaped name is used for developer convenience and not because of actual requirements.
- The most common requirement for using the escaped name is UI: reading names from or displaying them in a user interface (e.g., `kdb` CLI)

## Considered Alternatives

### Only escaped name

Because the escaped name is a simple `\0`-terminated string, it can be represented as a single `char *`.

Storing the name as a single `char *` would be the most space efficient.
But resizing would require counting the length every time.
Therefore, for storage the better solution may be a `char *` and a `size_t`.

However, in the API the name could always be a single `char *`, making for a very easy to use API.

The biggest problem with this approach is that comparing two escaped names is not trivial.
The comparison needs to account for namespaces, parts and escaping.

Similarly, iterating over the individual parts of a name (and/or manipulating them) is non-trivial, because it requires logic to handle escape sequences.

### Only unescaped name

The unescaped name contains `\0` bytes.
It therefore must be represented as a pointer and a size.

This can make for less convenient API, but there are mitigation strategies using additional types.
Using unescaped names in code can be inconvenient, especially regarding the namespace.
Without a namespace a name could be written as e.g., `"foo\0bar"`.
But with a namespace it would be something like `"\1\0\foo\0bar"` and developers would need to remember what namespace `\1` is.
Using the `KEY_NS_*` constants like this is not easily possible.

Both order and hierarchy comparisons are very simple in this case and can be implemented with a single `memcmp` and a tiny amount of extra logic (e.g., to handle cascading names).
Iterating over the individual parts is also trivial, since all parts are separated by `\0` bytes.

### Only unescaped name, with separate namespace

In the above solution, the entire unescaped name (including the namespace) would always be considered one unit.
As such, there would only be a single pointer and a size in an API that needs a name.
This can be inconvenient, because it makes using the `KEY_NS_*` constants more difficult.

This solution enhances the above, by considering the namespace a separate thing.
In the API the name could now be given as separated into namespace and the rest of the name.
Instead of taking a single pointer and size, which receive values like `"\1\0foo\0bar"` and `10`, the API would take a namespace, a pointer, and a size, with values like `KEY_NS_CASCADING`, `"foo\0bar"` and `8`.

Internally, we don't necessarily need to store this a separate fields.
The namespace could be combined into one buffer with the rest of the name, and stored as a single pointer and size.
However, depending on the API there can also be benefits to keeping the namespace as a separate field.

Even with a separate namespace field, the most benefits of "Only unescaped name" are retained.
The memory consumption is near minimal (alignment padding can cause a difference).
Comparisons are exactly the same, just with an additional namespace byte comparison beforehand.

### Both escaped and unescaped name

The previous approach used both to combine the advantages of escaped and unescaped name.

The API could largely rely on the escaped name, while e.g., comparisons can use the unescaped name.

The issue with this approach is the insane memory consumption.
Keynames can already be quite long and `Key` is at the base of Elektra.
Storing every name twice in only slightly different forms essentially doubles the memory consumption.

### Both escaped and unescaped name, but only unescaped stored

Instead of storing both escaped and unescaped name, only the unescaped name could be stored.

APIs that use the escaped name would do conversion on the fly.

This approach has several downsides.
First, while the conversion may be optimized, it will never be free in terms of runtime.
But more importantly, if an escaped name should be returned by an API, it must be stored somewhere.
This means extra allocations and crucially somebody needs to do the cleanup.
In other words, it complicates the API.

### Escaped and unescaped name in single buffer

Another variant of the above.
The escaped and unescaped name are stored in a single buffer.
This avoids extra allocations and extra pointers and sizes in structs.

The escaped name could also be stored lazily only when needed.
This would solve the cleanup problem.

While this may seem like the ideal solution, there are still some downsides.
The biggest problem is the API design.
If the API uses escaped names a lot (because it is more convenient), then this essentially degrades into the "Both escaped and unescaped name" solution.
Even if APIs exist for both escaped and unescaped names, the convenience benefit, will lead to more use of escaped names.
This means the escaped name will be stored for many keys and therefore the benefit of the lazy allocation is negated.

Without the lazy allocation benefit, the only difference to "Both escaped and unescaped name" is that we have fewer pointers and sizes in structs.
This saves some amount of memory and allocations, but makes internal code more difficult to write and understand.

## Decision

Go with "Only unescaped name, with separate namespace" from above:

- Store only unescaped name inside `struct _Key`
- API of `libelektra-core` will use unescaped name exclusively
- Convenience functions using escaped names, will be provided via other libraries
- Where appropriate the API will take the namespace as a separate argument to allow using `KEY_NS_*` constants.
- Whether namespace is stored separately in `struct _Key` will be decided at a later point, when the scope of all API changes and changes to `struct _Key` is clear.

## Rationale

- Largest memory savings among the proposed options
- Option to use separate namespace argument leads to more convenient API (`KEY_NS_*` constants).
- Simple internal code
- Escaped name requirements can easily be solved by additional library (e.g., `libelektra-ease`), because not every caller will need those.
- Full API and internal struct layout aren't designed yet, so deciding how to store namespace is difficult.

## Implications

- `keyNew` needs to change
- `keyName` returns unescaped name
- functions for escaped name move out of core

## Related Decisions

## Notes

### Printing unescaped name in GDB

In GDB (and probably others) the unescaped name of a `Key * key` can be printed with (assuming the name is in `key->ukey` and its size in `key->keyUSize`):

```
p *key->ukey@key->keyUSize
```

This prints `key->ukey` as a fixed-length string of length `key->keyUSize`, e.g., for `user:/abc` it prints:

```
$1 = "\006\000abc"
```
