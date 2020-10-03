# Key Names in Elektra

## Preface

This document is a full explanation of how Key Names work in Elektra.
In addition to this document, a reference Python implementation can be found in [keynames.py](keynames.py).
The goal this Python implementation is not to be fast, or to be used in any way other than as a reference.
If there are any discrepancies between this document, the Python implementation and the actual C implementation in [src/libs/elektra/keyname.c](../../src/libs/elektra/keyname.c), you should consider them as follows:

1. The C implementation is optimized for speed and much harder to maintain.
   It is mostly likely to be incorrect.
2. In most cases, this document outranks the Python implementation.
   There may, however, be cases where the language in this document was too vague and the Python implementation is actually correct.
3. If two of the sources agree, the third one is probably incorrect.
   Although again, if one of the agreeing sources is the C implementation it could still be the case that there is a mistake.

In any case: If you find a discrepancy, please file a bug report at https://issues.libelektra.org/new.

> _Note:_ Mistakes happen.
> So there is no 100% always correct specification for Elektra's Key Names.
> The goal is only to provide a reference that has a very high likelihood of being correct.

_To Elektra Developers:_ Feel free to add any unclear or previous incorrect examples to the test cases in [tests/ctest/test_keyname.c](../../tests/ctest/test_keyname.c).
These tests are very fast (1000+ test cases per second) and the more tests the better.

## 1. Key Name Parts and Namespaces

Before we get to Key Names proper, we need to talk about Key Name Parts and Namespaces.

Each Key is part of one of these Namespaces:

- Cascading
- Meta
- Spec
- Proc
- Dir
- User
- System
- Default

Each of these Namespaces has a very specific meaning, explained in the next section.

Apart from the Namespace, a Key Name is just a series of Key Name Parts.
Each Key Name Part is just an arbitrary sequence of non-zero bytes.

So without knowing anything about how Key Names are written, we could say that there is a Key in the Namespace "System" with the Key Name Parts "elektra", "version" and "info".

> _Note:_ Not all of these sequences, is a valid Key Name.
> For more information see [section 4](#4-valid-and-invalid-key-names)

### 1.1. Namespaces

TODO

<!-- TODO (kodebach): write section -->

## 2. Escaped Names

The standard way to represent a Key Name in Elektra is this:

```
system:/elektra/version/info
```

This can be deconstructed into:

- The Namespace: `system`
- The Namespace separator: `:`
- A Part Separator: `/`
- A Key Name Part: `elektra`
- A Part Separator: `/`
- A Key Name Part: `version`
- A Part Separator: `/`
- A Key Name Part: `info`
- The invisible null terminator

> _Note:_ It might seem strange, that there is a part **separator** before the first part.
> This makes sense, because then the Part Separator always introduces a new part.
> A better fitting description would be "part introducer".
> But since we commonly call `/` a separator, we will stick to this terminology.

For Keys in the Namespace "Cascading", we omit both the Namespace itself and as well as the Namespace separator:

```
/elektra/version/info
```

But there is a problem.
We said a Key Name part is "an arbitrary sequence of non-zero bytes".
This means "elektra/version" is a Key Name Part as well.
Since the slash `/` would conflict with the Part Separator, we can escape it with a backslash `\` (and `\` can be escaped with another `\`):

```
/elektra\/version\\/info
```

This can be deconstructed into:

- A Part Separator: `/`
- A key name part: `elektra/version\`
- A Part Separator: `/`
- A key name part: `info`
- An invisible null terminator

> _Note:_ When talking about a single Key Name Part `/` and `\` are never escaped.

Because of this escaping mechanism, we call this the _Escaped Name_ of a Key.

Elektra's Key Names are designed, to mimic UNIX paths to an extent.
To this end we support the commonly used `/.` and `/..`.
But to understand, how these work exactly, we need to understand the difference between _Canonical_ Key Names and _Non-Canonical_ Key Names.

### 2.1. (Non-)Canonical (Escaped) Key Names

> _Note:_ Only Escaped Key Names can be Canonical or Non-Canonical, so we normally omit the "Escaped" specifier.

<!-- TODO (kodebach): complete section -->

Canonical Key Names always consist of at least one Key Name Part.
If there is just a single part, that part may also be empty, otherwise empty parts are not allowed.
This makes `/` the shortest possible Escaped Name.

### 2.2. Other Escape Sequences

TODO

<!-- TODO (kodebach): write section -->

## 3. Unescaped Names

While the Escaped Name of a Key is nice for humans, it is not very well suited for machines.
The escaping of path separators makes it hard to find the Key Name Parts of a given Key Name.
The Escaped Name is also not well suited for sorting [[1]](#footnote-1).
<a id="ref-footnote-1"></a>

Both of these flaws are solved by the _Unescaped Name_.
In Unescaped Names we use a zero-byte as the Part Separator.
Since a Key Name Part cannot contain a zero-byte, we do not need an escaping mechanism for the path separator.

However, these zero-bytes mean that the Unescaped Name is not a printable string and therefore not human-readable.
This why we will only describe the Unescaped Name in the deconstructed form.

The Escaped Names from above correspond to the following Unescaped Names:

1. `system:/elektra/version/info`
   - The byte representing the Namespace "System": `0x07`
   - A Part Separator: `0x00`
   - A Key Name Part: `elektra`
   - A Part Separator: `0x00`
   - A Key Name Part: `version`
   - A Part Separator: `0x00`
   - A Key Name Part: `info`
   - The terminator byte: `0x00`
2. `/elektra/version/info`
   - The byte representing the Namespace "Cascading": `0x01`
   - A Part Separator: `0x00`
   - A Key Name Part: `elektra`
   - A Part Separator: `0x00`
   - A Key Name Part: `version`
   - A Part Separator: `0x00`
   - A Key Name Part: `info`
   - The terminator byte: `0x00`
3. `/elektra\/version\\/info`
   - The byte representing the Namespace "Cascading": `0x01`
   - A Part Separator: `0x00`
   - A Key Name Part: `elektra/version\`
   - A Part Separator: `0x00`
   - A Key Name Part: `info`
   - The terminator byte: `0x00`
4. `/`:
   - The byte representing the Namespace "Cascading": `0x01`
   - A Part Separator: `0x00`
   - The terminator byte: `0x00`

> _Note:_ We use 0xZZ to represent a single byte in hexadecimal.
> This form is only used when the context makes it clear that it represents a single byte and not a four character string.

The process of turning an Escaped Name into the corresponding Unescaped Name is called _unescapding_.
Turning an Unescaped Name back into an Escaped Name is called _escaping_.

## 4. Valid and Invalid Key Names

Not all of the Key Names described by the above sections are valid under all circumstances.
You might also say, that what we defined as a Key Name above, not necessarily refers to a Key in the KDB.
So while it might be a Key Name, there is not Key that actually uses this name.
Only _Valid Key Names_ refer to a Key in the KDB / are used by some Key as a name.
The remaining Key Names may be referred to as _Invalid Key Names_.

<!-- TODO (kodebach): describe what makes a Key Name (in)valid -->

The C-API does not allow you to construct a `Key` with an Invalid Key Name; for example `keyNew` (and `keyVNew`) will return `NULL`.

## 4.1. Reserved Key Names

Apart from Invalid Key Names, which cannot be constructed via the C-API, there are also _Reserved Key Names_.
These can be used with the C-API (`keyNew` returns a valid `Key *`), but there might be situations, in which `Key`s with such Key Names are treated differently.

A simple example is `/somewhere/_/below`.
If used with the globbing functionality, the Key Name Part `_` will be interpreted as "match anything that is not an array element".

<!-- TODO (kodebach): complete section -->

---

<a id="footnote-1">[1]:</a>
For performance reasons, we want to make the comparison between to Key Names as fast as possible.
A good solution is a single `memcmp`.
But this doesn't account for the fact that Key Names represent a hierarchy and that `/` has a special meaning:
`/key/sub` should always be sorted after `/key` and before `/key.1`.
With `memcmp`, `/key` is first, because it is the shortest and otherwise equal.
But then we would get `/key.1` not `/key/sub`, because `/ < .` in ASCII.
This cannot happen with zero-bytes as the separator, because there is no byte with a smaller value.
[↑](#ref-footnote-1)
