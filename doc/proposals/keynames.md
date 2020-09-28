# Future-proof 1:1 Relation for Key Names

## Current situation

First we will explore the current state of key names in Elektra.

### Key Name Parts and Namespaces

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

These Namespaces do have a certain meaning, but that is irrelevant to this proposal, so I will not go any further into that.

If you ignore the Namespace, a Key Name is just a series of Key Name Parts.
Each Key Name Part is just an arbitrary sequence of non-zero bytes.

So without knowing anything about how Key Names are written, we could say that there is a Key in the Namespace "System" with the Key Name Parts "elektra", "version" and "info".

### Escaped Names

The standard way to represent a Key Name in Elektra is this:

```
system:/elektra/version/info
```

This can be deconstructed into:

- The Namespace: `system`
- The Namespace separator: `:`
- A part separator: `/`
- A Key Name Part: `elektra`
- A part separator: `/`
- A Key Name Part: `version`
- A part separator: `/`
- A Key Name Part: `info`
- The invisible null terminator

> _Sidenote:_ It might seem strange, that there is a part **separator** before the first part.
> This makes sense, because then the part separator always introduces a new part.
> A better fitting description would be "part introducer".
> But since we commonly call `/` a separator, I will stick to this terminology.

For Keys in the Namespace "Cascading", we omit both the Namespace itself and as well as the Namespace separator:

```
/elektra/version/info
```

But there is a problem.
We said a Key Name part is "an arbitrary sequence of non-zero bytes".
This means "elektra/version" is a Key Name Part as well.
Since the slash `/` would conflict with the part separator, we can escape it with a backslash `\` (and `\` can be escaped with another `\`):

```
/elektra\/version\\/info
```

This can be deconstructed into:

- A part separator: `/`
- A key name part: `elektra/version\`
- A part separator: `/`
- A key name part: `info`
- An invisible null terminator

> Sidenote: When talking about a single Key Name Part `/` and `\` are never escaped.

Because of this escaping mechanism, we call this the "Escaped Name" of a Key.

> Sidenote: Technically there are canonical and non-canonical Escaped Names, but this distinction is not relevant to this proposal.
> In this proposal an Escaped Name is always the canonical version.

Key Names always consist of at least one Key Name Part.
If there is just a single part, that part may also be empty, otherwise empty parts are not allowed.
This makes `/` the shortest possible Escaped Name.

### Unescaped Name

While the Escaped Name of a Key is nice for humans, it is not very well suited for machines.
The escaping of path separators makes it hard to find the Key Name Parts of a given Key Name.
The Escaped Name is also not well suited for sorting.

Both of these flaws are solved by the Unescaped Name.
In Unescaped Names we use a zero-byte as a part separator.
Since a Key Name Part cannot contain a zero-byte, we do not need an escaping mechanism for the path separator.

However, these zero-bytes mean that Unescaped Name is not a printable string and therefore not human-readable.
This why I will only describe the Unescaped Name in the deconstructed form.

The Escaped Names from above correspond to the following Unescaped Names:

1. `system:/elektra/version/info`
   - The byte representing the Namespace "System": `0x07`
   - A part separator: `0x00`
   - A Key Name Part: `elektra`
   - A part separator: `0x00`
   - A Key Name Part: `version`
   - A part separator: `0x00`
   - A Key Name Part: `info`
   - The terminator byte: `0x00`
2. `/elektra/version/info`
   - The byte representing the Namespace "Cascading": `0x01`
   - A part separator: `0x00`
   - A Key Name Part: `elektra`
   - A part separator: `0x00`
   - A Key Name Part: `version`
   - A part separator: `0x00`
   - A Key Name Part: `info`
   - The terminator byte: `0x00`
3. `/elektra\/version\\/info`
   - The byte representing the Namespace "Cascading": `0x01`
   - A part separator: `0x00`
   - A Key Name Part: `elektra/version\`
   - A part separator: `0x00`
   - A Key Name Part: `info`
   - The terminator byte: `0x00`
4. `/`:
   - The byte representing the Namespace "Cascading": `0x01`
   - A part separator: `0x00`
   - The terminator byte: `0x00`

The process of turning an Escaped Name into the corresponding Unescaped Name is called _unescapding_.
Turning an Unescaped Name back into an Escaped Name is called _escaping_.

## Problem

At first, everything seems fine.
Key Names can be split into their Key Name Parts easily, and we also have simple sorting via `memcmp` with Unescaped Names.
But there is a Problem: We want to give additional meaning to some Key Name Parts.

For example, by convention we represent array elements as Key Name Parts that look like this: `#__123`.
To retain the simple sorting via `memcmp` these Key Name Parts have to follow a strict form: `#` followed by `n >= 0` underscores followed by an `n+1` digit number.
The Problem now is, **any** "arbitrary sequence of non-zero bytes" is a valid Key Name Part.
That means, we need to look at the whole Key Name Part before we can determine that it is an array element, because e.g. `#__123a` is a valid Key Name Part, but certainly not an array element.
This is similar to how splitting an Escaped Name into Key Name Parts is difficult. Ideally, we would look at the first byte in a Key Name Part and immediately know whether it is an array element (first byte is `#`) or not (first byte is not `#`).

At first, it seems this cannot be achieved, while maintaining the definition that a Key Name Part is "an arbitrary sequence of non-zero bytes".
But this is not so.
In fact what we need to change is how Unescaped Names work.

Let's ignore the Namespace, the first part separator and the terminator byte of an Unescaped Name for a moment and call the result the "Core Name" of the Key.
Now, to repeat: **any** "arbitrary sequence of non-zero bytes" is a Key Name Part.
If you think about it, a "Core Name" is just a series of Key Name Parts separated by zero-bytes.
Therefore, **any** "arbitrary sequence of bytes" (**including** zero-bytes) is a "Core Name".
And that is what needs to change.

If we put some restrictions on what a valid "Core Name" is, we can keep the definition of a Key Name Part and at the same time, give some meaning to Key Name Parts within Unescaped Names.
The important part here is that Key Name Part will **only** have additional meaning **within** the context of an Unescaped Name.

## Goals

### A Bit of Terminology

Currently, there is the notion of a "Base Name".
I find this name slightly confusing and will therefore **not** use it.
Instead, I will refer to what is currently known as the "Base Name" of a Key simply as its Last Unescaped Part.
This allows further terminology like the Last Escaped Part, which currently has no name.

Consequently, I will when referring to functions like `keyBaseName` or `keySetBaseName`, I will instead use the names `keyLastUnescapedPart` and `keySetLastUnescapedPart`.

### The Goals

1. The definition of Key Name Part ("an arbitrary sequence of non-zero bytes") MUST remain unchanged.
2. It SHOULD be possible to use any Key Name Part in a Key Name.
3. Every (valid) Unescaped Name MUST uniquely identify a single key.
4. A Key Name Part SHOULD be able to have additional meaning within Unescaped Names.
5. Determining whether a Key Name Part has additional meaning should as simple as possible.
6. `keySetLastUnescapedPart (k, keyLastUnescapedPart (k))` MUST NOT modify the Key Name of `k`.
   Similarly, if the result of `keyLastUnescapedPart` is stored (`strdup`ed), then is used `keySetLastUnescapedPart (k, NULL)` to remove the last unescaped part and finally the stored part is re-added with `keyAddLastUnescapedPart`, the Key Name of `k` MUST NOT be modified either.
7. `keySetLastEscapedPart (k, keyLastEscapedPart (k))` MUST NOT modify the Key Name of `k`.
   Similarly, if the result of `keyLastEscapedPart` is stored (`strdup`ed), then is used `keySetLastEscapedPart (k, NULL)` to remove the last unescaped part and finally the stored part is re-added with `keyAddLastEscapedPart`, the Key Name of `k` MUST NOT be modified either.
   > Sidenote: None of these functions currently exist, but `keyLastEscapedPart` can be thought of as calling `keyLastUnescapedPart` and then properly escaping the result.
   > `keySetLastEscapedPart (k, p)` is equivalent to calling `keySetLastUnescapedPart (k, NULL)` (which removes the last part of `k`) and then calling `keyAddName (k, p)`, assuming that `p` is a properly escaped Key Name Part.
   > Finally, `keyAddLastEscapedPart` is just equivalent to `keyAddName`, except that we assume the argument doesn't contain any unescaped `/`.
8. There MAY be a set of Key Name Parts that, in the context of certain Unescaped Names, are completely illegal and make the Unescaped Name invalid.
   This provides future-proofness, because these can be made legal and given a additional meaning in the future.

## Solution

### The Basic Idea

The structure of a "Core Name" changes from "`0x00`, Key Name Part, `0x00`, Key Name Part, ..." to "`0x00`, (optional byte), Key Name Part, `0x00`, (optional byte), Key Name Part, ...".
The "optional byte" give the following Key Name Part meaning and alter the interpretation of the whole Key Name.
I will call this "optional byte" the Part Meaning Byte (PMB).
The Part Meaning Byte must be from the Set of pre-defined Part Meaning Bytes (PMB-Set).

> Note: Part Meaning Byte refers to the byte within the "Core Name" or the Unescaped Name.
> To keep the terminology clear, will not use Part Meaning Byte or PMB when talking about bytes in the PMB-Set outside the context of a specific Key Name.

### Part Meaning Byte

Each byte B in the PMB-Set defines a set of Key Name Parts the KP-Set of B.
If a PMB is followed by a Key Name Part that is not in its KP-Set, the resulting byte-sequence is not a valid "Core Name" and can therefore not be a valid Unescaped Name.

The PMB-Set consists of these bytes: `\`, `#`, `.`, `_`, `*`, `$`, `@`, `&`, `?`, `%` (and maybe `{`)

Normally, you must take the PMB into account, when determining what Key an Unescaped Name refers to.

For example, consider the following Unescaped Names:

|                   | Key 1   | Key 2   |
| ----------------- | ------- | ------- |
| Namespace         | System  | System  |
| part separator    | `0x00`  | `0x00`  |
| Part Meaning Byte | `#`     | `.`     |
| Key Name Part     | `__123` | `__123` |
| Terminator Byte   | `0x00`  | `0x00`  |

These refer to different Keys.
Key 1 is the 123rd array element below the root key in the Namespace "System".
Key 2 is the hidden Key with Name `.__123` below the root key in the Namespace "System".

There is however one exception. The PMB `\` is not considered, when determining what Key an Unescaped Name refers to.
This allows arbitrary Key Name Parts (i.e. "arbitrary sequences of non-zero bytes") to be used in Unescaped Names, without giving them additional unintended meaning.

But since an omitted PMB obviously cannot influence what Key an Unescaped Name refers to, we have problem:

|                   | Key 1           | Key 2  |
| ----------------- | --------------- | ------ |
| Namespace         | System          | System |
| part separator    | `0x00`          | `0x00` |
| Part Meaning Byte | `\` | (omitted) |
| Key Name Part     | `abc`           | `abc`  |
| Terminator Byte   | `0x00`          | `0x00` |

These would both refer to the Key named `abc` directly below the root of the Namespace "System".

To solve this the following restrictions apply to the PMB `\` and when the PMB is omitted:

- If the PMB is omitted, only Key Name Parts whose first byte **is not** in the PMB-Set may follow.
- If the PMB is `\`, only Key Name Parts whose first byte **is** in the PMB-Set may follow.

Here is an overview of the other bytes in the PMB-Set:

- `#`: Used for arrays (see [below](#solution-for-arrays)), and globbing (if the following Key Name Part is empty)
- `.`: Used for hidden Keys. Any Key Name Part can follow the PMB `.`, but the resulting Unescaped Name refers to a hidden Key.
- `_`: Used for globbing. Following Key Name Part MUST be empty.
- `*`: Used for globbing. Following Key Name Part MUST be empty.
- `$`: Proposed for general purpose special-use Key Name Parts. (see [below](#the-million-$-solution))
- `@`: Refers to the Parent Key of the current context. Following Key Name Part MUST be empty.
- `&`: Reserved for future use. Use of `&` as a PMB makes the Unescaped Name invalid (for now).
- `?`: Reserved for future use. Use of `?` as a PMB makes the Unescaped Name invalid (for now).
- `%`: May be used for contextual values, otherwise reserved for future use like `&` and `?`.
- `{`: Proposed as a replaced for `%` in contextual values (see below).

Now, we have achieved the first 5 of our [goals](#the-goals), as well as Goal 8 (via at least `&` and `?`).
We will talk about Goal 6 and Goal 7 shortly.

### Unescaped Parts vs Literal Parts

In the current solution, the term Unescaped Part refers to a single Key Name Part within an Unescaped Name.
But we could equivalently define it Unescaped Part as, a sequence of non-zero bytes within the Unescaped Name that is surrounded by a two zero bytes. (Note: "surrounded by two zero bytes" intentionally excludes the Namespace byte)

For this proposal, we will use the second definition.
Therefore, an Unescaped Part is Key Name Part within an Unescaped Name as well as the preceding Part Meaning Byte if it is present.

This way, an Unescaped Part (obtain e.g. via `keyLastUnescapedPart (k)`) can be used to properly determine the meaning of a Key Name Part.

This proposal also introduces the term _Literal Part_.
A Literal Part is a Key Name Part without any additional meaning.
Within an Unescaped Name it is always preceded by no PMB at all or the PMB `\`.

### Escaped Parts and the Part Meaning Byte

The Part Meaning Byte is included in the Escaped Part just like it is in the Unescaped Part.
The Escaped Part is still obtained, by simply escaping any `/` and `\` within the Unescaped Part with a `\`.

This also means, that the PMB `\` is represented by **two** leading backslashes `\` in the Escaped Part.
An Escaped Part starting with a single `\`, followed by neither `\` nor `/`, is invalid as it ever was.

### Constructing Keys Names from Key Name Parts

There has to be some way to construct a Key Name from a sequence of Key Name Part.
While there could be a lot of APIs to do this, I will only consider the most basic version.

The following functions build the base of the API:

- `keyAddPart (key, pmb, part)`: Adds Part Meaning Byte `pmb` (if `pmb != NULL`) and the Key Name Part `part` to the end of the Key Name of `key` (also adds a part separator or a new terminator byte).
- `keySetLastPart (key, pmb, part)`: Replaces the last PMB in the Key Name of `key` with `pmb` (if `pmb != NULL`) and the last Key Name Part with `part`.
- `keyRemoveLastPart (key)`: Removes the last PMB and last Key Name Part from the Key Name of `key`.
- `keyLastPart (key)`: Returns the last Key Name Part in the Key Name of `key`.
- `keyLastPMB (key)`: Returns the PMB for the last Key Name Part in the Key Name of `key`, if there is one, or `NULL` otherwise.

> Note: The functions above are **not** intended to be part of the C API (neither public nor internal).
> They are only used to make the definitions below simpler.

All of these functions modify the Unescaped Name as well as the Escaped Name stored in `key`.
The `UnescapedPart` in the function name indicates, that `part` must be Key Name Part that is allowed to follow the PMB `pmb` within an Unescaped Name, i.e. `part` must be in the KP-Set of `pmb`.

With these functions, we can redefine the currently existing API:

- `keyAddUnescapedPart (key, part)`: The same as calling `keyAddPart (key, pmb, _part)`, where:
  - If `part[0]` is in the PMB-Set: `pmb = part[0]` and `_part` is the rest of `part`
  - Otherwise: `_part = part` and `pmb = NULL`.
- `keyAddEscapedPart (key, part)`: The same as calling `keyAddUnescapedPart (key, _part)`, where `_part` is the unescaped version of `part`.
- Similarly, we define `keySetUnescapedPart (key, part)` and `keySetEscapedPart (key, part)`, with the addition that we call `keyRemoveLastPart (key)`, if `part == NULL`.
- `keyAddName (key, suffix)`: We treat `suffix` as an Escaped Name from the Namespace "Cascading" (if necessary we add a leading `/`).
  We calculate the corresponding Unescaped Name and then add its Unescaped parts to `key` one after another.
- `keyLastUnescapedPart (key)`: We call both `pmb = keyLastPMB (key)` and `part = keyLastPart (key)` and return `part` prefixed with `pmb`, if `pmb != NULL`, or just `part` otherwise.
- `keyLastEscapedPart (key)`: We call `keyLastUnescapedPart (key)` and then properly escape the result.

We can now see, that the [Goals](#the-goals) 5 and 6 are fulfilled.

This, however, leaves us with a problem.
We restricted the arguments that can be given to `keyAddUnescapedPart` and `keySetLastUnescapedPart`.
This is not a desirable outcome, because there are applications that rely on the fact that these functions accept any argument.

As a partial solution we will introduce `keyAddLiteralPart` and `keySetLastLiteralPart`.
These should be used, if you want the Key Name Part argument to have no additional meaning, no matter what its first character is.

The definition is simple:

- `keyAddLiteralPart (key, part)`:
  - If `part[0]` is in the PMB-Set: prefix `part` with `\` and then call `keyAddUnescapedPart`
  - Otherwise: just call `keyAddUnescapedPart`
- `keySetLiteralPart (key, part)`:
  - If `part[0]` is in the PMB-Set: prefix `part` with `\` and then call `keySetUnescapedPart`
  - Otherwise: just call `keySetUnescapedPart`

We will intentionally **not** introduce a `keyLastLiteralPart` function.
"Literal Part" means the Key Name Part has no additional meaning.
This is not always true for the last part of a Key Name, so the function must have some way of dealing with other Key Name Parts.

There are three options to do this reasonably:

1. Return `NULL` if the result of `keyLastUnescapedPart` is not a Literal Part
2. Prefix the result of `keyLastUnescapedPart` with a `\` if it is not a Literal Part.
   This removes the additional meaning the PMB added and makes it a Literal Part.
3. Return the result of `keyLastUnescapedPart`, even if it is not a Literal Part.
   This makes the function fully equivalent to `keyLastUnescapedPart`, so I will ignore it.

The first two options have problems when it comes to the equivalent version of Goal 5 and 6:
One might expect that `keySetLiteralPart (key, keyLastLiteralPart (part))` would not affect the Key Name `key`.
But, if we use option 1, the last Key Name Part will be removed, if it is not a Literal Part.
With option 2, e.g. an array element will be prefixed with a `\` and loose the array element meaning.

Therefore instead of a `keyLastLiteralPart` function, we will provide: `keyPartIsLiteral (part)`.
This function just looks at `part[0]`. If it is in the PMB-Set and `part[0] != '\'`, we return `false` otherwise we return `true`.

This way if somebody was expecting `keyLastUnescapedPart` to return a literal part (e.g. because the original created the key with `keyAddLiteralPart`), they can verify this assumption with `keyPartIsLiteral`.
To obtain the original Key Name Part, they just need to ignore a (single!) leading backslash `\`, if there is one.

### Solution for Arrays

> Sidenote: `#` is used for globbing, if an empty Key Name Part follows.
> Here we only consider the case where the PMB `#` is followed by a non-empty Key Name Part.

The PMB `#` would be used for array elements.
The following Key Name Part MUST consist of `n` underscores followed by `n+1` digits.
This mimics the current structure of array elements, but designates the `#` as the PMB.

### The Million `$` Solution

There is a very general problem, with Elektra's current Key Names.

There is absolutely no way to avoid name collisions without introducing escaping mechanisms everywhere.
This is because simply is no set of reserved names.

This proposal solves this.

The PMB `$` gets a very special meaning.
Any Key Name Part may follow `$` to construct a valid Key Name.

However, such Keys may only be used in under very limited circumstances.
For this proposal we will consider the various problems a storage plugin, might have with the way Elektra's `KeySet`s work.

Therefore, we define that any Key whose Key Name contains a PMB `$` MUST NOT be returned by any plugin and MUST NOT be passed into `kdbSet`.
In essence, the PMB `$` designates the Key Name as referring to a purely in-memory key that (other than Keys in the Namespace "Proc") has no pre-defined relation to any other Key in the KDB.
This might sound, a bit confusing, but bare with me.

### Storage Plugins

Storage plugins and storage formats come in a wide variety and support different subsets of Elektra's KDB structure.

The following features might or might not be supported natively by the storage format:

1. Nested Structures: JSON has it, Java Properties doesn't
2. Native Arrays: JSON has it, Java Properties doesn't
3. Non-leaf keys with values: possible in XML (via text nodes), not possible in JSON
4. Metadata: possible in XML (via attributes), not possible in JSON

> Note: The second and third feature are only relevant if we have nested structures.

#### Metadata

First let's talk about metadata.
Many formats struggle to deal with the idea of metadata.
With this proposal it becomes easy.

Let's assume the Key `info/color` has the metadata `color = css_name`.

Now we could just store the metadata as `info/color/color`.
But what if this Key also exist?

The solution is to store the metadata as `info/color/$meta/color`.
Because of the limitations imposed by the PMB `$`, we know there cannot be name collision.

This translation can be used for flat formats like Java Properties:

```properties
info.color.$meta.color = css_name
```

as well as nested formats like JSON:

```json
{
  "info": {
    "color": {
      "$meta": {
        "color": "css_name"
      }
    }
  }
}
```

> Sidenote: Technically this allows e.g. `info/color/$meta/color/$meta/color`, i.e. Metadata on Metadata.
> Whether that is something desirable or should result in an error during a storage plugins read phase has to decided.

#### Nested Structures

First we'll consider a version of JSON that has no support for arrays.

Let's use this JSON file:

```json
{
  "version": {
    "info": "Information about the version",
    "major": 4,
    "minor": 3
  }
}
```

A storage plugin would translate this into a `KeySet` with three Keys ("Escaped Name" = Value):

```
version/info = Information about the version
version/major = 4
version/minor = 3
```

So far so good.
The problem arises, when we have a file like this:

```json
{
  "version": {
    "special/info": "Information about the version",
    "#major": 4,
    "#_12": 3
  }
}
```

Well, this actually has an easy solution.
It should result in an error.
Our version of JSON doesn't have native arrays, so we have to support Elektra's version of arrays.
But that means in our version of JSON `"#major#` is not a valid field name, and therefore this file has a syntax error.

Let's instead look at the syntactically correct version:

```json
{
  "version": {
    "special/info": "Information about the version",
    "\\#major": 4,
    "#_12": 3
  }
}
```

This should be translated into ("Escaped Name" = Value):

```
version/special\/info = Information about the version
version/\\#major = 4
version/\\#_12 = 3
```

> Note: we see the Escaped Name here, so the `\\` is actually just a single backslash and `\/` is just a slash.

To achieve this we construct the Key Names via a series of `keyAddUnescapedPart`.

When we want to produce the JSON for a certain `KeySet`, there is a lot of work needed to turn the flat `KeySet` into a set of named nested objects.
In this proposal we will only think about how a single Key Name can be translated into a series of names for these nested objects.

For our version of JSON it is easy, we have no native support for anything beyond nesting, so we just use the Unescaped Parts returned by e.g. `keyLastUnescapedPart` for the names of the objects.

#### Non-Leaf Keys with Values

Next let's look at non-leaf Keys with values (NLV-Key).

An NLV-Key is a Key that has both a value and other Keys below.
For example consider this `KeySet`:

```
version = Information about the version
version/major = 4
version/minor = 3
```

We again want to write this in our version of JSON without arrays (although arrays wouldn't make a difference here).
We achieve this again via a PMB `$`.

We can translate the `KeySet` above into:

```
version/$value = Information about the version
version/major = 4
version/minor = 3
```

And then we produce:

```json
{
  "version": {
    "$value": "Information about the version",
    "major": 4,
    "minor": 3
  }
}
```

#### Arrays

Finally, let's look at arrays.
We'll now use proper JSON (with array support).

```json
{
  "version": [4, 3],
  "more": {
    "#3": "Not an array element!",
    "#other": "Also not an array element"
  }
}
```

This should be turned into (ignoring the metadata for the array length)

```
version/#0 = 4
version/#1 = 3
more/\\#3 = Not an array element!
more/\\#other = Also not an array element
```

We won't go into exactly how this is done, but it will involve `keyAddUnescapedPart` and `elektraWriteArrayNumber` for the array elements and `keyAddLiteralPart` for other fields.

In effect the Key `version/#0` would be constructed via `keyAddLiteralPart (k, "version")` and `keyAddUnescapedPart (k, "#0")`.
And `more/\\#3` would come from `keyAddLiteralPart (k, "more")` and `keyAddLiteralPart (k, "#3")`.

> Sidenote: the `keyAddPart (k, pmb, part)` function used in the definitions above, might actually be helpful here

The more interesting part is the reverse.
Here we have to look at each Unescaped Part and determine if it is:

1. A Literal Part (with or without a PMB `\`)
2. An array element Part, i.e. PMB `#`
3. Some other Part

In the first and fourth case, we can just take the Unescaped Part, remove the leading `\` if it is present, and use that as our JSON field name.
If it is an array element (case 3), we need to put it into the array.

If the `KeySet` is a bit weird, and there are array elements and non-array elements directly below the same Key, we can again employ the help of the PMB `$`.

For example this `KeySet`

```
weird/key/foo = non-array
weird/key/#0 = array
```

can be written as this JSON:

```json
{
  "weird": {
    "key": {
      "$array": ["array"],
      "foo": "non-array"
    }
  }
}
```

### The `storage` Library

The ideas for storage plugins described above, should be generalized into a `storage` library.
In particular the procedure described for arrays can be generalized for all bytes in the PMB-Set and provided as a function.
For example, there might a format that has a native way to mark an object as hidden (e.g. in YAML you could use a tag `!hide` on an object, instead of using the `.` prefix).

Furthermore, there is sadly still a small need for escaping that would be handled by the library.

We could for example have a keys

```
special/\\$value = x
special/key = y
```

The procedure described in the array section above would turn that into

```json
{
  "special": {
    "$value": "x",
    "key": "y"
  }
}
```

which would be read (assuming we also support the NLV-Key stuff), as:

```
special = x
special/key = y
```

To solve this, we need to know which `$` parts the plugin understands and then, on only those parts, keep the `\` prefix from the Unescaped Part.
We can't keep it on all `$` parts, because then we could no longer read all e.g. JSON files

```json
{ "$more": 1 }
```

would be illegal for example, because it should be

```json
{ "\\$more": 1 }
```

> Note: The goal was to allow storage plugins to turn arbitrary e.g. JSON files into some kind of `KeySet`.
> In some cases the structure of that `KeySet` will not quite match that of the file (e.g. `$meta` and `$value`), but that can't really be avoided, if you also want every `KeySet` to be representable.
