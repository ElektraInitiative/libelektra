# Key Names in Elektra

This document is a full explanation of how _key names_ work in Elektra.

## 0. Reference Implementation

In addition to this document, a reference Python implementation can be found in [keynames.py](../scripts/keynames.py).
The goal of the Python implementation is not to be fast, or to be used in any way other than as a reference.
If there are any discrepancies between this document, the Python implementation and the actual C implementation in [src/libs/elektra/keyname.c](../src/libs/core/keyname.c), you should consider them as follows:

1. The C implementation is optimized for speed and thus much harder to implement correctly.
2. In most cases, this document outranks the Python implementation.
   There may, however, be cases where the language in this document was too vague and the Python implementation is actually correct.
3. If two of the sources agree, the third one is probably incorrect.
   Although again, if one of the agreeing sources is the C implementation it could still be the case that there is a mistake.

In any case: If you find any discrepancies, please file a bug report at https://issues.libelektra.org/new.

> _Note:_ Mistakes happen.
> The goal of the Python implementation was to provide a reference that has a very high likelihood of being correct.

_To Elektra developers:_ Feel free to add any unclear or previous incorrect examples to the test cases in [tests/ctest/test_keyname.c](../tests/ctest/test_keyname.c).
These tests are very fast (1000+ test cases per second) and the more tests the better.

## 1. Key Name Parts and Namespaces

Before we dive into key names, we need to talk about key name parts and namespaces.

Each key is part of one of these _namespaces_:

- cascading
- meta
- spec
- proc
- dir
- user
- system
- default

Each of these namespaces has a very specific meaning, explained in [Section 1.2](#12-namespaces-and-root-keys).

Apart from the namespace, a key name is just a series of zero or more _key name parts_.
Each key name part is just an arbitrary (possibly empty) sequence of non-zero bytes.

So without knowing anything about how key names are written, we could say that there is a key in the namespace "system" with the key name parts "elektra", "version" and "info".

> _Note:_ Not every such sequence, is a valid key name.
> For more information see [Section 4](#4-valid-and-invalid-key-names)

### 1.1. Key Hierarchy

Elektra's keys commonly look like Unix paths:

```
/elektra/version/info
```

> _Note:_ How this representation works exactly and how namespaces come into play, will be explained in the next section.
> For now, we only care that there is some similarity to Unix paths.

This is on purpose.
Elektra's _key database (KDB)_ is designed to resemble a Unix filesystem as much as possible.
In particular, the KDB has a similar hierarchy.
More generally, all key names exhibit this hierarchy.
By going back to thinking about a key name as a namespace and a series of key name parts, we can define this _key hierarchy_.

Each namespace has a separate hierarchy.
The relation between these, will be explored in the next section.
For now, we just look at a single namespace.

In a Unix filesystems, we commonly talk about files and directories.
We also say a file is located within a directory.
But you might also know that in Unix "everything is a file".
This applies to directories as well, but "a file is located within a file" is a bit clunky, so you might say "file `A` is located below file `B`", if `B` is a file within the directory `A`.
What makes `A` a directory is just the fact that there can be other files below `A` [[1]](#footnote-1).
<a id="ref-footnote-1"></a>

#### 1.1.1. The "is below" Relation

This relation of "is below" is also what defines Elektra's key hierarchy.
Based on a key `K` with `n` key name parts, we say:

- A key `Km` is _below_ `K`, if `Km` has `n+m` key name parts and the first `n` key name parts of `Km` are equal to the key name parts of `K` (in the same order).
- A key `K1` is _directly below_ `K`, if `K1` is below `K` and `K1` has `n+1` key name parts.

Here are a few examples to show how this works in practice (using the Unix-path-like representation teased above):

| Key 1                   | Key 2                   | Relation                          |
| ----------------------- | ----------------------- | --------------------------------- |
| `/elektra/version/info` | `/elektra/version`      | "Key 1" is directly below "Key 2" |
| `/elektra/version/info` | `/elektra`              | "Key 1" is below "Key 2"          |
| `/elektra`              | `/elektra/version/info` | "Key 2" is below "Key 1"          |
| `/elektra/version/info` | `/elektra/version/info` | "Key 1" and "Key 2" are equal     |
| `/elektra/version/info` | `/elektra/data`         | no relation                       |
| `/elektra/data`         | `/elektra/version`      | "Key 1" and "Key 2" are siblings  |

You can think of the key hierarchy (within a single namespace) as a big tree of keys.
Each node in the tree is a single key `K` and the children of the nodes are the keys that are directly below `K`.

<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/keynames_tree.svg" alt="Tree structure of a key hierarchy" />

The diagram above shows the key hierarchy of the keys in the table above (`A` -> `B` denotes `A` is directly below `B`).

> _Note:_ While we could use the directory vs. file terminology for Elektra as well, it is recommended to avoid it.
> This is because in Elektra, every key may have an associated value.
> In particular a key may have a value, even if there are other keys below it.
> This value is **not**, as a beginner might suspect, the set of keys below it, like you could say the value of a directory is the set of files (and directories) within.
> The value is just another value, like any other key would have.
>
> Instead, we recommend the terms _leaf key_ and _non-leaf key_, as these are commonly used for tree-like structures and their definitions fit perfectly.

#### 1.1.2. The "parent" Confusion

As an inverse to "is below" we sometimes use "parent".
For example:

- `/elektra/version` is directly below `/elektra`, so `/elektra` is the (direct) parent of `/elektra/version`
- `/elektra/version/info` is below `/elektra`, so `/elektra` is a parent of `/elektra/version`
- `/elektra/version/info` and `/elektra/version` are below `/elektra`, so `/elektra` is the common parent.

This terminology can be easy to confuse, as "parent" is used for multiple different things.
If the context doesn't make it clear what "parent" means, you might consider more clear terms, e.g. other common terms for tree-like structures.
For example, you could use "ancestor" and "direct parent" as a clear differentiation.

There is also the term "(the) parent key".
In many cases, this is refers to a very specific key in the given context that is a "common parent" to a certain set of keys.
If you are working within such a context, be careful about "a parent key" vs. "the parent key".

### 1.2. Namespaces and Root Keys

We mentioned above that there are different namespaces in Elektra.
Now we will explain their meaning.

To recap, Elektra knows these namespaces:

- cascading
- meta
- spec
- proc
- dir
- user
- system
- default

We mentioned above that there are key names with zero key name parts, i.e. just namespace.
These are called _root keys_ (based on Unix's filesystem root, as well as the root of a tree).

Let us explore them one by one:

- The simplest namespace is the **"meta"**.
  The namespace "meta" is used exclusively for meta keys, i.e. keys that are attached to another key as metadata.
  The key hierarchy of the namespace "meta" is entirely separate from the key hierarchies in the other namespaces.
  Without external information, you cannot determine from a key name with namespace "meta", which key this metakey is attached to.
- keys with namespace **"proc"** only exist in memory and are scoped to the current <b>proc</b>ess.
- keys with namespace **"dir"** are scoped to a single filesystem <b>dir</b>ectory.
  They will (normally) be stored somewhere within this directory.
  Currently, there is no way of knowing which filesystem directory a key with namespace "dir" is scoped to.
  Elektra only uses keys scoped to the current working directory.
- Similarly, keys with namespace **"user"** are scoped to a single **user**.
  They will (normally) be stored somewhere within the user's home directory.
  Again, there is no way of knowing which user a key is scoped to and Elektra only uses keys scoped to the current user, i.e. the one that executed the application.
- The namespace **"system"** is what makes Elektra global.
  keys with namespace "system" are the same for all users of the system, independent of context.
  They are stored in system level directory.
- keys with namespace **"default"** are special.
  While you could create them manually, you normally don't want to.
  It is used for keys with default values for namespace resolution (explained below).
  keys with this namespace are also in-memory only.
- Then there is **"spec"**.
  This namespace, like "meta", is separate from the rest.
  We use it for keys that are part of a specification used to describe other keys.
  The metadata of every key with namespace "spec" describes a specification for the keys that have the same key name part but a different namespace (except "meta").
  So the namespace "spec" has a closer relation to the others than "meta".
- Now just the namespace **"cascading"** remains.
  This namespace is used for namespace resolution (see below).
  It is the one that applications and end-users will use most commonly when interacting with Elektra.
  keys with namespace "cascading" are never stored.
  Not on disk and normally also not in a `KeySet`.

There is also a certain ranking between the namespaces "proc", "dir", "user", "system" and "default".
Namely, that they override each other in exactly this order.
Given two key names with identical key name parts, but one with namespace "dir" and one with namespace "user", the one with namespace "dir" should be considered more specific and should be preferred.

A special feature of Elektra is _namespace resolution_.
Namespace resolution is the process of finding an appropriate namespace for a key based on a key name with namespace "cascading".
It is most commonly used, when you need to find which key in the KDB should be used, based on a series of key name parts.

To resolve the namespace, we just look at each of the namespaces in the ranking defined above.
We then use the first namespace where the key actually exists.
Namespace resolution is performed, when `ksLookup`/`ksLookupByName` is called with a key name with namespace "cascading" [[2]](#footnote-2).
<a id="ref-footnote-2"></a>
This is also done, if you call `kdb get` or `kdb set` with a key name with namespace "cascading".

## 2. Escaped Names

The standard way to represent a key name in Elektra is this:

```
system:/elektra/version/info
```

This can be deconstructed into:

- The namespace: `system`
- The namespace separator: `:`
- A part separator: `/`
- A key name part: `elektra`
- A part separator: `/`
- A key name part: `version`
- A part separator: `/`
- A key name part: `info`
- The invisible null terminator

> _Note:_ It might seem strange, that there is a part **separator** before the first part.
> This makes sense, because then the part separator always introduces a new part.
> A better fitting description would be "part introducer".
> But since we commonly call `/` a separator, we will stick to this terminology.

For keys in the namespace "cascading", we omit both the namespace itself and as well as the namespace separator:

```
/elektra/version/info
```

We also have a special rule for the root keys, i.e. the key names with zero key name parts and just a namespace.
According to above rules `system:` would be the escaped name of the root key for the namespace "system" and the escaped name for the root key of the namespace "cascading" would be an empty string.
But this is not the case.
We use `system:/` and `/` instead.
The exact details will be explored later, but for now just remember, that an (escaped) key name **always** contains at least one part separator (`/`).

But there is a problem.
We said a key name part is "an arbitrary sequence of non-zero bytes".
This means "elektra/version" is a key name part as well.
Since the slash `/` would conflict with the part separator, we can escape it with a backslash `\` (and `\` can be escaped with another `\`):

```
/elektra\/version/info/back\\slash
```

This can be deconstructed into:

- A part separator: `/`
- A key name part: `elektra/version`
- A part separator: `/`
- A key name part: `info`
- A key name part: `back\slash`
- An invisible null terminator

> _Note:_ When talking about a single key name part `/` and `\` are never escaped.

Because of this escaping mechanism, we call this the _escaped name_ of a key.

Elektra's key names are designed to mimic Unix paths to some extent.
To this end we support the commonly used `/.` and `/..`.
This is one reason, why we need to differentiate between _canonical_ key names and _non-canonical_ key names.

### 2.1. (Non-)Canonical (Escaped) Key Names

Following the syntax of Unix paths, in Elektra both `/elektra/./version` and `/elektra/version` refer to the same key.
Similarly, `/elektra/../version` and `/version` refer to the same key.

To give each key a unique key name, we need to introduce a _canonical (escaped) key name_.
For Unix paths, we could say that the canonical path is the shortest possible path that refers to a file.
In Elektra this doesn't quite work, but will use this definition for now.

> _Note:_ Only escaped key names can be canonical or non-canonical, so we normally omit the "escaped" specifier.

Let us look at a few examples to get a feeling for canonical and non-canonical key names.

| Non-canonical           | Canonical          |
| ----------------------- | ------------------ |
| `/elektra/./version`    | `/elektra/version` |
| `/elektra/../version`   | `/version`         |
| `/elektra/.././version` | `/version`         |
| `/elektra///version`    | `/elektra/version` |
| `/elektra//../version`  | `/version`         |
| `/elektra/./../version` | `/version`         |
| `/elektra/../../`       | `/`                |
| `user:/elektra/../../`  | `user:/`           |
| `/elektra/version/`     | `/elektra/version` |

As you can see, the behavior of `.` and `..` matches that of Unix paths as long as we are using the namesapce "cascading".
The namespace of a key name can never be changed via `..`, so `user:/..` is equivalent to `user:/`.

There is also a small difference in the last example.
In Unix such paths would also refer to the same file, but in some Unix tools a trailing slash alters the behavior of the tool.
In Elektra this is never the case.
`/elektra/version/` and `/elektra/version` refer to the same key and are always treated as the same key name.

The only exception are the root keys.
The canonical key names for the root keys always end with a `/`.
In fact, we will see [later](#4-valid-and-invalid-key-names), that removing the `/` makes the key name invalid.

There also is a completely new addition in Elektra.
Elektra has a notion of _array parts_.
These are key name parts that denote an array index.
How exactly these work, will be explored [later](#4-valid-and-invalid-key-names).
For now, we only need to know that they start with an `#` and their canonical form has `n` underscores followed by `n+1` digits.

A few examples for array parts:

| Non-canonical    | Canonical           |
| ---------------- | ------------------- |
| `/elektra/#10`   | `/elektra/#_10`     |
| `/elektra/#1234` | `/elektra/#___1234` |

### 2.2. Other Escape Sequences

We already know, that `/` and `\` have to be escaped in an escaped key name.
In addition to these two, there are a few more characters that have to be escaped.
However, these additional characters may **only** be escaped under certain conditions.

The characters in question are: `.`, `#`, `%`.
The conditions under which these characters can be escaped can be found [below](#4.1.-illegal-escape-sequences).

## 3. Unescaped Names

While the escaped name of a key is nice for humans, it is not very well suited for machines.
The escaping of path separators makes it hard to find the key name parts of a given key name.
The escaped name is also not well suited for sorting [[3]](#footnote-3).
<a id="ref-footnote-3"></a>

Both of these flaws are solved by the _unescaped name_.
In unescaped names we use a zero-byte as the part separator.
Since a key name part cannot contain a zero-byte, we do not need an escaping mechanism for the path separator.

However, these zero-bytes mean that the unescaped name is not a printable string and therefore not human-readable.
This why we will only describe the unescaped name in the deconstructed form.

The escaped names from above correspond to the following unescaped names:

1. `system:/elektra/version/info`
   - The byte representing the namespace "system": `0x07`
   - A part separator: `0x00`
   - A key name part: `elektra`
   - A part separator: `0x00`
   - A key name part: `version`
   - A part separator: `0x00`
   - A key name part: `info`
   - The terminator byte: `0x00`
2. `/elektra/version/info`
   - The byte representing the namespace "cascading": `0x01`
   - A part separator: `0x00`
   - A key name part: `elektra`
   - A part separator: `0x00`
   - A key name part: `version`
   - A part separator: `0x00`
   - A key name part: `info`
   - The terminator byte: `0x00`
3. `/elektra\/version\\/info`
   - The byte representing the namespace "cascading": `0x01`
   - A part separator: `0x00`
   - A key name part: `elektra/version\`
   - A part separator: `0x00`
   - A key name part: `info`
   - The terminator byte: `0x00`
4. `/`:
   - The byte representing the namespace "cascading": `0x01`
   - A part separator: `0x00`
   - The terminator byte: `0x00`

> _Note:_ We use 0xZZ to represent a single byte in hexadecimal.
> This form is only used when the context makes it clear that it represents a single byte and not a four character string.

The process of turning an escaped name into the corresponding unescaped name is called _unescaping_.
Turning an unescaped name back into an escaped name is called _escaping_.

Unescaping works, by simply removing the backslashes `\` that are used as escapes.
This applies both to `\/` and `\\` anywhere in key name parts, as well as to the escape sequences that are only used at the start of key name parts, e.g. `\#`.

## 4. Valid and Invalid Key Names

Not all of the key names described by the above sections are valid under all circumstances.
You might also say, that a key name as defined above does not necessarily refer to a key in the KDB.
So while it might be a key name, there is no key that actually uses this name.
Only _valid key names_ refer to a key in the KDB and may be used by a key as its name.
The remaining key names are referred to as _invalid key names_.

For unescaped key names, it is pretty simple:
Unescaped key names are valid key names, if all of the following are true:

- The first byte is a valid namespace byte, i.e. `0x01` - `0x08`.
- The second byte is a zero-byte `0x00`.
- The last byte is a zero-byte `0x00`.

For this reason unescaped names for root keys are 3 bytes long, with the last two bytes being zero-byte.
Between the second and the last byte, we find the key name parts separated by a zero-byte `0x00`.

For escaped key names it is easier to define, what makes an invalid key name, than what makes a valid key name, so we will go this route.
An escaped key name is considered an invalid key name, if any of the following are true:

- It is an empty string.
- The last character before the invisible null terminator is a backslash `\`.
  This is a dangling escape as we expect another character after the escape character `\`.
- It contains a namespace (i.e. the namespace is not "cascading"), but the namespace separator `:` is not followed by a `/`.
  (This mainly applies to root keys.)
- It contains a namespace separator `:`, but the substring before the first `:` is not one of: `meta`, `spec`, `proc`, `dir`, `user`, `system` and `default`.
- It contains an illegal escape sequence (see below).
- It is the string `/%` or consists of a namespace followed by the namespace separator `:` followed by `/%`.
  In other words, the first escaped part is translated into an empty unescaped part.
  The unescaped names for these keys would collide with the root keys `/`, `user:/`, etc.

> _Note:_ The C-API does not allow you to construct a `Key` with an invalid key name; for example `keyNew` (and `keyVNew`) will return `NULL`.

### 4.1. Illegal Escape Sequences

The escape sequences `\\` and `\/` are always valid.
For the other escape sequences certain conditions must be fulfilled:

- `\.`: can be used at the start of a key name part, if the whole key name part is `\.` or `\..`.
  In other words, `\` can be used to escape the behavior of `.` and `..`.
- `\#`: can be used at the start of a key name part, if the key name part would be a non-canonical array part without the `\`.
  specifically, `\#` can be used, if the key name part matches the regular expression `\\#[1-9][0-9]+` and the digits form a number less than or equal to 9223372036854775807 (= `2^63 - 1`).
  Meaning, `\` can be used to avoid array part canonicalization.
- `\%`: can be used, if the whole key name part is `\%`.
  That is `\%` is the escaped version of `%` (the empty key name part).

It may seem weird that some escape sequences have such specific requirements.
This is necessary to create a 1:1 mapping between (canonical) escaped names and unescaped names.
Without the restrictions, e.g. both `\%abc` and `%abc` would be unescaped as `%abc`.
In addition, the conditions for `\#` have to be so specific, because `\` must only be allowed, if it affects unescaping.
For example, we cannot allow `\#abc`, because that would unescape into `#abc`, just like `#abc`.

### 4.2. Array Parts

As mentioned above, Elektra has a notion of array parts.
More specifically, certain key name parts will be interpreted as array indices under certain circumstances (see also [documentation for arrays]()).

<!-- TODO: link to detailed array documentation -->

We already mentioned above, that array parts have canonical and non-canonical forms.

A canonical array part is a hash-sign `#` followed by `n` underscores (`_`) followed by `n+1` digits.
Additionally, the digits must form a number greater than or equal to `0` and less than or equal to 9223372036854775807 (= `2^63 - 1`).
The number must not have any leading `0`s (except for the number zero itself).

In non-canonical key names, the underscores (`_`) are omitted.
That is, either the correct number of underscores is used or no underscores at all.

Any other key name part that starts with a `#` is never an array part.

**All** key name parts starting with `#` are valid.
It does not matter, if the key name part is an array part or not.
However, some parts of Elektra may expect array parts under certain circumstances.
Providing other key name parts under such circumstances, may cause problems.
If the context doesn't call for an array part, then array parts behave no different and are treated as plain string just like any other key name part.

Finally, some examples:

<a id="ref-footnote-4"></a>

| Key name part | Behavior in array                 | Behavior elsewhere   |
| ------------- | --------------------------------- | -------------------- |
| `#0`          | Index of first element            | Child named `#0`     |
| `#_10`        | Index of 11th element             | Child named `#_10`   |
| `#_99`        | Index of 100th element            | Child named `#_99`   |
| `#__100`      | Index of 101st element            | Child named `#__100` |
| `#abc`        | Possible error [[4]](#footnote-4) | Child named `#abc`   |
| `#_100`       | Possible error [[4]](#footnote-4) | Child named `#_100`  |
| `#__10`       | Possible error [[4]](#footnote-4) | Child named `#__10`  |
| `#10a`        | Possible error [[4]](#footnote-4) | Child named `#10a`   |

## 4.3. Reserved Key Names

Apart from invalid key names, which cannot be constructed via the C-API, there are also _reserved key names_.
These can be used with the C-API (`keyNew` returns a valid `Key *`), but there might be situations, in which `Key`s with such key names are treated differently or rejected entirely.

Generally speaking, any part of Elektra may define that some key names have special meaning, are not allowed, etc.
However, sometimes guaranteed compatibility with other parts of Elektra is required.
A good example are storage plugins.
A storage plugins should strive to be compatible with as much of Elektra as possible.
But since the storage plugin doesn't know anything about other plugins or even the application using Elektra, it is hard to attribute special meaning to certain key names.

This is why there are two types of reserved key name:

1. Any key name that is below `system:/elektra`:
   These key names are reserved for Elektra's internals.
   Each of these keys has a very specific purpose that is defined globally for all of Elektra.
   Using such a key name automatically carries this meaning.
   Even outside the context in which Elektra uses these directly, you should never use `system:/elektra` keys for other purposes.
2. Any key name containing the key name part `®elektra`:
   These key names are reserved, but their meaning depends on the context.
   Similar to the [METADATA.ini](METADATA.ini) file for metadata, some conventions for these key names are defined in [reserved name document]().

   > _Note:_ We use UTF-8 here, so `®elektra` is specifically the 9-byte sequence `C2 AE 65 6C 65 6B 74 72 61`.

   `Key`s with such key names will _never_ be used in the interface between storage plugins and the rest of Elektra.
   This allows storage plugins to use `®elektra` to encode things that otherwise wouldn't be possible (e.g. values of non-leaf keys).

<!-- TODO: link to detailed ®elektra document -->

---

<a id="footnote-1">[1]:</a>
This explanation of Unix paths and Unix filesystems, is not entirely accurate.
But it is good enough for our purposes, so we will just ignore some details.
[↑](#ref-footnote-1)

<a id="footnote-2">[2]:</a>
The actual process of resolution process that happens in `ksLookupByName` and `ksLookup` is a bit more complicated.
It may involve some keys with namespace "spec" as well.
[↑](#ref-footnote-2)

<a id="footnote-3">[3]:</a>
For performance reasons, we want to make the comparison between to key names as fast as possible.
A good solution is a single `memcmp`.
But this doesn't account for the fact that key names represent a hierarchy and that `/` has a special meaning:
`/key/sub` should always be sorted after `/key` and before `/key.1`.
With `memcmp`, `/key` is first, because it is the shortest and otherwise equal.
But then we would get `/key.1` not `/key/sub`, because `/ < .` in ASCII.
This cannot happen with zero-bytes as the separator, because there is no byte with a smaller value.
[↑](#ref-footnote-3)

<a id="footnote-4">[4]:</a>
We classify this as a "possible error", because not all parts of Elektra will fully validate all conventions and rules around arrays.
In particular, a standard `KeySet` itself imposes no restrictions.
Therefore, you can use any `KeySet` locally in your application.
As soon as you pass the `KeySet` into an external function (e.g. `kdbSet`), however, errors may occur since the `KeySet` may pass through a function that requires `KeySet` which are properly validated against array rules.
For more details what conventions and rules exist around arrays, see [documentation for arrays](). [↑](#ref-footnote-4)

<!-- TODO: link to detailed array documentation -->
