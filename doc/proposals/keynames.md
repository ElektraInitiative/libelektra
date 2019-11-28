# Future-proof 1:1 Relation for Key Names

## Basic idea

Not all byte sequences are unescaped key names anymore.
Some C strings are no longer valid unescaped key name parts.

This makes a future proof 1:1 relation of escaped and unescaped key names possible.
Strings may become valid unescaped key parts, but once a string is valid it cannot become invalid anymore.

## Semi-formal Explanation

### Definitions

- For strings `s` I will use pythons indexing, e.g. `s[0]` is the first character, `s[1]` the second and `s[1:]` is everything but the first character.
- `c:s` is the string `s` prefixed with the character `c`.
- `R` is the set of reserved characters.
- `\` must be part of `R`, but `/` must not be.
- For all `r` in `R` the following holds:
  - `I{r}` is a bijection between two sets of strings
  - If `I{r}(x) = y` then `r = x[0] = y[0]`. `I{r}(x)` is undefined, if `r != x[0]`.
- `I{\}(e) := e`
- `I(e)` is defined as `I{e[0]}(e)`, if `e[0]` is in `R`.
  Otherwise `I(x) = x`.
  (`I` interprets the escaped part `e` and returns its unescaped form.)
- `J` is the inverse function of `I`, i.e. `J(u) := e`, if and only if `I(e) = u`.
- `e` is _e-part-valid_, if and only if `e[0]` is in `R` implies that there is a `u` such that `I(e) = u`.
- `u` is _u-part-valid_, if and only if `u[0]` is in `R` implies that there is an `e` such that `I(e) = u`.

I ignore the (un)escaping of `/` and `\` here, as it's pretty clear how it works.

### Operations

The following operations can be used to construct a key name from parts and to inspect the last part of the key name.
This is enough for theoretical reasoning, things like `keySetName` can just be thought of as splitting the argument into parts and using the operations below, even if it should never be implemented like that.

If an operation is "undefined", it means we have an error; an invalid key name.

- `push(k, e, u)`: adds the escaped part `e` and the unescaped part `u` to the key `k`.
  This cannot be used directly.
  It is only used to define the other operations.
- `lastE(k)`: returns the last escaped part of the key `k`.
- `lastU(k)`: returns the last unescaped part of key `k`.
- `pushE(k, e)`: adds an escaped part `e` to the key `k`.
  Defined as:
  - If `e` is _e-part-valid_ then `pushE(k, e) := push (k, e, I(e))`.
  - Otherwise `pushE(k, e)` is undefined.
- `pushU(k, u)`: adds an unescaped part `u` to the key `k`.
  Defined as:
  - If `u` is _u-part-valid_ then `pushU(k, u) := push (k, J(u), u)`.
  - Otherwise `pushU(k, u)` is undefined.
- `pushL(k, s)`: helper for adding a "literal" part `s` to the key `k`.
  This can be used to avoid a part having special meaning.
  Defined as:
  - If `s[0]` is in `R` then `pushL(k, s) := pushU (k, \:s)`.
  - Otherwise `pushL(k, s) := push (s, s)`.
- `putE(k, e)`, `putU(k, u)` and `putL(k, s)` are the same as removing the last part of `k` and then calling `pushE(k, e)`, `pushU(k, u)` and `pushL(k, s)` respectively.

### Storage Plugins

Storage plugins for flat formats (without a native nesting hierarchy) should just use the `keyName` to store values (and metadata, if possible).

Storage plugins with support for a native hierarchy should use the operations `store` and `load` to find the names to use in that hierarchy (e.g. field names in JSON).

`S` is a subset of `R`.
It is the set of reserved characters, whose meaning has a native representation in the plugins storage format.
e.g. `#` is in `S`, if the storage format has native arrays.
`\` must not be in `S`.

The following operations would be provided in a separate library for storage plugins:

- `store(S, u)`: turns an unescaped part `u` into a name the storage plugin should use.
  Defined as:
  - If `u[0] = \` and `u[1]` is in `S` then `store(S, u) := u[1:]`
  - If `u[0] = \` and `u[1]` is not in `S` then `store(S, u) := u`.
    (NB:`u[1]` must be in `R` because `u` is valid).
  - If `u[0]` is in `S` then `store (S, u) := NULL`.
    Indicates to the plugin that it should do its native handling.
  - Otherwise `store(S, u) := u`.
- `load(k, S, n)`: adds an unescaped part corresponding to the name `n` (produced by `store`) to the key `k`.
  Defined as:
  - If `n[0]` is in `S` then `load(k, S, n) := pushL(k, n)`.
  - Otherwise `load(k, S, n) := pushU(k, n)`.

#### Directory values

Directory keys with values can be handled much more cleanly.

I will use `\value` in the example, but any other invalid unescaped key part whose first character is not in `S` can also be used.

I will use JSON as an example, it requires that a `\` is escaped as `\\`.

##### Store procedure

If there are keys below the key `k`, store the value of `k` in the nested field `\value`:

```json
"k": {
    "\\value": "value of k",
    ...
}
```

Otherwise, store the value directly.

```json
"k": "value of k"
```

If `k` is an array parent, I recommend introducing a pseudo-level in the hierarchy (other ways might work to).
In JSON for example:

```json
"k": {
    "\\value": "value of k",
    "\\array": [ ... ]
}
```

The benefit of this solution is, it also works for mixed array and non-array parents.
However, I wouldn't generally recommend using such structures.

##### Load procedure

If we encounter a field named `\value`, we store its value not in `p/\value` (which is invalid anyway), but instead in the parent key `p`.

If we encounter the `\array` field from above, we ignore it when constructing names for the elements of the array.
We don't construct `p/\array/#1` (again invalid), but `p/#1`.

#### Metadata

A similar construction to the `\array` handling above, but instead with a nested `\meta` object can be used for metadata as well.

For example a directory key with value and metadata in JSON:

```json
{
"key": {
    "\\value": "value of key",
    "\\meta": {
        "type": "string"
    },
    "subkey": 1
}
```

Or an array parent with value and metadata:

```json
{
"array": {
    "\\value": "value of key",
    "\\meta": {
        "type": "string"
    },
    "\\array": [
        "element1",
        {
             "element2": 2
        }
    ]
}
```

These would correspond to the `ni` format files:

```ini
key = value of key
key/subkey = 1

[key]
type = string
```

and

```ini
key = value of key
key/#1 = element1
key/#2/element2 = 2

[key]
type = string
```

## Results

1. `putE(k, lastE(k))` is a no-op on `k`.
2. `putU(k, lastU(k)` is a no-op on `k`.
3. `putL(k, lastU(k)[1:])` is a no-op on `k`, if `lastU(k)[0] = \`.
4. `store` and `load` allow round-tripping through deconstruction/re-construction in storage plugins.
5. Arbitrary unescaped key parts are no longer possible.
6. However, adding arbitrary strings in an encoded form as unescaped key parts is still possible.
7. Storage plugins can still accept (almost) arbitrary names in their input files.
8. The rules for names in storage files are easy.
   If you want name `n` you do:
   - If `n[0]` in `R` but not in `S`, use `\:n`, unless you want the name to have the meaning defined by `n[0]`, in which case the name MUST be valid to that interpretation.
   - Otherwise, use `n` directly.
9. Applications looking for an arbitrary name e.g. part of the name is an SSID, should construct a lookup key in a way that all arbitrary parts that should have no meaning are added with `pushL`.
10. If a `ksLookup` is not possible or wanted and an application instead wants to compare unescaped key parts against a string e.g. SSID, a leading `\` should be ignored in the comparison.

## Proofs

Proofs for some of the results above.

1. Since the second argument to `putE` comes from `lastE`, we know that it is _e-part-valid_.
   We also know that for each `e` there is only one `u` such that `u = I(e)`.
   Wich means `I(lastE(k)) = lastU(k)`.
   In total: `putE(k, lastE(k)) = put(k, lastE(k), lastU(k))`, where `put` removes the last part of the escaped and the last part of the unescaped name and then calls `push`.
   This is obviously a no-op on `k`.
2. We know that for each `u` there is only one `e` such that `u = I(e)`.
   The rest of the proof is analogous to 1.
3. Since `lastU(k)` is always _u-part-valid_, `lastU(k)[0] = \` implies that `lastU(k)[1]` is in `R`.
   Therefore `putL(k, lastU(k)[1:])` falls into the first case of `pushL` and we have `putL(k, lastU(k)[1:]) = putU(k, \:lastU(k)[1:]) = putU(k, lastU(k))`.
   We already proved that this is a no-op on `k`.
4. We look at an arbitray unescaped part `u` of the key `k` under the assumption that `pushU(k1, u)` turns `k1` into `k`.
   In other words, `k1` is the last key we wrote to the file, `k` is the next key we want to write and `u` is what needs to be stored.
   Let `n` be the string we use as the next field name.
   If `u[0]` is not in `S`, we have `store(S, u) != NULL` and define `n := store(S, u)`.
   Otherwise we are not writing a normal field and have to do some special handling (e.g. write array).
   When reading back the file we find a field name `m`.
   We know this is a normal field written with a name from `store`, otherwise we would do some special handling (e.g. parsing array).
   Therefore, `m = store(S, v)` for some `v`.
   We have two cases:
   - `m[0]` is in `R` (but not in `S`): By definition of `store` this means `v = \:m`.
   - `m[0]` is not in `R`: By definition of `store`, `v = m`.
     Looking at the definitions of `load` and `pushL`, we see that: `v = u` and therefore `load(k1, m) = load(k1, store(S, u)) = pushU(k1, u)`.
     This means we can reconstruct `k1` to be the same as `k` after desconstructing it and writing it to a file.
5. e.g. `#abc` and `\abc` are invalid
6. `\#abc` and `\\abc` are valid
7. Only those names that start with a character `c` in `R` but not in `S`, cannot be accepted. For example most plugins won't accept `\abc`; those that handle arrays will accept `#abc` and `\#abc`.

## Notes

- A note on parts starting with `\`:
  If I want an unescaped part that is interpreted as actually starting with `\`, I use an escaped part starting with `\\` which will produce an unescaped part also starting with `\\`.
  As stated before, the first of these `\` should be ignored in comparisions, so the actual interpretation of the part only starts at the second `\`.
  In the rest of the unescaped part, all `\` are to be taken literally.
- A `lastL` can be added that takes care of ignoring the `\` for comparisons.
  However, I would recommend a different name or a VERY clear warning that there is no general round-tripping for `pushL` and `lastL`, i.e. `putL(k, lastL(k))` is not generally a no-op on `k`.
  If `lastL(k)[0]` is in `R` the meaning of `lastL(k)` will be removed.
- I intentionally chose new names based on `push` and `last`.
  This was to avoid any confusion or assumptions related to the existing `BaseName` terminology.
  I would even recommend, that we remove the `BaseName` functions and choose new names, if we go with this proposal.
  This would ensure every user of Elektra looks at their code and hopefully thinks about which of the new operations replaces the `BaseName` function in their use case.
- Implementing this will be some effort.
  It should probably be done in a follow-up to #3115, since there are only small differences in which escaped key names are valid compared to the current state of #3115.
  The bulk of the current PR deals with the change to namespaces anyway.
- A note on the directory value stuff:
  Since we intentionally use names which are not valid key parts, this solution obviously cannot be a separate plugin.
  However, I this is not a problem.
  During iteration of a KeySet it is trivial to check, whether the current key is a directory key.
  Just use `keyIsBelow (ksAt(i), ksAt(i+1))`.
  Detecting array parents is also a non-issue with the above solution, since the `\array` pseudo-field can be created when ever we hit the first array element.
  It don't actually care whether a key is an array-parent, just whether are any keys below.
  Lastly, detecting array elements is also trivial, since only valid array elements will start with a `#`.
