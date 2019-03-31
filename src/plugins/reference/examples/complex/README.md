# Example: Validating Complex Recursive Structures

Suppose you have some mutually recursive `struct`s in C. You want to map this structure
onto a hierarchy inside your KDB. This by itself was always possible, but the reference
plugin in combination with the spec plugin, now allows for this in a way that can be
validated and checked by Elektra.

The structure we will use for this example is easily defined in C code:

```c
struct typeA {
  char *name;
  struct typeB *ref;
};

struct typeB {
  long int id;
  struct typeA *ref;
};

struct typeA *rootkey_ref;
```

Starting with a reference to an element of type `typeA`, we except an alternating chain of
`typeA` and `typeB`. In this chain each element of type `typeA` shall have a string attached
to it, while elements of type `typeB` shall contain an integer.

The specification used for such a structure is as follows (using the syntax of the `ni` plugin):

```ini
[rootkey/ref]
check/reference = recursive
check/reference/restrict = ../typeA/_

[typeA/_]
default = ""

[typeA/_/name]
check/type = string

[typeA/_/ref]
check/reference/restrict = ../../typeB/_

[typeB/_]
default = ""

[typeB/_/id]
check/type = long

[typeB/_/ref]
check/reference/restrict = ../../typeA/_
```

The basic idea is to use the keys `typeA` and `typeB` as a sort of 'directory', in which
every 'file' represent an element of type `typeA` or type `typeB` respectively.

To achieve our goals of type validation, we specify `rootkey/ref` to be the root of our
reference graph, but also restrict the possible reference to keys directly below `typeA`.
That way we ensure that any reference set in `rootkey/ref` will refer to a key which is,
validated to be compatible with our `struct typeA` by the spec and type plugins.

Using the spec plugin we then specify all direct children of `typeA` to have a default value
of `""`. This ensures that these keys exist and therefore can be referenced. [¹](#note-empty-val)
Then we simply specify the `name` sub-key for all of these children to be a string, and restrict
the possible reference to direct children of `typeB`. From there we proceed similarly for the
children of `typeB` and we are done.

---

<a id="note-empty-val">¹</a> It would actually be better to only give these keys a value,
if the `name` sub-key exists, but there is currently no good way to do that in Elektra. An
alternative would be to use the keys themselves, i.e. give them an actual value.
