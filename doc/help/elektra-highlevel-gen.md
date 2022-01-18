# elektra-highlevel-gen(7) -- High-level API code-generation advanced features

This document focuses on the advanced features of the high-level API code-generator template. We assume you already familiarized yourself
with the basic features explained in [`kdb-gen-highlevel(1)`](kdb-gen-highlevel.md).

## Configuration Options

The parameters that are relevant to the concepts described here are (for the rest see [`kdb-gen-highlevel(1)](kdb-gen-highlevel.md)):

- `embeddedSpec`: allowed values: `full` (default), `defaults`, `none`
- `enumConv`: allowed values: `strcmp`, `switch`, `auto` (default)

Using `embeddedSpec` you can configure how much of the specification is embedded into your application. By default we use `full`. This means
the full specification is embedded into your application's binary. Since this can drastically increase the size of the binary, you can also
choose `defaults` or `none`. The `defaults` setting embeds a reduced version of the specification, which only contains the metadata required
by `elektraOpen`. By setting `embeddedSpec=none` you can also remove this reduced specification.

The advantage of using `full` is that your application is contained in a single executable file. If you don't use `full`, the code-generator
produces an additional `.spec.eqd` file and omits specload function (called `exitForSpecload` by default). This file contains the full
specification in quickdump format. You can either mount it directly via `quickdump`, or if you want the features of `specload` use a
`specload` configuration like this: `app=/usr/bin/cat args=#0 args/#0="path-to-spec-output-file"`.

Setting `embeddedSpec=none` is only recommended, if you must have the minimal binary size and you know what you are doing. In this case no
defaults are passed to `elektraOpen` and defaults are only handled via the `spec` plugin. If the specification/configuration isn't mounted,
the getter functions may fail.

The case of a misconfigured mountpoint will be detected automatically and reported as an error. It will
cause the initialization function (by default named `loadConfiguration`) to fail, if the specification is not mounted at the expected mountpoint or if the specification was not `spec-mount`ed.

## Enums

We support the mapping of a set of string values to a native C `enum`. To use this feature, you need to write your specification the same
way that the enum part of the `type` plugin expects.

```ini
[myenum]
type=enum
check/enum=#_4
check/enum/#0=none
check/enum/#1=red
check/enum/#2=green
check/enum/#4=blue
default=blue
```

The above specification will generate the following C `enum`:

```c
typedef enum
{
    ELEKTRA_ENUM_MYENUM_NONE = 0,
    ELEKTRA_ENUM_MYENUM_RED = 1,
    ELEKTRA_ENUM_MYENUM_GREEN = 2,
    ELEKTRA_ENUM_MYENUM_BLUE = 4,
} ElektraEnumMyenum;
```

As you can see the integer values of the different enum values are taken from the indices of the `check/enum/#` array. You may also use
e.g. `gen/enum/#2/value=1 << 1` to set a different value. The `gen/enum/#/value` values are inserted literally into the C files, so the
values must be valid C code. The name of the enum may be configured via `gen/enum/type`. If you want to use an existing `enum` and map its
values to strings you can turn the generation of the enum off, by adding `gen/enum/create=0`. In this case you have to add a header that
defines the `enum` or `typedef`s it, to the `headers` parameter of the code-generator invocation.

Like with any other key, the code-generator produces `static inline` getter and setter functions for the key. Since there are no generic
functions for the conversion of the strings into the enum values, we also generate those:

```c
ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_TO_CONST_STRING_SIGNATURE (ElektraEnumMyenum, EnumMyenum);

ELEKTRA_GET_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_SET_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
```

These functions are not generated per key, but per enum type. If multiple keys use the same enum type (both need to define the full metadata,
including the full set of values), we only generate one set of these functions.

The getter and setter functions won't be explained here, they work like any of the other getter and setter functions of the high-level API.

The other three functions are used to convert between the string values and the generated `enum`. You may find these useful in your
application. You can call them via e.g. `ELEKTRA_KEY_TO (EnumMyenum)`. The difference between `ELEKTRA_TO_STRING` and
`ELEKTRA_TO_CONST_STRING` is that the first returns a `char *` allocated via `elektraMalloc`, while the second returns a static
`constchar *`.

Both `ELEKTRA_TO_STRING` and `ELEKTRA_TO_CONST_STRING` are always implemented via a straightforward `switch` statement. The implementation
of `ELEKTRA_KEY_TO` on the other hand can be changed via the `enumConv` parameter. If you set `enumConv=strcmp`, we will generate a code
analogous to:

```c
if (strcmp (string, "none") == 0) { /* ... */ }
if (strcmp (string, "red") == 0) { /* ... */ }
if (strcmp (string, "green") == 0) { /* ... */ }
if (strcmp (string, "blue") == 0) { /* ... */ }
```

This code is not really optimal, since we really only need to look at the first character to determine the correct enum value. This is where
`enumConv=switch` comes in. With this option, we generate a series of (nested, if necessary) switch/case statements:

```c
switch (string[0])
{
case 'b': /* blue */
case 'g': /* green */
case 'n': /* none */
case 'r': /* red */
}
```

Of course this version also has its own problems. Take for example the enum with the values: `blue`, `blueish` and `brown`. With
`enumConv=switch` this would generate the following code:

```c
switch (string[0])
{
case 'b':
    switch (string[1])
    {
    case 'l':
        switch (string[2])
        {
        case 'u':
            switch (string[3])
            {
            case 'e':
                switch (string[4])
                {
                case 'i': /* blueish */
                }
                /* blue */
            }
            break;
        }
        break;
        case 'r': /* brown */
    }
    break;
}
```

This is already quite hard to read and `blueish` isn't even that long.

To provide a compromise between readability and performance, we default to `enumConv=auto`. This options uses the switch version, if the
depth is less than 3, and the `strcmp` version in all other cases. A depth of `n` means looking at the first `n` characters
`string[0], string[1], ..., string[n-1]`. In other words a depth of `n` uses `n` switch statements.

## Structs

The `highlevel` template also has support for structs. By setting `type = struct` on a key, you can enable the generation of a native C
`struct` for the keys below it.

We will look at this simple example:

```ini
[mystruct]
type=struct
check/type=any
default=""

[mystruct/a]
type=string
default=""

[mystruct/b]
type=long
default=8
```

Note: That we set `check/type=any` and `default=""`. This is to avoid problems with the `type` plugin, which doesn't know about `struct`s.

The generated struct looks like this:

```c
typedef struct ElektraStructMystruct
{
    const char * a;
    kdb_long_t b;
} ElektraStructMystruct;
```

Similar to enums, you can customise the generated struct via additional metadata:

- Metadata for the key with `type=struct`:
  - `gen/struct/type` can be used to set the name of the generated struct.
  - `gen/struct/create=0` disables the struct generation and only generates the accessor functions. Use this to use structs defined elsewhere.
    Don't forget to include the needed header in the `headers` parameter.
  - `gen/struct/alloc` (values `0`, `1`) sets whether the struct is _allocating_. This changes how the getter works and also has some other
    implications. By default structs are non-allocating.
  - `gen/struct/depth` sets at how many levels below the `type=struct` key, we will include in the generated struct. Note that keys ending
    in `/#` (i.e. array keys) count as one level above. So `mystruct/x/#` would be included with the default `gen/struct/depth=1`.
- Metadata for keys corresponding to fields of the struct:
  - `gen/struct/field` sets the name of the field in the generated struct.
  - `gen/struct/field/ignore=1` ignores this key during struct generation, i.e. we don't create a field for it.
  - `gen/array/sizefield` sets the name of the field used to store the size of arrays. Only useful on array keys. For example, by default
    the size of the array key `mystruct/x/#` is stored in `xSize`, while the array is accessed via the field `x`.

We will also generate getter and setter functions:

```c
ELEKTRA_GET_SIGNATURE (ElektraStructMystruct *, StructMystruct);
// or ELEKTRA_GET_OUT_PTR_SIGNATURE (ElektraStructMystruct, StructMystruct);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraStructMystruct *, StructMystruct);
// or ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE (ElektraStructMystruct, StructMystruct);

ELEKTRA_SET_SIGNATURE (const ElektraStructMystruct *, StructMystruct);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const ElektraStructMystruct *, StructMystruct);
```

The difference between `ELEKTRA_GET_SIGNATURE` and `ELEKTRA_GET_OUT_PTR_SIGNATURE` is explained in the next section. Both versions are called
via `ELEKTRA_GET (...) (...)`.

Allocating structs also generate `ELEKTRA_STRUCT_FREE (/* struct name */)`, which is used to free the allocated memory.

### Allocating vs. Non-Allocating

The main difference between allocating and non-allocating structs, is how their getter function works.

Allocating structs use a getter similar to the one primitive types, strings and enums use. It returns a pointer to a newly allocated struct,
which has to be freed using the generated `ELEKTRA_STRUCT_FREE` function.

Non-allocating structs meanwhile use a different kind of getter declared via `ELEKTRA_GET_OUT_PTR_SIGNATURE` instead of
`ELEKTRA_GET_SIGNATURE`. This version doesn't return a pointer, instead it takes a pointer to an existing struct and only sets its fields.
This is why you have to use the convenience macros `elektraFillStruct` and `elektraFillStructV` for these structs.

Non-allocating structs are also more limited than their allocating counterparts. They do not support arrays or struct references. They also
cannot be for unions. Their main advantage is that you can use non-allocating structs without (additional) `malloc`/`free`, by providing a
stack allocated pointer to the getter function.

### Struct references

Structs cannot be nested, but they can reference each other. This allows for complex and possibly recursive structures. Take for example:

```ini
[person/#]
type=struct
check/type=any
default=""
gen/struct/alloc=1

[person/#/name]
type=string
default=Max

[person/#/mother]
type=struct_ref
check/type=any
default=""
check/reference=recursive
check/reference/restrict=../../../person/#

[person/#/children/#]
type=struct_ref
check/type=any
default=""

[person/#/children]
default=""
check/reference=recursive
check/reference/restrict=../../../person/#
```

This results in a struct like this:

```c
typedef struct ElektraStructPerson
{
    struct ElektraStructPerson * mother;
	kdb_long_long_t childrenSize;
	struct ElektraStructPerson ** children;
	const char * name;
} ElektraStructPerson;
```

As you can see an instance of `ElektraStructPerson` may reference different instances. To declare this we must add a key with
`type=struct_ref`. We use the metakeys of the `reference` plugin (which should be mounted to validate reference) to define what struct we
want to reference. We also again set `check/type=any` and `default=""` to please the `type` plugin.

Struct references are also supported as arrays, in which case the `check/reference` keys must be on a different key than the rest of the
metadata, because of how the `reference` plugin works. The example above shows this with `person/#/children` and `person/#/children/#`.

If you access an element of the `person/#` array via the getter function, we will recursively read the references structs. Writing structs
that contain struct references or setting `struct_ref` keys directly is not supported.

Struct references can also exist outside of structs and maybe accessed directly via the generated accessor functions. Please, be careful
when handling struct references, since invalid references will cause fatal errors.

## Unions

The most advanced feature of the code-generator are unions. Sometimes we want a reference inside a struct, but it is not always to the same
struct. For example in a menu structure, we might have a list of entries that are either submenus or actual items that execute a command.

```c
[menu/#]
type=struct
check/type=any
default=""
gen/struct/alloc=1

[menu/#/name]
type=string
default=""

[menu/#/entries/#]
type=struct_ref
check/type=any
default=""
gen/reference/discriminator/enum = MenuEntryType
gen/reference/discriminator/union = MenuEntry
gen/reference/restrict/#0/discriminator = item
gen/reference/restrict/#1/discriminator = menu

[menu/#/entries]
default=""
check/reference=recursive
check/reference/restrict=#1
check/reference/restrict/#0=@/menu/#
check/reference/restrict/#1=@/item/#

[menu/#/discriminator]
type = discriminator
check/type = enum
check/enum = #1
check/enum/#0 = item
check/enum/#1 = menu
gen/enum/type=MenuEntryType
default = menu

[item/#]
type=struct
check/type=any
default=""
gen/struct/alloc=1

[item/#/name]
type=string
default=""

[item/#/command]
type=string
default=""

[item/#/entries]
check/reference/restrict=

[item/#/discriminator]
type = discriminator
check/type = enum
check/enum = #1
check/enum/#0 = item
check/enum/#1 = menu
gen/enum/type=MenuEntryType
default = item
```

As you can see the unions feature requires quite a bit more setup. We will start with `menu/#/entries/#`. It is set to `type=struct_ref`
like you would do for normal struct reference, but the accompanying `menu/#/entries` uses `check/reference/restrict` as an array. This tells
the `reference` plugin that any of the given reference restrictions are allowed. Therefore we could be referencing one of several structs
and the code-generator has to deal with that somehow.

To allow alternative references, we need to define `gen/reference/discriminator/union` and `gen/reference/discriminator/enum` on the key
with `type=struct_ref`. The former of these defines the name of the native C `union` the code-generator creates:

```c
typedef union {
    struct ElektraStructMenu * item;
    struct ElektraStructMenu * menu;
} MenuEntry;
```

The other required metakey defines which enum shall be used as a discriminator between the union values:

```c
typedef enum {
    ELEKTRA_ENUM_MENU_ENTRY_TYPE_ITEM = 0,
    ELEKTRA_ENUM_MENU_ENTRY_TYPE_MENU = 1
} MenuEntryType;
```

Each of the possibly referenced structs must have a discriminator key. This key must be part of the struct, it must have
`type=discriminator` and should have `check/type=enum`. All the discriminator keys must also set `gen/enum/type` to the same value as chosen
for `gen/reference/discriminator/enum` and all of them have to define the same enum, via the `check/enum/#` array.
The values also have to match the values of the `gen/reference/restrict/#/discriminator` metakeys on the `type=struct_ref` key.

The generated structs will then look like this:

```c
typedef struct Menu
{
	const char * name;
	kdb_long_long_t entriesSize;
	MenuEntryType * entryTypes;
	MenuEntry * entries;
} Menu;

typedef struct Item
{
	const char * name;
	const cahr * command;
} Menu;
```

As you can see the discriminator field is excluded from the struct itself and stored in a separate array. We do generate getter and free
functions for unions, but we don't recommend using them directly. There are no setter functions for unions, because they involve struct
references.
