- infos = Information about the conditionals plugin is in keys below
- infos/author = TBD
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/recommends =
- infos/placements = pregetstorage
- infos/status = idea
- infos/metadata =
- infos/description = the plugin calculates the default value of a given key if no value is present

## Introduction

The plugin is used to calculate default values if no specific value was given.

## Description

The plugin can be used to calculate or use values which it fetches from others keys.
Basically the functionality can be splitted into three components:

1. Getting the value of other keys
2. Arithmetic expressions
3. Conditional expressions

Those three components can be used alone or together such as fetching the value
of 2 keys, adding them up and wrapping it in a conditional block.

Returning a specific value for the current key is done via the `return`
statement. For some examples look into the examples section.

The following sections give more detailed descriptions of all components.

### 1. Getting the value of other keys

The syntax for fetching the value of another key is simply either

- relative upwards: eg. ../upper/key
- relative downwards: eg. ./deeper/key
- relative to the parent: eg. @/some/key
- absolute: eg. /some/path/some/key

### 2. Arithmetic expressions

Arithmetic expressions are limited to `+ - / * %` and can be keys or constants.
So for example if you want to add up two keys you could do it like this:

```
../key/A + ../key/B  #via keys
../key/A + 5             #key and constant
```

### 3. Conditional expressions

The basic syntax of conditionals are
`if(expression){BLOCK}`, `else if(expression){BLOCK}`, `else{BLOCK}` statements.

`expression` has to be a valid condition in the form of

`Key` *Operation* `('String' | constant | Key)`

where *Operation* stands for
`!=, ==, <, <=, =>, >` which are comparisons as in C.

A `BLOCK` can either be

1. a nested conditional expression
2. a `return` statement which can either be another key, constant or
arithmetic expression

#### Testing if Key exists
`(! ../key/B)` evaluates to true if the key `../key/A` doesn't exist, to false if it exists.

## Open Questions

Imagine the following case:
```
Key A = int
Key B = int
Key C = A + B
```
Every time the user uses `kdb get` on C the value gets calculated.
Now a user unintentionally assigns a String to Key A.

When calling `kdb get` the default value calculation will inevitably fail
and cannot return any value at all.
I see 3 remedies in this case:

1. Assume that every other value is type checked and adheres its specification
2. On every `kdb set` execute the default value calculation at least once
to be sure that a value can be calculated
3. Require a fallback

## Examples

Will come soon...