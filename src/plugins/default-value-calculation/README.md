- infos = Information about the conditionals plugin is in keys below
- infos/author = TBD
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/recommends =
- infos/placements = pregetstorage
- infos/status = idea
- infos/metadata = calculate/default
- infos/description = the plugin calculates the default value of a given key if no value is present

## Introduction

The plugin is used to calculate default values if no specific value was given.

## Description

The plugin can be used to calculate or use values which it fetches from others keys.
The associated metadata is `calculate/default` and is a DSL.
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

Arithmetic expressions *always* return an integer and do not cast to
float/double.

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

Conditionals always require an `else` statement to guarantee a return
value.

#### Testing if Key exists
`(! ../key/B)` evaluates to true if the key `../key/A` doesn't exist, to false if it exists.

## Open Questions

### Unreturnable defaults after changes to other keys
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

### Errors

It is hard to think of all possible errors but one suggestion could be:

1. Type Errors (adding strings to integers)
2. Syntax Errors (forgetting brackets)
3. Calculation errors (overflow/ underflow)

I think most possible errors will be found once the implementation is going
to start.

## Examples

### Conditional calculation

Assume the following config:
```yaml
setting_a: 5
setting_b: 32
```

Imagine that if setting_a is set explicitly then setting_b must be set to at
least twice the size of setting_a / 1024. Otherwise the integer 32 should be returned

This would look like this in the metadata:

```ini
[/setting_b]
calculate/default = "
if( ../setting_a) {
    return (setting_a/1024)*2
} else {
    return 32
}
"
```

We check if setting_a exists and return the calculated value. If it
is absent we return the specified constant.

### Switch clauses

Assume the following config:
```ini
# legal: 12232, 12832, 1602
Model:12232

Size:20x4
```

Imagine that you want to change the default size depending on the
`Model` selected. Lets assume the following size-model mappings:
12232: 20x4; 12832: 21x4; 1602: 16x2
This would look like this in the metadata:

```ini
[/Size]
calculate/default = "
if( ../Model == "12232") {
    return "20x4"
} else if (../Model == "12832") {
    return 21x4
} else {
    return 16x2
}
"
```

The code is a simple `if else` which is common in most of the
programming languages.