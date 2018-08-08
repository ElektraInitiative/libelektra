

## Introduction

I took [Relax NG](http://relaxng.org/spec-20011203.html#define-ref)'s `ref` and `define` part and tried to "copy" 
their approach for elektra's specification.
Until now I haven't really gotten a sensible idea for that though. Maybe we could talk on the next meeting about this.

Also I have no idea what your `struct` plugin actually does. There is no example, just some code snippet from 
the contract which doesn't help understanding the plugin.

## Problems
1) We want to check if all recursions are non-cyclical
2) We want to check if a recursive config actually leads to another correct config and not into NULL for example
3) Forcing users to define non-recursive settings first on which they can build up 
recursive settings is inconvenient and could lead to orphan settings.
4) We can only check right after calling `kdb set` and not a whole config. 
This means that user can have "unfinished" configurations while setting new values.

## Possible Solution

Users have to define recursive setting specifications in the following way:

```
kdb mount config.dump /recursive dump recursion
kdb setmeta user/recursive check/recursion/vertex "A, B"
kdb setmeta user/recursive check/recursion/leaf "z, y"
```

`check/recursion/vertex` basically means that keys containing `A` or `B` after `/recursive`
will have further configurations below them. `A` for example could stand for a Menu entry which can have
Menu entries below. `B` could be some other form of Menu, but also having entries below. There could be 
potentially more `vertices` which can have children.

So users would build up structures like this:
`/recursive/<vertex>/#[0-9]+/<vertex>/#[0-9]+/.....`

`#[0-9]+` is used to declare children. So  

- `/recursive/A/#0`
- `/recursive/A/#1`

for example both have A as their parent.


`check/recursion/leaf` means that there is no subvertex allowed anymore. So this is not allowed:
`/recursive/<vertex>/#[0-9]+/<leaf>/#0/<vertex>`.

Now take this setting as an example:

* A
    * A1
        * z
    * A2
        * B
        * y

Users will have to set this structure in the following way:

```
kdb set /recursive/menu menu
kdb set /recursive/menu/#0 menu1
kdb set /recursive/menu/#1 menu2
kdb set /recursive/menu/#0/menupoint1 menu2
kdb set /recursive/menu/#1/menupoint1/#0 submenu
kdb set /recursive/menu/#1/menupoint2/#1 menupoint2
```

Vertices without leafs are allowed (eg. submenu in this example).

A command will fail if users want to set settings for non-existent vertices, eg. `kdb set /recursive/menu/#5/....`

The validation solely happens on the structure, not on the values of the keys.

Users should also not be able to define endless recursions. If the value of a key though 
tells where the submenu is, endless recursions are possible.