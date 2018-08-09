

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
kdb setmeta user/recursive check/recursion/vertex "define"
kdb setmeta user/recursive check/recursion/ref "ref"
```

`check/recursion/vertex` is the basic building block for any recursive structure.
 
`define` in the the example above is the placeholder in they key name to declare a basic building block.
For example `/recursive/define` would be such a declaration.

So users would build up structures like this:
`/recursive/<vertex>/#[0-9]+/<vertex>/#[0-9]+/.....`

`#[0-9]+` is used to declare children. So

- `/recursive/define/#0`
- `/recursive/define/#1`

are both definitions of a recursive structure. Now assume you want to refer from `define/#0` to `define/#1`.
This would work like this: `kdb set /recursive/define/#0/ref/#0 /recursive/define/#1`.

Now take this setting as an example:

* A0
    * A1
        * B0
    * A2
        * B1
        * B2

Users will have to set this structure in the following way:

```
#First we define all entries
kdb set /recursive/define/#0 A0
kdb set /recursive/define/#1 A1
kdb set /recursive/define/#2 A2
kdb set /recursive/define/#3 B0
kdb set /recursive/define/#4 B1
kdb set /recursive/define/#5 B2

#Next we link each entries toegether

#For A:
kdb set /recursive/define/#0/ref/#0 /recursive/define/#1
kdb set /recursive/define/#0/ref/#1 /recursive/define/#2

#For A1:
kdb set /recursive/define/#1/ref/#0 /recursive/define/#3

#For A2:
kdb set /recursive/define/#2/ref/#0 /recursive/define/#4
kdb set /recursive/define/#2/ref/#1 /recursive/define/#5
```


Arbitrary additional data can be saved too such as:
`kdb set /recursive/define/#2/permission admin`

## Guarantees

1) References can only be set if the corresponding entry exists. This ensures that users do not set
keys which lead into nothing

2) Endless loops will be prohibited. eg. 
`kdb set /recursive/define/#0/ref/#0 /recursive/define/#0`

## Problems
Users will be forced to declare building blocks first and then link them together.
Loading data via plugin does not guarantee ordering.

How to do that? eg. just emit warnings or introduce an "incomplete" state and users will have to call
`kdb getmeta /recursive complete/recursion` to check if the configuration is complete?