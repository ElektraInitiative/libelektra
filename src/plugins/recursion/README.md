

## Introduction

I took [Relax NG](http://relaxng.org/spec-20011203.html#define-ref)'s `ref` and `define` part and tried to "copy" 
their approach for elektra's specification.
Until now I haven't really gotten a sensible idea for that though. Maybe we could talk on the next meeting about this.

Also I have no idea what your `struct` plugin actually does. There is no example, just some code snippet from 
the contract which doesn't help understanding the plugin.

## Basic Problem
1) We want to check if all recursion links are non-cyclical, eg. `a` -> `b` -> `c` -> `a`
2) Imagine a structure like `a` -> `b`. A user sets `a` but forgets `b`. He links `a` to (yet non existent) `b`.
Now there are two possibilties. Either we prohibit the linking and emit an error, forcing the user to define
the `b` setting first or we just emit a warning and expect the user to declare `b` later. The latter one could lead to
incomplete configurations.

The first option would require a certain ordering which clashes with the fact that some storage plugins do not
premain the ordering. An example:
```
[b]

[a]
ref = b
```

is a legal configuration. Loading and saving the configuration again could lead to a reversed structure

```
[a]
ref = b

[b]
```

which could cause an error since `a` cannot be loaded as `b` is yet non existent.

## Solution (basic plugin functionality)

Users have to define recursive setting specifications in the following way:

```sh
kdb mount config.dump user/recursive dump recursion
kdb setmeta user/recursive check/recursion "ref"
```

The value of `check/recursion` defines the basename for _reference arrays_. In the example above any key which is below `user/recursive` and whose name ends in `ref` must be an [array](https://www.libelektra.org/tutorials/arrays). Additionally all elements in these arrays must have values referencing other keys. That means if `user/recursive/key1/ref/#0` had the value `key2` then the key `user/recursive/key2` must exist.

In other words each of these reference arrays defines a list of edges of a graph. Continuing the example `user/recursive/key1/ref/#0 = key2` defines a directed edge between `user/recursive/key1` and `user/recursive/key2`. The graph resulting from these edges, must be a directed acyclic graph.

All other keys will not be touched by this plugin.

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
kdb set /recursive/A0
kdb set /recursive/A1
kdb set /recursive/A2
kdb set /recursive/B0
kdb set /recursive/B1
kdb set /recursive/B2

#Next we link each entries toegether

#For A:
kdb set /recursive/A0/ref/#0 A1
kdb set /recursive/A0/ref/#1 A2

#For A1:
kdb set /recursive/A1/ref/#0 B0

#For A2:
kdb set /recursive/A2/ref/#0 B1
kdb set /recursive/A2/ref/#1 B2
```

### Additional functionality
In addition to the functionality described above the plugin can be customized in a few ways.

1. By specifying the `check/recursion/restrict` metakey on a reference array key the allowed references can be restricted.
2. By specifying the `check/recursion/override` metakey the name used for reference arrays can be overriden.

#### Reference Restriction
Let's start with a basic setup:
```
kdb setmeta /recursive check/recursion ref

#entries
kdb set /recursive/typeA/key1
kdb set /recursive/typeA/key2
kdb set /recursive/typeB/key1
kdb set /recursive/typeB/key2
```

The value of the `check/recursion/restrict` metakey is a globbing expression. This globbing expression does not have to match the full key referenced, but the referenced key must be below a matching key. So by setting the following metadata we restrict the possible references from `/recursive/typeA/key1` to keys below `/recursive/typeB`:

```
kdb setmeta /recursive/typeA/key1/ref check/recursion/restrict typeB

kdb set /recursive/typeA/key1/ref/#0 /recursive/typeB/key1 # works
kdb set /recursive/typeA/key1/ref/#1 /recursive/typeB/key2 # works

kdb set /recursive/typeA/key1/ref/#2 /recursive/typeB/key1 # ERROR!!
```

By setting the metakey `check/recursion/restrict` to an empty value `""` you can specify that there should not be any references at all, i.e. the node of the graph **must** be a leaf node.

You can also specify multiple alternative restrictions by using an array:
```
kdb setmeta /recursive/typeA/key1/ref check/recursion/restrict/#0 typeB
kdb setmeta /recursive/typeA/key1/ref check/recursion/restrict/#1 typeC

kdb set /recursive/typeA/key1/ref/#0 /recursive/typeB/key1 # works
kdb set /recursive/typeA/key1/ref/#1 /recursive/typeB/key2 # works

kdb set /recursive/typeA/key1/ref/#2 /recursive/typeB/key1 # ERROR!!

kdb set /recursive/typeA/key1/ref/#0 /recursive/typeC/key1 # works too as long as /recursive/typeB/key1 exists
```
NOTE: The value of `check/recursion/restrict` itself will always override any array sub-keys. So if you want to specify alternative restrictions do not set the metakey `check/recursion/restrict`. If you set `check/recursion/restrict` to an empty value and specify array elements, the value of `check/recursion/restrict` will still win and mark the node as a leaf node.


#### Overriding
We will use the same basic setup:
```
kdb setmeta /recursive check/recursion ref

#entries
kdb set /recursive/typeA/key1
kdb set /recursive/typeA/key2
kdb set /recursive/typeB/key1
kdb set /recursive/typeB/key2
```

The value of `check/recursion/override`, as the name suggests, overrides the value of `check/recursion`.

```
kdb setmeta /recursive/typeA check/recursion/override altref
```

After executing the above line `/recursive/typeA/ref/#0` will no longer be considered a reference, but will be ignored like any other value. This because the metadata on `/recursive/typeA` tells the plugin to look for `altref` instead of `ref`, when looking at keys below `/recursive/typeA`. This allows giving the reference keys more expressive names instead of settling for a single name inside one structure.

## Another example

In order to better show how this plugin can be used, a more sophisticated example will be given.
Imagine that you want to create your own menu bar with arbitrary submenus possible. The following example
is inspired from libreoffice and simplified.

Imagine an editor with a `File` menu with a `Print` option as well as a submenu called `Settings`.
The `Settings` submenu has `Global Settings` and  `Autocorrect Options`
Beside the `File` menu we also have a `Tool` menu which also has the same `Settings` submenu but also
has gotten a `Extension Manager` and a Menu `Macros`. Macros has again a submenu `Organize Macros` which
again is a menu and has `Macro Basics`.

The following picture illustrates the menu tree:
![menu-example](example.png?raw=true "Example")

So how can we map the specification as well as configuration into elektra?

First we create a config and mount it. Next we will set the specification for all menu entries as following:
```
kdb mount config.dump /editor dump recursion
sudo kdb setmeta /editor check/recursion "menuref"
```

Keys below `check/recursion` are used to define the basic building block of our recursive structure, 
just like vertices in a graph. The provided string (`menuref` in our example) is used to link together our defined
building blocks. 

Each subkey in our example below `/editor` will now be treated as recursive entry (vertex). You can define arbitrary
many menues like this. Menues can be linked in our example via `menuref`, 
like this: `/editor/<other_resursive_entry>/menuref/#[d]` and is 
done in the [array notation](https://www.libelektra.org/tutorials/arrays).

Next we create all distinct recursive entries such as `File`, `Print` etc. Note that the `Settings`
 menu appears twice but we will only need to declare it once. The order of the array
  or the `kdb set` commands does not matter:

```
kdb set /editor/File
kdb set /editor/Tools
kdb set /editor/Print
kdb set /editor/Settings
kdb set /editor/GlobalSettings
kdb set /editor/AutocorrectOptions
kdb set /editor/ExtensionManager
kdb set /editor/Macros
kdb set /editor/OrganizeMacros
kdb set /editor/MacroBasics
```

Now that we have defined we can link together all vertices(menues). We start with the `File` menu:

```
kdb set /editor/File/menuref/#0 Print
kdb set /editor/File/menuref/#1 Settings
```

Next we link the `Settings` vertex:

```
kdb set /editor/Settings/menuref/#0 GlobalSettings
kdb set /editor/Settings/menuref/#1 AutocorrectOptions
```

Now we link the `Tools` vertex:
```
kdb set /editor/Tools/menuref/#0 ExtensionManager
kdb set /editor/Tools/menuref/#1 Settings
kdb set /editor/Tools/menuref/#2 Macros
```

Note that `Settings` is already linked. We do not need to do it for the `Tools` vertex again.

Finally we link all vetices of `Macros`:

```
kdb set /editor/Macros/menuref/#0 OrganizeMacros
kdb set /editor/OrganizeMacros/menuref/#0 MacroBasics
```

Note that you can add arbitrary data to any key such as
`kdb set /editor/File/highlight_color yellow`.

### Example of wrong settings

In the above example the following settings will be rejected:

```
#Link "Macro Basics" to the parent menu "Macros" and create an endless loop
kdb set /editor/MacroBasics/menuref/#0 Macros
# ERR: 198
# STDERR: Reason: Cyclical reference detected: "MacroBasics" -> 
# "Macros" -> OrganizeMacros" -> "Macro Basics"

#Link to something which does not exist or is potentially outside of the /editor rootkey
kdb set /editor/MacroBasics/menuref/#0 AboutPage
# ERR: 199
# STDERR: Reason: Could not find recursive entry with name "AboutPage".
# Maybe it does not exist yet?
```

Sitenote: Maybe the last example here will just emit a warning so users are free in the order of declaration.

## Guarantees

1) References can only be set if the corresponding entry exists. This ensures that users do not set
keys which lead into nothing. If we do not want to restrict users in the order of declaration, we would emit a warning if the setting does not exist yet.

2) Endless loops will be prohibited. eg. 
`kdb set /recursive/Vertex/ref/#0 Vertex`