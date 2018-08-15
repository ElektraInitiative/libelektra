

## Introduction

I took [Relax NG](http://relaxng.org/spec-20011203.html#define-ref)'s `ref` and `define` part and tried to "copy" 
their approach for elektra's specification.
Until now I haven't really gotten a sensible idea for that though. Maybe we could talk on the next meeting about this.

Also I have no idea what your `struct` plugin actually does. There is no example, just some code snippet from 
the contract which doesn't help understanding the plugin.

## Problems
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

## Possible Solution

Users have to define recursive setting specifications in the following way:

```sh
kdb mount config.dump /recursive dump recursion
kdb setmeta user/recursive check/recursion "ref"
```

`check/recursion` is the basic building block for any recursive structure such as a vertex in a graph.
 Every key below `/recursive` in this example will be treated as vertex, such 
 as `/recursive/rootkey` and `/recursive/childkey`.
Furthermore the provided keyword (`ref` in the upper example) is used to refer to other recursive building blocks.

Note that you are only allowed to refer to vertices below the key on which the metadata is set (`/recursive` 
in the example above).

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

It is not needed to add `/recursive` before every key as it is only possible to choose keys
below this rootkey anyway. Referring to other keys is done by 
the [array notation](https://www.libelektra.org/tutorials/arrays).

Arbitrary additional data can be saved too such as:
`kdb set /recursive/A1/permission admin`

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

### Problems

#### Klemens architecture
Klemens exported the [ini file](https://github.com/Piankero/lcdproc-specification/blob/master/src/lcdexec/misc/lcdexec_elektra.ini) 
and showed me his approach.

First of all in his approach, he differs the `main` menu and the `menu` which are below the main menu differently. Also they have an additional attribute
for the displayname.

In the above example, the LCDproc variant would look like this in the current version:
```
kdb set /editor/menu/main/#0/displayname File
kdb set /editor/menu/main/#1/displayname Tools
```

Additionally they refer to submenu's implicitly. Eg the `Print` and `Settings` menu would be set like this:

```
kdb set /editor/menu/main/#0/menu/#0/displayname Print
kdb set /editor/menu/main/#0/menu/#1/displayname Settings
```

Note the difference between `main` and `menu` in the key. Additionally it would prohibit the reuse of the
`Settings` substructure for the `Tools` menu. The user would need to define everything again instead of just
referencing to it.

#### LCDproc's current format

[This](https://github.com/lcdproc/lcdproc/blob/master/clients/lcdexec/lcdexec.conf) is the current format in
lcdexec.
 
Their reference keyword (`check/recursion`) to other settings is `Entry`.

Their approach is easily adoptable with our current form and would not require any changes
for LCDproc.

Nonetheless I would suggest that LCDproc provides its own namespace for the menubar entries as it
 is much more clear which setting is meant.
It is much more clear to have eg `/lcdexec/menubar/Server` which could refer to a Server settings menu entry rather
than having it here: `/lcdexec/Server` which could be mistaken for the IPaddress to which the client connects to.


## Guarantees

1) References can only be set if the corresponding entry exists. This ensures that users do not set
keys which lead into nothing. If we do not want to restrict users in the order of declaration, we would emit a warning
if the setting does not exist yet.

2) Endless loops will be prohibited. eg. 
`kdb set /recursive/Vertex/ref/#0 Vertex`

## Problems
Users will be forced to declare building blocks first and then link them together.
Loading data via plugin does not guarantee ordering.

How to do that? eg. just emit warnings or introduce an "incomplete" state and users will have to call
`kdb getmeta /recursive complete/recursion` to check if the configuration is complete?