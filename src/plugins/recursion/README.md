

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

## A more real example

In order to better show how this plugin can be used, a more sophisticated example will be given.
Imagine that you want to create your own menu with arbitrary submenus possible. The following example
is inspired from libreoffice and simplified.

Imagine an editor with a `File` menu with a `Print` option as well as a submenu called `Settings`.
The `Settings` submenu has `Global Settings` and  `Autocorrect Options`
Beside the `File` menu we also have a `Tool` menu which also has the same `Settings` submenu but also
has gotten a `Extension Manager` and a Menu `Macros`. Macros has again a submenu `Organize Macros` which
again is a menu and has `Macro Basics`.

The following picture illustrates the menu tree:
![menu-example](example.png?raw=true "Example")

So how can we map this into an elektra configuration?

First we create a config and mount it. Next we will set the specification for all menu entries as following:
```
kdb mount config.dump /editor dump recursion
sudo kdb setmeta /editor check/recursion/vertex "menu"
sudo kdb setmeta /editor check/recursion/ref "menuref"
```
Each key definition in the form of `/editor/menu` will now be treated as recursive entry. You can define arbitrary
many menues like this: `/editor/menu/#[d]` where `[d]` stands for a positive number.

Menues can be linked via `menuref` like this: `/editor/menu/#[d]/menuref/#[d]` and is again in the array notation.
Note that `menu` and `menuref` are our defined keywords in the setmeta command from above.

Next lets create all distinct menu entries such as `File`, `Print` etc. Note that the Settings
 menu appears twice but we will only need to declare it once. The order does not matter:

```
kdb set /editor/menu/#0 File
kdb set /editor/menu/#1 Tools
kdb set /editor/menu/#2 Print
kdb set /editor/menu/#3 Settings
kdb set /editor/menu/#4 "Global Settings"
kdb set /editor/menu/#5 "Autocorrect Options"
kdb set /editor/menu/#6 "Extension Manager"
kdb set /editor/menu/#7 "Macros"
kdb set /editor/menu/#8 "Organize Macros"
kdb set /editor/menu/#9 "Macro Basics"
```

Now that we have defined we can link together all menues. Lets start with the `File` menu:

```
kdb set /editor/menu/#0/menuref/#0 /editor/menu/#2
kdb set /editor/menu/#0/menuref/#1 /editor/menu/#3
```

Next we link the `Settings` menu:

```
kdb set /editor/menu/#3/menuref/#0 /editor/menu/#5
kdb set /editor/menu/#3/menuref/#1 /editor/menu/#6
```

Now we link the `Tools` section:
```
kdb set /editor/menu/#1/menuref/#0 /editor/menu/#5
kdb set /editor/menu/#1/menuref/#1 /editor/menu/#3
kdb set /editor/menu/#1/menuref/#2 /editor/menu/#7
```

Note that `Settings` is already linked. We do not need to do it for the `Tools` menu again.

Finally we link all submenus of `Macros`:

```
kdb set /editor/menu/#7/menuref/#0 /editor/menu/#8
kdb set /editor/menu/#8/menuref/#0 /editor/menu/#9
```

### Example of wrong settings

In the above example the following settings will be rejected:

```
#Link "Macro Basics" to the parent menu "Macros" and create an endless loop
kdb set /editor/menu/#9/menuref/#0 /editor/menu/#7
ERR: 198
Reason: Cyclical reference detected: "Macro Basics" -> "Macros" -> Organize Macros" -> "Macro Basics"

#Link to something which does not exist
kdb set /editor/menu/#9/menuref/#0 /editor/menu/#10
ERR: 199
Reason: Could not find vertex with #10. Maybe it does not exist yet?
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

Their approach is not array based but name based. 
Their reference keyword (`check/recursion/ref`) to other settings is `Entry`.

They do not use an array based approach though but give names instead.
So eg. instead of `menu/#0` they give it an arbitrary name such as `FileMenu`.

Again taking the example from above for the `File` menu:

```
#Link to (yet non existent) menus
kdb set /editor/MainMenu/Entry/#0 /editor/FileMenu

#Give the displayname
kdb set /editor/FileMenu/DisplayName File

#Link to (yet non existent) menus
kdb set /editor/FileMenu/Entry/#0 /editor/PrintMenu
kdb set /editor/FileMenu/Entry/#1 /editor/SettingsMenu

#Give the displaynames
kdb set /editor/PrintMenu/DisplayName SettingsPrint
kdb set /editor/SettingsMenu/DisplayName Settings

#Link the Settings Menu
kdb set /editor/SettingsMenu/Entry/#0 /editor/GlobalSettings
kdb set /editor/SettingsMenu/Entry/#1 /editor/AutocorrectOptions

#Give the displaynames
kdb set /editor/GlobalSettings/DisplayName "Global Settings"
kdb set /editor/AutocorrectOptions/DisplayName "Autocorrect Options"
``` 

This would at least guarantee reusability of settings.

The plugin could theoretically support an arbitrary name instead of an array. But how does the 
specification know what is actually a menu entry and what is just some arbitrary other setting.

Imagine you want to configure something completely Menu unrelated, eg. the Fonts for the editor:

```
kdb set /editor/SupportedFonts/Entry/#0 Verdana
kdb set /editor/SupportedFonts/Entry/#1 TimesNewRoman
```

How can the plugin differentiate between settings it should check for and those who are not of any interest?
We could say that for example we apply the recursive specification to another namespace, eg. 
`/editor/menudeclarataion/FileMenu`.

This though would at least require LCDproc to prepend the namespace infront.


## Guarantees

1) References can only be set if the corresponding entry exists. This ensures that users do not set
keys which lead into nothing. If we do not want to restrict users in the order of declaration, we would emit a warning
if the setting does not exist yet.

2) Endless loops will be prohibited. eg. 
`kdb set /recursive/define/#0/ref/#0 /recursive/define/#0`

## Problems
Users will be forced to declare building blocks first and then link them together.
Loading data via plugin does not guarantee ordering.

How to do that? eg. just emit warnings or introduce an "incomplete" state and users will have to call
`kdb getmeta /recursive complete/recursion` to check if the configuration is complete?