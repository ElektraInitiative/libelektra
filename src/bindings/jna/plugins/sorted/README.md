> - infos = Sorted Java Plugin, loaded by process plugin
> - infos/author = Markus Bointner <e11808221@student.tuwien.ac.at>, Philipp Leeb <e11808219@student.tuwien.ac.at>
> - infos/license = BSD
> - infos/provides = check
> - infos/placements = presetstorage postgetstorage
> - infos/metadata = check/sorted check/sorted/direction
> - infos/status = experimental
> - infos/version = 1
> - infos/description = Enforces a given order of array elements based on a custom defined key or primitive values

# Sorted checker plugin

This plugin checks if an Elektra array is sorted.

To mount the plugin, use the following command as guidance:

```sh
kdb mount-java config.ni user:/test/process kdb:ni java:org.libelektra.plugin.SortedPlugin
```

This will mount the file `config.ni` on mountpoint _user:/test/process_ with the KDB plugin _ni_ and the Java plugin _SortedPlugin_.

This plugin will only work on arrays, so add the array metakey first:

```sh
kdb meta-set user:/test/process array "#2"
```

The minimal configuration for this plugin is done by adding the metakey `check/sorted` with an empty value:

```sh
kdb meta-set user:/test/process check/sorted ""
```

The plugin will then assume the array elements are primitive types (e.g. string or integer) and will check if they are sorted in ascending order.

> Note: the plugin will always sort lexicographically, which can make a difference for numbers  
> e.g. numbers 9 and 10 are normally sorted as [9, 10], but with lexicographic order they are sorted as [10, 9] since 1 is before 9

If the array elements are not primitive types and have sub-keys, it is possible to provide a name to this sub-key (including a "/" prefix).  
Behavior is undefined when the prefix "/" is omitted.  
E.g. to sort by a sub-key `/nr`:

```sh
kdb meta-set user:/test/process check/sorted "/nr"
```

The plugin then requires all array elements to have a sub-key according to the set value (here "/nr") and sorts by these values.

To change the sorting direction, you can set the metakey `check/sorted/direction` to either "asc" (default) or "desc":

```sh
kdb meta-set user:/test/process check/sorted/direction "desc"
```

## Metakey summary

| Metakey                | Description                                                                                                                                                             |
| ---------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| check/sorted           | Activates the plugin. 2 possible values: <br/> - Empty: uses values of array elements directly <br/> - String representing a sub-key: uses value of sub-key for sorting |
| check/sorted/direction | Optional.<br/> Possible values:<br/> - _asc_: Ascending order (default, if direction is omitted)<br/>- _desc_: Descending order                                         |
