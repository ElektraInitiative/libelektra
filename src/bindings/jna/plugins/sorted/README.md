> - infos = Sorted Java Plugin, loaded by process plugin  
> - infos/author = Markus Bointner <e11808221@student.tuwien.ac.at>, Philipp Leeb <e11808219@student.tuwien.ac.at>  
> - infos/license = BSD  
> - infos/provides = check  
> - infos/placements = presetstorage postgetstorage  
> - infos/metadata = check/sorted check/sorted/direction  
> - infos/description = Enforces a given order of array elements based on a custom defined key or primitive values  
> - infos/status = experimental  
> - infos/version = 1  
> - exports/has/set = 1  
> - exports/has/get = 1  

# Sorted checker plugin

This plugin checks if an elektra array is sorted.  

To activate the plugin add the following meta-key (with empty value) to an existing array:

``check/sorted = ``

The plugin will then assume the array elements are primitive types (e.g. string or integer) and will check if they are sorted in ascending order.  
If the array elements are not primitive types and have sub-keys, it is possible to provide a name to this sub-key.
E.g. to sort by a sub-key `/nr`: 

``check/sorted = /nr``

It is also possible to change the sorting direction by setting the key `check/sorted/direction` to either *asc* or *desc*.

## Metakey summary

| Metakey                | Description                                                                                                                                                             |
|------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| check/sorted           | Activates the plugin. 2 possible values: <br/> - Empty: uses values of array elements directly <br/> - String representing a sub-key: uses value of sub-key for sorting |
| check/sorted/direction | Optional.<br/> Possible values:<br/> - *asc*: Ascending order (default, if direction is omitted)<br/>- *desc*: Descending order                                         |
