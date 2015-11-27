- infos = Information about ini plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = 


## changes ##
Ordering by default

autosectioning by default on kdbSet

removed meta support (moving to keytometa ? )
 
## Array ##

`array`
repeating keys are treated as an array
```
[sec]
a = 1
a = 2
a = 3
a = 4
```
gets 
```
/sec
/sec/a
/sec/a/#0
/sec/a/#1
/sec/a/#2
/sec/a/#3

```

## simple ##
`simple`
Turns ini into simpleIni

