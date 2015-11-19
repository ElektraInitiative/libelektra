- infos = Information about ini plugin is in keys below
- infos/author = <name@libelektra.com> 
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = ini file backend

## EXAMPLES ##

```
akey/looking/like/sections = value
[emptysection]
[section1]
key/with/subkey = value2
key1 = value1
[section1/sub1]
[section1/sub1/subsub]
key1sub = val1sub
[section2]
key2 = value2
emptykey =
realemptykey
[section2/with/subkey]
key23 = value3
[section2/with/subkey/subsub/subsubsub]
key234 = value4
[sec]
a = 1
a = 2
a = 3
a = 4

```

`[section2/with/subkey]` and `[section2/with/subkey/subsub/subsubsub]` and their keys all belong to the same section `[section2]`
keys within a section (or subsection) are relatively ordered to it's section.


`kdb set system/ini/section1/sub2/keysub2 blubb`
creates a new section `[section1/sub2]` below the section `[section1]` with a key `keysub2 = blubb` resulting in

```
akey/looking/like/sections = value
[emptysection]
[section1]
key/with/subkey = value2
key1 = value1
[section1/sub1]
[section1/sub1/subsub]
key1sub = val1sub
[section1/sub2]
keysub2 = blubb
[section2]
key2 = value2
emptykey =
realemptykey
[section2/with/subkey]
key23 = value3
[section2/with/subkey/subsub/subsubsub]
key234 = value4
[sec]
a = 1
a = 2
a = 3
a = 4

```


## INTERNAL ##

```
system/ini:(/tmp/test.ini.9453:1448056785.734749.tmp)    sync: 1, ini/lastKey: 0, ini/lastSection: 5, ini/section: 0
system/ini/GLOBALROOT:()         sync: 0, binary: , ini/lastKey: 1, ini/section: 1
system/ini/GLOBALROOT/akey/looking/like/sections:(value)         sync: 0, ini/section: 1, order: 1, order/parent: system/ini/GLOBALROOT
system/ini/emptysection:()       sync: 0, binary: , ini/lastKey: 0, ini/section: 2
system/ini/sec:()        sync: 0, binary: , ini/lastKey: 1, ini/section: 5
system/ini/sec/a:()      sync: 0, ini/array: #3, ini/section: 5, order: 1, order/parent: system/ini/sec
system/ini/sec/a/#0:(1)  sync: 0, ini/section: 5
system/ini/sec/a/#1:(2)  sync: 0, ini/section: 5
system/ini/sec/a/#2:(3)  sync: 0, ini/section: 5
system/ini/sec/a/#3:(4)  sync: 0, ini/section: 5
system/ini/section1:()   sync: 0, binary: , ini/lastKey: 2, ini/section: 3
system/ini/section1/key/with/subkey:(value2)     sync: 0, ini/section: 3, order: 1, order/parent: system/ini/section1
system/ini/section1/key1:(value1)        sync: 0, ini/section: 3, order: 2, order/parent: system/ini/section1
system/ini/section1/sub1:()      sync: 0, binary: , ini/lastKey: 0, ini/section: 3, order/parent: system/ini/section1
system/ini/section1/sub1/subsub:()       sync: 0, binary: , ini/lastKey: 1, ini/section: 3, order/parent: system/ini/section1/sub1
system/ini/section1/sub1/subsub/key1sub:(val1sub)        sync: 0, ini/section: 3, order: 1, order/parent: system/ini/section1/sub1/subsub
system/ini/section1/sub2:()      sync: 1, binary: , ini/lastKey: 1, ini/section: 3, order/parent: system/ini/section1
system/ini/section1/sub2/keysub2:(blubb)         sync: 1, ini/section: 3, order: 1, order/parent: system/ini/section1/sub2
system/ini/section2:()   sync: 0, binary: , ini/lastKey: 3, ini/section: 4
system/ini/section2/emptykey:()  sync: 0, ini/section: 4, order: 2, order/parent: system/ini/section2
system/ini/section2/key2:(value2)        sync: 0, ini/section: 4, order: 1, order/parent: system/ini/section2
system/ini/section2/realemptykey:()      sync: 0, ini/empty: , ini/section: 4, order: 3, order/parent: system/ini/section2
system/ini/section2/with/subkey:()       sync: 0, binary: , ini/lastKey: 1, ini/section: 4, order/parent: system/ini/section2
system/ini/section2/with/subkey/key23:(value3)   sync: 0, ini/section: 4, order: 1, order/parent: system/ini/section2/with/subkey
system/ini/section2/with/subkey/subsub/subsubsub:()      sync: 0, binary: , ini/lastKey: 1, ini/section: 4, order/parent: system/ini/section2/with/subkey
system/ini/section2/with/subkey/subsub/subsubsub/key234:(value4)         sync: 0, ini/section: 4, order: 1, order/parent: system/ini/section2/with/subkey/subsub/subsubsub

```


## EXTERNAL ##

The exported tree looks like
```
system/ini
system/ini/GLOBALROOT
system/ini/GLOBALROOT/akey/looking/like/sections
system/ini/emptysection
system/ini/sec
system/ini/sec/a
system/ini/sec/a/#0
system/ini/sec/a/#1
system/ini/sec/a/#2
system/ini/sec/a/#3
system/ini/section1
system/ini/section1/key/with/subkey
system/ini/section1/key1
system/ini/section1/sub1
system/ini/section1/sub1/subsub
system/ini/section1/sub1/subsub/key1sub
system/ini/section1/sub2
system/ini/section1/sub2/keysub2
system/ini/section2
system/ini/section2/emptykey
system/ini/section2/key2
system/ini/section2/realemptykey
system/ini/section2/with/subkey
system/ini/section2/with/subkey/key23
system/ini/section2/with/subkey/subsub/subsubsub
system/ini/section2/with/subkey/subsub/subsubsub/key234

```
Note that interally they key `system/ini/section2/with/subkey/subsub` exists, but gets removed from the keyset because it's useless externally.


