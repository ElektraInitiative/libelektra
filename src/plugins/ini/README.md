- infos = Information about ini plugin is in keys below
- infos/author = 
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
[section2/with/subkey]
key23 = value3
[section2/with/subkey/subsub/subsubsub]
key234 = value4
```

`[section2/with/subkey]` and `[section2/with/subkey/subsub/subsubsub]` and their keys all belong to the same section `[section2]`
keys within a section (or subsection) are relatively ordered to it's section.


`kdb set /test/section1/sub2/keysub2 blubb`
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
[section2/with/subkey]
key23 = value3
[section2/with/subkey/subsub/subsubsub]
key234 = value4
```
