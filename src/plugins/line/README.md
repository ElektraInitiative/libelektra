- infos = Information about LINE plugin is in keys below
- infos/author = Ian Donnelly <ian.s.donnelly@gmail.com>
- infos/provides = storage
- infos/licence = BSD
- infos/needs = null
- infos/placements = getstorage setstorage
- infos/status = maintained unittest nodep libc final
- infos/description = storage plugin which stores each line from a file

## Introduction ##

This plugin is useful if you have a file in a format not supported
by any other plugin and want to use the Elektra tools to edit
individual lines.

This plugin is designed to save each line from a file as a key.
The keys form an array. The key names are determined by the 
line number such as `#3` or `#_12` for lines 4 and 13.
The plugin considers `#0` to be the first line.
The plugin will automatically add `_` to the beginning
of key names in order to keep them in numerical order (like specified
for Elektra arrays).

The value of each key hold the content of the actual file line-by-line.

For example, consider the following content of the file `~/.config/line` where the
numbers on the left represent the line numbers:

    1  setting1 true
    2  setting2 false
    3  setting3 1000
    4  #comment
    5
    6
    7  //some other comment
    8
    9  setting4 -1

We mount that file by:

    > kdb mount line user/line line

This file would result in the following keyset which is being displayed as
`key: value`, e.g. with:

    > kdb export -c "format=%s: %s" user/line simpleini

    #0: setting1 true
    #1: setting2 false
    #2: setting3 1000
    #3: #comment
    #4:
    #5:
    #6: //some other comment
    #7:
    #8: setting4 -l
