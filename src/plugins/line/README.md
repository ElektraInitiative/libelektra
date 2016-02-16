- infos = Information about LINE plugin is in keys below
- infos/author = Ian Donnelly <ian.s.donnelly@gmail.com>
- infos/provides = storage
- infos/licence = BSD
- infos/needs = null
- infos/placements = getstorage setstorage
- infos/status = maintained unittest nodep libc final
- infos/description = Very simple storage plug-in which stores each line from a file as a key

## Introduction ##

This plugin is designed to save each line from input as a key. The
keys are stored in an array. The key names are determined by the 
line number such as `#3` or `#_12` and the value of each key 
is the information stored in that line of the file. The plugin considers
`#0` to be the first line and will automatically add `_` to the beginning
of key names in order to keep them in numerical order.  

For instance, consider the following file name `.config` where the 
numbers on the left just represent the line numbers:

    1  setting1 true
    2  setting2 false
    3  setting3 1000
    4  #comment
    5
    6
    7  //some other comment
    8
    9  setting4 -1
    10 

This file would result in the following keyset which is being diplayed as
`name: value`:

    #0: setting1 true
    #1: setting2 false
    #2: setting3 1000
    #3: #comment
    #4:
    #5:
    #6: //some other comment
    #7:
    #8: setting4 -l
    #9:
    #_10:
