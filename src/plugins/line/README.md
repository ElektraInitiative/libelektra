- infos = Information about LINE plugin is in keys below
- infos/author = Ian Donnelly <ian.s.donnelly@gmail.com>
- infos/licence = BSD
- infos/needs = null
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = Very simple storage plug-in which stores each line from a file as a key

## Introduction ##

This plugin is designed to save each line from input as a key. The
keys are stored in an array. The key names are determined by the 
line number such as `#01` or `#12` and the value of each key 
is the infromation stored in that line of the file.  

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

This file would result in the follwing keyset which is being diplayed as
`name: value`:

    #01: setting1 true
    #02: setting2 false
    #03: setting3 1000
    #04: #comment
    #05:
    #06:
    #07: //some other comment
    #08:
    #09: setting4 -l
    #10:
