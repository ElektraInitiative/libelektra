- infos = Information about LINE plugin is in keys below
- infos/author = Ian Donnelly <ian.s.donnelly@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = Very simple storage plug-in which stores each line from a file as a key

## Introduction ##

This plug-in is designed to save each line from input as a key. They keys are names line# 
with 0s used as padding so that the lines are stored in numerical order.


## Restrictions ##

Lines are named with traling 0s based on the total number of lines for numerical ordering. 
The first line in a file with 1000 lines will be named line0001 whereas the first line in a file 
with 10 lines will be named 001.
