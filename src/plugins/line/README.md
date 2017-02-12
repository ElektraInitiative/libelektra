- infos = Information about line plugin is in keys below
- infos/author = Ian Donnelly <ian.s.donnelly@gmail.com>
- infos/provides = storage/line
- infos/licence = BSD
- infos/needs = null
- infos/placements = getstorage setstorage
- infos/status = maintained unittest nodep libc final limited
- infos/description = storage plugin which stores each line from a file

## Introduction

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

## Examples

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

    > sudo kdb mount line user/line line

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

### Creating Files


```sh
# Backup-and-Restore:/examples/line

sudo kdb mount line /examples/line line

kdb set /examples/line/add something
kdb set /examples/line/ignored huhu
kdb set /examples/line ignored   # adding parent key does nothing
kdb set /examples/line/add here

cat `kdb file /examples/line`
#> here
#> huhu
#> something

kdb ls /example/line
#> user/example/line
#> user/example/line/#0
#> user/example/line/#1
#> user/example/line/#2

kdb export /example/line simpleini
#> user = 
#> #0 = here
#> #1 = is
#> #2 = something

kdb set /example/line/#1 huhu
#> Using name user/example/line/#1
#> Set string to huhu

kdb export /example/line simpleini 
#> user = 
#> #0 = here
#> #1 = huhu
#> #2 = something

kdb export /example/line line
#> here
#> huhu
#> something

sudo kdb umount /examples/line
```


### Other Tests

```sh
# Backup-and-Restore:/examples/line

sudo kdb mount line /examples/line line

# create and initialize testfile
cat > `kdb file /examples/line` << EOF \
setting1 true\
setting2 false\
setting3 1000\
#comment\
\
\
//some other comment\
\
setting4 -1

# output filecontent and display line numbers
awk '{print NR-1 "-" $0}' < `kdb file /examples/line`
#> 0-setting1 true
#> 1-setting2 false
#> 2-setting3 1000
#> 3-#comment
#> 4-
#> 5-
#> 6-//some other comment
#> 7-
#> 8-setting4 -1

# export keyset with syntax '_key: value'
kdb export -c "format=_%: %" /examples/line simpleini
#> _user: 
#> _#0: setting1 true
#> _#1: setting2 false
#> _#2: setting3 1000
#> _#3: #comment
#> _#4: 
#> _#5: 
#> _#6: //some other comment
#> _#7: 
#> _#8: setting4 -1

# cleanup
kdb rm -r /examples/line
sudo kdb umount /examples/line
```
