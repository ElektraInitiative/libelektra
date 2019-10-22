
# How-To: kdb merge

## Introduction

The kdb tool allows users to access and perform functions on the Elektra key database from the command line. We added
a new command to this very useful tool, the `merge` command. This command allows a user to perform a three-way merge
of KeySets from the `kdb` tool.

The command to use this tool is:

kdb cmerge [OPTIONS] ourpath theirpath basepath resultpath

The standard naming scheme for a three-way merge consists of `our`, `their`, and `base`:

- `our` refers to the local copy of a file
- `their` refers to a remote copy
- `base` refers to their common ancestor.

For configuration files in the context of package upgrades:

- `our` is the user's copy
- `their` is the maintainer's copy,
- `base` is the previous version of the maintainer's copy.

## Options

As for the options, there are a few basic options:

- `-f`, `--force`: which overwrites any Keys in `resultpath`
- `-v`, `--verbose`: give additional information

Strategies offer fine grained control over conflict handling. The option is:

- `-s <name>`, `--strategy <name>`: which is used to specify a strategy to use in case of a conflict

The current strategies are:

- `abort`: the merge will abort if any conflict happens
- `our`: the merge will use our version during a conflict
- `their`: the merge will use their version during a conflict

If no strategy is specified, the merge will default to the abort strategy.


## Examples


### Simple



The easiest case is if all three versions contain equal data.
```sh
kdb set user/tests/base a
#> Create a new key user/tests/base with string "a"
kdb set user/tests/their a
#> Create a new key user/tests/their with string "a"
kdb set user/tests/our a
#> Create a new key user/tests/our with string "a"
kdb cmerge user/tests/our user/tests/their user/tests/base user/tests/result
kdb get user/tests/result
#> a
```



We change the our version for another example.
```sh
kdb set user/tests/our b
#> Set string to "b"
```



Using a result path that is not empty gives an error.
The option `--force` can be used to override.
```sh
kdb cmerge user/tests/our user/tests/their user/tests/base user/tests/result
# RET: 1
# STDERR: ERROR: 1 keys exist in resultpath. Use -f to override the keys there.
kdb cmerge -f user/tests/our user/tests/their user/tests/base user/tests/result
kdb get user/tests/result
#> b
```



We can use the same key for different versions.
```sh
kdb set user/tests/same a
#> Create a new key user/tests/same with string "a"
kdb cmerge -f user/tests/same user/tests/same user/tests/same user/tests/result
kdb get user/tests/result
#> a
```

### hosts

We import three different (see the comment) versions of a hosts file.
```sh
echo "127.0.0.1       localhost\
127.0.1.1       computer\
\
# BASE The following lines are desirable for IPv6 capable hosts\
::1     ip6-localhost ip6-loopback\
fe00::0 ip6-localnet\
ff00::0 ip6-mcastprefix\
ff02::1 ip6-allnodes\
ff02::2 ip6-allrouters" | kdb import user/tests/hosts/base hosts

echo "127.0.0.1       localhost\
127.0.1.1       computer\
\
# OUR The following lines are desirable for IPv6 capable hosts\
::1     ip6-localhost ip6-loopback\
fe00::0 ip6-localnet\
ff00::0 ip6-mcastprefix\
ff02::1 ip6-allnodes\
ff02::2 ip6-allrouters" | kdb import user/tests/hosts/our hosts

echo "127.0.0.1       localhost\
127.0.1.1       computer\
\
# THEIR The following lines are desirable for IPv6 capable hosts\
::1     ip6-localhost ip6-loopback\
fe00::0 ip6-localnet\
ff00::0 ip6-mcastprefix\
ff02::1 ip6-allnodes\
ff02::2 ip6-allrouters" | kdb import user/tests/hosts/their hosts

kdb cmerge user/tests/hosts/our user/tests/hosts/their user/tests/hosts/base user/tests/hosts/result

kdb meta-get user/tests/hosts/result/ipv6/ip6-localhost comment/#1
#>  OUR The following lines are desirable for IPv6 capable hosts
```

See how for the comments the `our`version was preserved.


### Arrays

cmerge uses LibGit2 to handle arrays in an efficient manner. Without it 


```sh
echo "one\
two\
three\
four\
five" | kdb import user/tests/arrays/original line

echo "previous\
one\
two\
three\
four\
five" | kdb import user/tests/arrays/changed line

kdb cmerge -f user/tests/arrays/changed user/tests/arrays/original user/tests/arrays/original user/tests/arrays/result

kdb get user/tests/arrays/result/#0
#> previous
```



## elektra-install-config-files
use the same file base name, but from a different folder!
pay attention when mounting that mounting into user/ is strange
use sudo for mounting
example call is
elektra-install-config-files system/lines ~/programming/elektra_initiative/stuff/data/artificial/exampletext/new/test line
elektra-install-config-files system/lines ~/programming/elektra_initiative/stuff/data/artificial/exampletext/test line
