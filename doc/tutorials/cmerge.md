# How-To: kdb cmerge

## Introduction

The kdb tool allows users to access and perform functions on the Elektra key database from the command line. We added
a new command to this very useful tool, the `cmerge` command. This command allows a user to perform a three-way merge
of key sets from the `kdb` tool.

The command to use this tool is:

`kdb cmerge [OPTIONS] our their base result`

`our`, `their` and `base` represent the three keys that are used in
a three-way merge. Compared to the `diff3` tool, this would be
the three files MYFILE (our), OLDFILE (base) and YOURFILE (their)
that are the required for it. The result off the three-way merge
will be stored in `result`.

For configuration files in the context of package upgrades:

- `their` is the current version of the maintainer's copy,
- `base` is the previous version of the maintainer's copy.
- `our` is the user's copy, derived from `base`

## Options

The options of `kdb cmerge`are:

- `-f`, `--force`: overwrite existing keys in `result`
- `-v`, `--verbose`: give additional information

Strategies offer fine grained control over conflict handling. The option is:

- `-s <name>`, `--strategy <name>`: which is used to specify a strategy to use in case of a conflict

## Strategies

The available strategies are:

- `abort`: the merge will abort if any conflict happens and merge the 3 key sets together otherwise.
- `our`: This option forces conflicting keys to be auto-resolved cleanly by favoring `our`. Changes from the other key sets that do not conflict with the `our` version are reflected to the merge result. This works like the recursive strategy with the `ours` option from git-merge.
- `their`: This is the opposite of `our`. The merge will use the `their` version when a conflict happens.

If no strategy is specified, the merge will default to the abort strategy.

## Return value

0 on success.

1 if a conflict or any other error could not be resolved automatically.

The result of the merge is stored in `result`.

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

Using a `result` path that is not empty gives an error.
The option `--force` can be used to override. **Attention!** This deletes existing keys below `result`.

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

Merging is also possible in more complicated situations.
We import three different (see the comment) versions of a hosts file. Please remove the trailing backslashes from the example code.

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
::2     ip6-localhost ip6-loopback\
fe00::0 ip6-localnet\
ff00::0 ip6-mcastprefix\
ff02::1 ip6-allnodes\
ff02::2 ip6-allrouters" | kdb import user/tests/hosts/their hosts

kdb cmerge user/tests/hosts/our user/tests/hosts/their user/tests/hosts/base user/tests/hosts/result
kdb get user/tests/hosts/result/ipv6/ip6-localhost
#> ::2
```

### Meta data

We continue with the hosts example. During a merge meta data strictly belongs to the key. Note that all the meta data of the changed key is in the result as well.

```sh
kdb meta-get user/tests/hosts/result/ipv6/ip6-localhost comment/#1
#>  THEIR The following lines are desirable for IPv6 capable hosts
```

In case that the values of some keys are equal, the meta data of the `our` version is used. The goal is to preserve a user's personal notes for a key.

```sh
kdb set user/tests/meta/original irrelevant
#> Create a new key user/tests/meta/original with string "irrelevant"
kdb set user/tests/meta/changed irrelevant
#> Create a new key user/tests/meta/changed with string "irrelevant"
kdb meta-set user/tests/meta/original something original
kdb meta-set user/tests/meta/changed something changed
kdb cmerge user/tests/meta/changed user/tests/meta/original user/tests/meta/original user/tests/meta/result
kdb meta-get user/tests/meta/result something
#> changed
```

### Arrays

cmerge uses LibGit2 to handle arrays in an efficient manner.

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

You can use the script `elektra-install-config-files` to comfortably handle your configuration files with Elektra. This script install or merges configuration files from the file system into Elektra. There are two possible scenarios:

1. You have never used the script for a file. In this case the file is mounted into Elektra and additionally stored in a special path to preserve the original version. You may safely modify your configuration.
2. You have already used the script for a previous version of the file. In this case a three-way merge will happen. The newly selected file will be regarded as `their` artifact and the currently mounted file is regarded as the `our` version. We use the archived version from step 1 as `base` version.
   The script uses the file's name is as its unique identifier. It is therefore important that different versions of your file have exactly the same name. You can store it in different folders to perform a merge. You can, for example, mount a file from your home directory. Afterwards, you download a new version of the file from the internet and store it in `~/Downloads`. This already gives you the required structure.

When specifying the parameters for the script, pay attention that it uses the regular `kdb mount`. This that you have to pay attention when mounting into `user/` or to use `sudo`.
