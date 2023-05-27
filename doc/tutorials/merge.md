# How-To: Merging

## Introduction

Elektra takes the semantic aspects of configuration files into account when merging them into one result file.
Those configuration files are represented as key sets in Elektra. Key sets are collections of name-value pairs.

The `kdb` tool allows users to access and perform functions on the Elektra key database from the command line. We added
a new command to this very useful tool, the `merge` command. This command allows a user to perform a three-way merge
of key sets from the `kdb` tool.

The syntax to use this tool is:

`kdb merge [OPTIONS] our their base result`

`our`, `their` and `base` represent the three keys that are used in
a three-way merge. As in the `diff3` tool, required are
the three files MYFILE (our), OLDFILE (base) and YOURFILE (their)
The result off the three-way merge
will be stored in `result`.

## Simple example

The easiest case is if all three versions contain equal data.

```sh
kdb set user:/tests/base a
#> Create a new key user:/tests/base with string "a"
kdb set user:/tests/their a
#> Create a new key user:/tests/their with string "a"
kdb set user:/tests/our a
#> Create a new key user:/tests/our with string "a"
kdb merge user:/tests/our user:/tests/their user:/tests/base user:/tests/result
kdb get user:/tests/result
#> a
```

We change the key for another example.

```sh
kdb set user:/tests/our b
#> Set string to "b"
```

Using a `result` path that is not empty gives an error.
The option `-f` can be used to override. **Attention!** This deletes existing keys below `result`.

```sh
kdb merge user:/tests/our user:/tests/their user:/tests/base user:/tests/result
# RET: 3
# There are keys in the result path. Use -f to override them.
kdb merge -f user:/tests/our user:/tests/their user:/tests/base user:/tests/result
kdb get user:/tests/result
#> b
```

We can use the same key multiple times in a single call to merge.

```sh
kdb set user:/tests/same a
#> Create a new key user:/tests/same with string "a"
kdb merge -f user:/tests/same user:/tests/same user:/tests/same user:/tests/result
kdb get user:/tests/result
#> a
```

### hosts

As a real-world example, we import three different (see the comment) versions of a hosts file.

```
echo "127.0.0.1       localhost
127.0.1.1       computer

# BASE The following lines are desirable for IPv6 capable hosts
::1     ip6-localhost ip6-loopback
fe00::0 ip6-localnet
ff00::0 ip6-mcastprefix
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters" | kdb import user:/tests/hosts/base hosts

echo "127.0.0.1       localhost
127.0.1.1       computer

# OUR The following lines are desirable for IPv6 capable hosts
::1     ip6-localhost ip6-loopback
fe00::0 ip6-localnet
ff00::0 ip6-mcastprefix
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters" | kdb import user:/tests/hosts/our hosts

echo "127.0.0.1       localhost
127.0.1.1       computer

# THEIR The following lines are desirable for IPv6 capable hosts
::2     ip6-localhost ip6-loopback
fe00::0 ip6-localnet
ff00::0 ip6-mcastprefix
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters" | kdb import user:/tests/hosts/their hosts

kdb merge user:/tests/hosts/our user:/tests/hosts/their user:/tests/hosts/base user:/tests/hosts/result
```

The merge notices that only one of the three versions of the key `ip6-localhost` has changed.
Assuming that this was an update it puts the new value in the result.

```
kdb get user:/tests/hosts/result/ipv6/ip6-localhost
#> ::2
```

## Metadata

Metadata gets merged as well.
We do not follow a complicated approach for this topic.
When a key-value pair is chosen from the three versions to be present in the result it takes all its metadata with it.

In case that the values of some keys are equal, the `our` version wins and consequently the metadata of the `our` version is used. The reason for this is that users might have used the metadata for personal comments.

To demonstrate this, we continue the hosts example:

<!--- Some ini tests fail with multiline metadata => no sh => no Markdown shell
recorder tests -->

```
kdb meta-get user:/tests/hosts/result/ipv6/ip6-localhost comment/#2
#>  THEIR The following lines are desirable for IPv6 capable hosts
```

We set up some keys:

```sh
kdb set user:/tests/meta/base equal
#> Create a new key user:/tests/meta/base with string "equal"
kdb meta-set user:/tests/meta/base comment/#0 "This is the original inline comment"
kdb meta-set user:/tests/meta/base comment/#1 "This is the first line of the original comment above the key"
kdb meta-set user:/tests/meta/base comment/#2 "This is the second line of the original comment above the key"

kdb set user:/tests/meta/their equal
#> Create a new key user:/tests/meta/their with string "equal"
kdb meta-set user:/tests/meta/their comment/#0 "This is their inline comment"
kdb meta-set user:/tests/meta/their comment/#1 "This is the first line of their comment above the key"
kdb meta-set user:/tests/meta/their comment/#2 "This is the second line of their comment above the key"

kdb set user:/tests/meta/our equal
#> Create a new key user:/tests/meta/our with string "equal"
kdb meta-set user:/tests/meta/our comment/#0 "This is your custom inline comment"
kdb meta-set user:/tests/meta/our comment/#1 "This is the first line of your custom comment above the key"
kdb meta-set user:/tests/meta/our comment/#2 "This is the second line of your custom comment above the key"

kdb merge user:/tests/meta/our user:/tests/meta/their user:/tests/meta/base user:/tests/meta/metaFromOur
```

Now we can check if the metadata has been merged as expected.

<!--- Some ini tests fail with multiline metadata => no sh => no Markdown shell
recorder tests -->

```
kdb meta-get user:/tests/meta/metaFromOur comment/#0
#> This is your custom inline comment
kdb meta-get user:/tests/meta/metaFromOur comment/#1
#> This is the first line of your custom comment above the key
kdb meta-get user:/tests/meta/metaFromOur comment/#2
#> This is the second line of your custom comment above the key
```

If a key is part of the result because its value has changed then the result will also contain the metadata of that key.

```sh
kdb set user:/tests/meta/their different
#> Set string to "different"

kdb merge user:/tests/meta/our user:/tests/meta/their user:/tests/meta/base user:/tests/meta/metaFromChanged
```

We can test again if the result meets our expectations.

<!--- Some ini tests fail with multiline metadata => no sh => no Markdown shell
recorder tests -->

```
kdb meta-get user:/tests/meta/metaFromChanged comment/#2
#> This is the second line of their comment above the key
```

## Arrays

merge uses LibGit2 to handle arrays in an efficient manner.

```sh
echo "one\
two\
three\
four\
five" | kdb import user:/tests/arrays/original line

echo "previous\
one\
two\
three\
four\
five" | kdb import user:/tests/arrays/changed line

kdb merge -f user:/tests/arrays/changed user:/tests/arrays/original user:/tests/arrays/original user:/tests/arrays/result

kdb get user:/tests/arrays/result/#0
#> previous
```

## Scripts

There are two tools of which merge is the central tool:

1. [`kdb install-config-file`](/doc/help/kdb-install-config-file.md) installs or merges configuration files from the file system into
   Elektra. There is [a tutorial](/doc/tutorials/install-config-files.md) for this tool, too.
2. [`kdb merge-config-files`](/doc/help/kdb-merge-config-files) performs a three-way merge on three files using Elektra

## Calling the API

All the tools that use the merge library rely on the same API.
An exemplary call to this API can be found in the [examples folder](/examples/kdbset.c).
