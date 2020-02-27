# Command-line Options

## Introduction

Many applications use command-line options and environment variables as a way to override configuration values.
In Elektra this can be automated by providing a specification that maps command-line options and environment variables
to keys in the KDB.

The function `elektraGetOpts` uses this specification together with `argv` and `envp` and creates new keys in the `proc`
namespace for any command-line option or environment variable it finds. Because the `proc` namespace is used, these values
will be preferred over any stored values in a cascading lookup.

There is also the `gopts` plugin. It is a global plugin and should be mounted via `kdbEnsure`. This plugin extracts `argv`
and `envp` via OS specific APIs and then calls `elektraGetOpts`. The advantage of using `gopts` is that it retrieves
command-line options before any validation plugins are called. This means that values passed on the command-line can be
validated.

## Setup

To use `elektraGetOpts` you need to link against `elektra-opts`, `elektra-meta` and `elektra-ease`.

## Options

To define a command-line option either set the `opt` metakey to the short option you want to use, or set `opt/long` to
the long option you want to use. For short options, only the first character of the given value will be used ('\0' is ignored).
Short and long options can be used simultaneously.

Additionally a key can also be associated with multiple short/long options. To achieve this treat `opt` as an array.
For example for two options `-a` and `-b` you would set `opt=#1`, `opt/#0=a` and `opt/#1=b`. If not explicitly stated
otherwise, you can replace `opt` with any `opt/#` array element in all meta-keys mentioned in this document. This of course
includes long options (i.e. `opt/#0/long`, etc.).

While you can specify multiple options (or environment variables, see below) for a single key, only one of them can be used
at the same time. Using two or more options (or variables) that are all linked to the same key, will result in an error.

### Option Arguments

Per default an option is expected to have an argument. Arguments to short and long options are given in the same way
as with `getopt_long(3)` (i.e. `-oarg`, `-o arg`, `--option arg` or `--option=arg`).

To change whether an option expects an argument set `opt/arg` to either `"none"` or `"optional"` (the default is `"required"`).

- If you choose `"none"`, the corresponding key will be set to `"1"`, if the option is used. This value can be changed
  by setting `opt/flagvalue`.
- An option that is set to `"optional"` is treated the same as with `"none"`, except that you can also set the value
  with the long option form `--option=value`. This also means that `opt/flagvalue` is used, if no argument is given.
  Contrary to `getopt_long(3)` options with optional arguments can still have short forms. They just cannot have an
  argument in this form.

## Environment Variables

Elektra also supports parsing environment variables in a similar manner. For these there are however, less configuration
options. You can simply specify one or more environment variables for a key using the `env` metakey (or `env/#` meta-array
for multiple).

## Arrays

Both options and environment variables expose special behavior, if used in combination with arrays.

If an option is specified on a key with basename `#`, the option can be used repeatedly. All occurrences will be collected
into the array.

Environment variables obviously cannot be repeated, instead a behavior similar that used for PATH is adopted. On Windows
the variable will be split at each ';' character. On all other systems ':' is used as a separator.

## Parameter Arguments

All unused elements of `argv` are be collected into an array. You can access this array by specifying `args=remaining` on a
key with basename `#`. The array will be copied into this key. As is the case with getopt(3) processing of options will stop,
if `--` is encountered in `argv`.

If `elektraGetOpts` is called with a `parentKey` that has `posixly = 1` set, then processing of options will also stop, when
a non-option argument is encountered. This is similar to the POSIX version of getopt(3).

Additionally, there is `args=indexed`. If it is specified on a key, the key must also have the metakey `args/index=N` set to
an integer `N`. Such a key will be set to the unused element at index `N`. If a key has `args=indexed and`args/index=N`, then there must also be keys for all integers`0 <= X < N`with`args=indexed`and`args/index=N`set. For example you cannot use`args/index=0`and`args/index=2`without`args/index=1`.

Combining `args=indexed` and `args=remaining` in the same specification (on different keys) is also possible. The key with
`args=remaining` will only contain those elements to used via `args=indexed`.

### Example

```ini
[from]
args = indexed
args/index = 0

[to]
args = indexed
args/index = 1

[more/#]
args = remaining
```

If an application `app` with the specification above is called as `./app apple banana cherry date`, then the keys will be
assigned as follows:

- `from = apple`
- `to = banana`
- `more/#0 = cherry`
- `more/#1 = date`

## Sub-Commands

`elektraGetOpts` also supports sub-commands. For example calling `git -p add` and `git add -p` result in different behaviour.
This is because the options that `git` understands are separate from the options that its sub-command `add` knows. However,
the option `-p` is understood by both. In `git` it is short for `--paginate` and in `add` it is short for `--patch`.

The first thing you need to know about sub-commands is that they imply `posixly = 1` mode. This means **all** options must
be given before any non-option arguments (such as parameters or sub-commands). Otherwise, we couldn't distinguish between
`git -p add` and `git add -p`.

A sub-command is created by specifying `command` on a key. To enable sub-command processing the parent key of the whole
specification must have `command` set to an empty string. All keys marked with `command` **directly** below another
key `K` marked with `command` (e.g. the parent key) are sub-commands of `K`. It is an error, if the immediate parent of
a key `X` marked with `command` is not marked with `command` and `X` is not the parent of the whole specification.

To inform the application about the invoked sub-commands, `elektraGetOpts` sets each `command` key to one of two values:

- All invoked sub-commands `S` will be set to `XYZ`, where `XYZ` is the sub-command of `S` that was invoked.
- Any other sub-command is set to an empty string.

For example consider `./app add more`: The parent key will be set to `add`, the key for `add` will be set to `more` and
the key for `more` is set to an empty string, because none of its sub-commands were invoked.

Invoking a non-existent sub-command does not result in an error. Instead, the relevant `command` keys are assigned as
described above and then `elektraGetOpts` returns, without further processing.

Every key considered by `elektraGetOpts` is assigned either to the root command, or a single sub-command. Specifically,
each key is assigned to the command of its immediate parent. If sub-commands are used and the immediate parent of an `opt`
or `args` key has no `command` metadata an error occurs. The value of a key will **only** be set by `elektraGetOpts`, if
the corresponding sub-command was invoked.

Lastly, it is allowed to have keys with `args` and `command` below the same parent. If a matching sub-command is found among
the `command` keys, processing will continue there. Otherwise, the `args` keys will be considered. This allows an application
to implement dynamic commands (like `git` or `kdb`) by using the `args=remaining` array to invoke another application.

All of this is best understood with an example:

```ini
[kdb]
command = ""

[kdb/printversion]
type = boolean
opt = v
opt/long = version
opt/arg = none

[kdb/get]
command = get

[kdb/get/verbose]
type = boolean
opt = v
opt/long = verbose
opt/arg = none

[kdb/get/keyname]
type = string
args = indexed
args/index = 0

[kdb/set]
command = set

[kdb/set/verbose]
type = boolean
opt = v
opt/long = verbose
opt/arg = none

[kdb/dynamic/#]
type = string
args = remaining
```

- If we invoke `kdb -v`, keys below `kdb/get`, `kdb/set` and `kdb/dynamic` are not touched. The result is:
  - `kdb = ""`
  - `kdb/printversion = 1`
  - `kdb/get = ""`
  - `kdb/set = ""`
- If we invoke `kdb get -v name`, keys below `kdb/set` and `kdb/dynamic` are not touched. The result is:
  - `kdb = get`
  - `kdb/get = ""`
  - `kdb/get/verbose = 1`
  - `kdb/get/keyname = name`
  - `kdb/set = ""`
- If we invoke `kdb -v set -v`, keys below `kdb/get` and `kdb/dynamic` are not touched. The result is:
  - `kdb = set`
  - `kdb/printversion = 1`
  - `kdb/set = ""`
  - `kdb/set/verbose = 1`
  - `kdb/get = ""`
- If we invoke `kdb -v custom -v -x z`, keys below `kdb/get` and `kdb/set are not touched. The result is:
  - `kdb = ""`
  - `kdb/printversion = 1`
  - `kdb/dynamic/#0 = custom`
  - `kdb/dynamic/#1 = -v`
  - `kdb/dynamic/#2 = -x`
  - `kdb/dynamic/#3 = z`

## Help Message

When the help option `--help` is encountered in `argv`, `elektraGetOpts` only reads the specification, but does not
create any keys in the `proc` namespace. It will however, generate a help message that can be accessed with
`elektraGetOptsHelpMessage`.

The help message consists of a usage line and an options list. The program name for the usage line is taken from `argv[0]`.
If the value contains a slash (`/`) it will be considered a path and only the part after the last slash will be used.

The options list will contain exactly one entry for each key that has at least one option. Each entry has to parts. First
all the options for the key are listed and then (possibly on the next line, if there are a lot of options), the description
for the key is listed. The description is taken from the `opt/help` or alternatively the `description` metakey.

**Note:** `opt/help` is specified _only once per key_. That means even if the key uses `opt/#0`, `opt/#1`, etc. (unlike most
other metadata) the description will always be taken from `opt/help` directly, because there can only be one description. In
general we recommend using `description`, because it is used by other parts of Elektra as well. `opt/help` is intended to
provide a less verbose description more suitable for the command-line.

The help message can be modified in a few different ways:

- The `usage` argument of `elektraGetOptsHelpMessage` can be used to replace the default usage line.
- The `prefix` argument of `elektraGetOptsHelpMessage` can be used to insert text between the usage line and the options list.
- An option can can be hidden from the help message by setting `opt/hidden` to `"1"`. This hides both the long and short form
  of the option. If you want to hide just one form, use an array of two options an hide just one index.
- If the option has an `"optional"` or `"required"` argument, the string `ARG` will be used as a placeholder by default. You
  can change this, by setting `opt/arg/help` for the corresponding option.

### Sub-Commands

If sub-commands are in use, the help message returned by `elektraGetOptsHelpMessage` will apply to the invoked sub-command only.
For example `./app --help` generates the general help message for `./app` containing only options valid for the root command.
In contrast, `./app more --help` generates the help message for the sub-command `more`.

## Precedence

The order of precedence is simple:

- If a short option for a key is found, it will always be used.
- If none of the short options for a key are found, we look for long options.
- Neither short nor long options are found, environment variables are considered.

## Limitations

- Both options and environment variables can only be specified on a single key. If you need to have the
  value of one option/environment variable in multiple keys, you may use `fallback`s.
- `-` cannot be used as short options, because it would collide with, the "option end marker".
- `help` cannot be used as a long option, because it would collide with the help option.

## Examples

The following specification describes the command line interface similar to the one used by `rm`. (It is based of `rm (GNU coreutils) 8.30`).

```ini
[force]
opt = f
opt/long = force
opt/arg = none
description = ignore nonexistent files and arguments, never prompt

[interactive]
opt = #1
opt/#0 = i
opt/#0/long = interactive
opt/#0/arg = optional
opt/#0/flagvalue = always
opt/#1 = I
opt/#1/flagvalue = once
opt/#1/arg = none
opt/arg/name = WHEN
description = prompt according to WHEN: never, once (-I), or always (-i); without WHEN, prompt always

[singlefs]
opt/long = one-file-system
opt/arg = none
description = when removing a hierarchy recursively, skip any directory that is on a file system different from that of the corresponding line argument

[nopreserve]
opt/long = no-preserve-root
opt/arg = none
description = do not treat '/' specially

[preserve]
opt/long = preserve-root
opt/arg = optional
opt/arg/name = all
opt/flagvalue = root
description = do not remove '/' (default); with 'all', reject any command line argument on a separate device from its parent

[recursive]
opt = #1
opt/#0 = r
opt/#0/long = recursive
opt/#0/arg = none
opt/#1 = R
opt/#1/arg = none
description = remove directories and their contents recursively

[emptydirs]
opt = d
opt/long = dir
opt/arg = none
description = remove empty directories

[verbose]
opt = v
opt/long = verbose
opt/arg = none
env = VERBOSE
description = explain what is being done

[showversion]
opt/long = version
opt/arg = none
description = output version information and exit

[files/#]
args = remaining
env = FILES
description = the files that shall be deleted
```

If this specification is used in a program called `erm` (for Elektra rm), which is called like this:

```
FILES="one.txt:other.log" VERBOSE=1 erm -fi --recursive
```

The following keys will be created by `elektraGetOpts` (assuming the specification is mounted at `spec/sw/org/erm/#0/current`):

- `proc/sw/org/erm/#0/current/force = "1"`
- `proc/sw/org/erm/#0/current/interactive = "always"`
- `proc/sw/org/erm/#0/current/recursive = "1"`
- `proc/sw/org/erm/#0/current/verbose = "1"`
- `proc/sw/org/erm/#0/current/files [array] = "#1"`
- `proc/sw/org/erm/#0/current/files/#0 = "one.txt"`
- `proc/sw/org/erm/#0/current/files/#1 = "other.log"`

Calling `FILES="abcd.txt" erm 123.txt 456.txt` meanwhile will result in:

- `proc/sw/org/erm/#0/current/files [array] = "#1"`
- `proc/sw/org/erm/#0/current/files/#0 = "123.txt"`
- `proc/sw/org/erm/#0/current/files/#1 = "456.txt"`

NOTE: `proc/sw/org/erm/#0/current/files [array] = "#1"` means the `array` metadata of `proc/sw/org/erm/#0/current/files` is `#1`.

You can find a full working example [here](/examples/opts.c). However, it uses a hard coded
specification which is a bit harder to read.
