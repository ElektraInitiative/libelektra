# Command-line Options

## Introduction

Many applications use command-line options and environment variables as a way to override configuration values.
In Elektra, this can be automated by providing a specification that maps command-line options and environment variables to keys in the KDB.

The recommended way to do this is via the `gopts` plugin.
This plugin internally calls the actual parser `elektraGetOpts`.
However, since there are some downsides to calling the parser manually, we cannot generally recommend doing so.
If you think you have a use case for calling `elektraGetOpts` directly, take a look at the section [Advanced Use: Calling `elektraGetOpts` directly](#elektra-opts) below.

The parser uses a specification together with `argc`/`argv` and a list of environment variables to create keys in the `proc:/` namespace.
Because the keys are in the `proc:/` namespace, they will be preferred over all other namespaces in a standard cascading lookup.
This allows us to use command-line options or environment variables to override standard configuration keys.

## Setup

While you could manually mount and configure `gopts`, it is _not recommended_ doing so.
Instead, you should use `elektraGOptsContract` to create a contract for use with `kdbOpen()`.
This contract ensures that `gopts` is automatically mounted and correctly configured.
To use `elektraGOptsContract`, include `kdbgopts.h`. This gives you access to these two functions:

```c
int elektraGOptsContract (KeySet * contract, int argc, const char * const * argv, const char * const * envp, const Key * parentKey, KeySet * goptsConfig);

int elektraGOptsContractFromStrings (KeySet * contract, size_t argsSize, const char * args, size_t envSize, const char * env, const Key * parentKey, KeySet * goptsConfig);
```

Whenever possible, we recommend that you use `elektraGOptsContract` since it has less memory and processing overhead than `elektraGOptsContractFromStrings`.
However, to use `elektraGOptsContract` the pointers given for `argv` and `envp` must remain valid until after calling `kdbClose()`, because the `gopts` plugin will directly use these pointers.
In the standard use case (i.e. using the `argv` from `main()` and the global `environ` for `envp`), this restriction is not a problem.

An example for using `elektraGOptsContract` could look like this:

```c
extern char ** environ;

int main (int argc, char ** argv)
{
  Key * parentKey = keyNew ("/sw/org/example/#0/current", KEY_END);

  KeySet * contract = ksNew (0, KS_END);
  KeySet * goptsConfig = ksNew (0, KS_END);

  // error handling omitted for brevity
  elektraGOptsContract (contract, argc, argv, environ, parentKey, goptsConfig);

  KDB * kdb = kdbOpen (contract, parentKey);
}
```

If you cannot provide pointers that meet the requirements, you may use `elektraGOptsContractFromStrings`.
This function copies its arguments `args` and `env` into separate memory, so the pointers need only be valid for the duration of the function call.
This is mainly useful for language bindings, since in many programming languages manual memory management is not possible and there is no (easy) way to ensure the `argv` and `envp` pointers meet the necessary requirements.

The `gopts` plugin can also use operating system specific functions to retrieve the command-line arguments and environment variables internally.
We recommend that you do not rely on this behavior whenever possible, since it can be a bit flaky (especially for command-line arguments).
However, if for example you are writing a library or for some other reason do not have access to the necessary data, you can use this fallback.

If you pass `argc=0` **and** `argv=NULL` to `elektraGOptsContract` or `argsSize=0` **and** `args=NULL` to `elektraGOptsContractFromStrings`, `gopts` will fallback to the internal lookup of command-line options.
Similarly, if you pass `envp=NULL` to `elektraGOptsContract` or `envSize=0` **and** `env=NULL` to `elektraGOptsContractFromStrings`, environment variables will be retrieved internally.

The other parameters are the same for both functions.
The contract will be written into the `contract` KeySet.
The `parentKey` indicates where to find the specification.
The actual namespace that `parentKey` uses is irrelevant (we recommend a cascading key, so that it can be reused for `kdbGet()`).
The parser will use the keys below the `spec:/` namespace key equivalent to `parentKey` as the specification, and it will write keys to the equivalent key in `proc:/`.

The last parameter `goptsConfig` can be used to provide additional configuration values to `gopts`.
For example, this can be used to configure the auto-generated help message.
The keys that `gopts` accepts in this KeySet will be explained throughout the document.
A full list can be found in the [`gopts` README](../../src/plugins/gopts/README.md).

## Specification

This section describes the specification used by the command-line option and environment variable parser.

### Options

To define a command-line option either set the `opt` metakey to the short option you want to use, or set `opt/long` to the long option you want to use.
For short options, only the first character of the given value will be used ('\0' is ignored).
Short and long options can be used simultaneously.

Additionally, a key can also be associated with multiple short/long options.
To achieve this treat `opt` as an array.
For example for two options `-a` and `-b` you would set `opt=#1`, `opt/#0=a` and `opt/#1=b`.
If not explicitly stated otherwise, you can replace `opt` with any `opt/#` array element in all meta-keys mentioned in this document.
This of course includes long options (i.e. `opt/#0/long`, etc.).

While you can specify multiple options (or environment variables, see below) for a single key, only one of them can be used at the same time.
Using two or more options (or variables) that are all linked to the same key, will result in an error.

#### Option Arguments

Per default an option is expected to have an argument.
Arguments to short and long options are given in the same way as with `getopt_long(3)` (i.e. `-oarg`, `-o arg`, `--option arg` or `--option=arg`).

To change whether an option expects an argument set `opt/arg` to either `"none"` or `"optional"` (the default is `"required"`).

- If you choose `"none"`, the corresponding key will be set to `"1"`, if the option is used.
  This value can be changed by setting `opt/flagvalue`.
- An option that is set to `"optional"` is treated the same as with `"none"`, except that you can also set the value with the long option form `--option=value`.
  This also means that `opt/flagvalue` is used, if no argument is given.
  Contrary to `getopt_long(3)` options with optional arguments can still have short forms.
  They just cannot have an argument in this form.

### Environment Variables

Elektra also supports parsing environment variables in a similar manner.
For these there are however, less configuration options.
You can simply specify one or more environment variables for a key using the `env` metakey (or `env/#` meta-array for multiple).

### Arrays

Both, options and environment variables, expose special behavior, if used in combination with arrays.

If an option is specified on a key with basename `#`, the option can be used repeatedly.
All occurrences will be collected into the array.

Environment variables obviously cannot be repeated, instead a behavior similar to that used for PATH is adopted.
On Windows the variable will be split at each ';' character.
On all other systems ':' is used as a separator.

### Parameter Arguments

All unused elements of `argv` are collected into an array.
You can access this array by specifying `args=remaining` on a key with basename `#`.
The array will be copied into this key.
As is the case with getopt(3) processing of options will stop, if `--` is encountered in `argv`.

If we parse command-line options like the POSIX version of getopt(3) does, then we would also stop processing options at the first non-option argument.
This is not the case by default, but we can enable this behavior.
To do so, you need to pass a Key `/posixly` with value `1` in the `goptsConfig` KeySet of the `gopts` contract.

Additionally, there is `args=indexed`.
If it is specified on a key, the key must also have the metakey `args/index=N` set to an integer `N`.
Such a key will be set to the unused element at index `N`. If a key has `args=indexed` and `args/index=N`, then there must also be keys for all integers `0 <= X < N` with `args=indexed` and `args/index=N` set.
For example, you cannot use `args/index=0` and `args/index=2` without `args/index=1`.

Combining `args=indexed` and `args=remaining` in the same specification (on different keys) is also possible.
The key with `args=remaining` will only contain those elements not used via `args=indexed`.
For example, if there are keys with `args/index=0` and `args/index=1` then the `args=remaining` array will start with the third (index 2) parameter argument.
Note however, the `args=remaining` array **always** starts with index `#0`, even if it doesn't contain the first parameter argument.

#### Example

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

If an application `app` with the specification above is called as `./app apple banana cherry date`, then the keys will be assigned as follows:

- `from = apple`
- `to = banana`
- `more/#0 = cherry`
- `more/#1 = date`

### Sub-Commands

The parser also supports sub-commands.
Explaining sub-commands is easiest through the help of an example: `add` and `commit` are both sub-commands of `git`, since we can call `git add` and `git commit` and they do entirely different things.
The most important impact of using sub-commands is their effect on option arguments. For example calling `git -p add` and `git add -p` result in different behavior, since the `-p` option is interpreted differently.
The options that `git` understands are separate from the options that its sub-command `add` knows.
However, the option `-p` is understood by both.
In `git` it is short for `--paginate` and in `add` it is short for `--patch`.

An important thing to know about sub-commands is that they automatically turn on POSIX mode.
This means **all** options for a specific sub-command must be given before any non-option arguments (such as parameters or sub-commands).
Otherwise, we couldn't distinguish between `git -p add` and `git add -p`.
In other words, an option argument is always assigned to the first sub-command to its left.
Any element of `argv` that is not an argument for a sub-command, either switches to a new sub-command or is the start of the parameter arguments.

A sub-command is created by specifying `command` on a key.
To enable sub-command processing the parent key of the whole specification must have `command` set to an empty string. All keys marked with `command` **directly** below another key `K` marked with `command` (e.g. the parent key) are sub-commands of `K`.
It is an error, if the immediate parent of a key `X` marked with `command` is not marked with `command` and `X` is not the parent of the whole specification.

To inform the application about the invoked sub-commands, the parser sets each `command` key to one of two values:

- The basename of the key, whose command was invoked.
- An empty string otherwise.

For example consider `./app add more`: The parent key will be set to the basename of whatever key `command=add` was specified on, the key for `add` will be set to the basename corresponding to `more` and the key for `more` is set to an empty string, because none of its sub-commands were invoked.
A more detailed example is shown below.

Every key considered by the parser is assigned either to the root command, or a single sub-command.
Specifically, each key is assigned to the command of its immediate parent.
If sub-commands are used and the immediate parent of an `opt` or `args` key has no `command` metadata an error occurs.
The value of a key will **only** be set, if the corresponding sub-command was invoked.

Lastly, it is allowed to have keys with `args` and `command` below the same parent. If a matching sub-command is found among
the `command` keys, processing will continue there. Otherwise, the `args` keys will be considered. This allows an application
to implement dynamic commands (like `git` or `kdb`) by using the `args=remaining` array to invoke another application.

If an unknown sub-command is encountered without an `args` key, an error is returned.

All of this is best understood with an example:

```ini
[kdb]
command = ""

[kdb/printversion]
description = "print version information and exit (ignoring all other options/commands/parameters)"
opt = v
opt/long = version
opt/arg = none

[kdb/getter]
description = "get a key's value"
command = get

[kdb/getter/verbose]
description = "print additional information about where the value comes from"
opt = v
opt/long = verbose
opt/arg = none

[kdb/getter/keyname]
description = "name of the key to read"
args = indexed
args/index = 0

[kdb/setter]
description = "set a key's value"
command = set

[kdb/setter/verbose]
description = "print additional information about where the value will be stored"
opt = v
opt/long = verbose
opt/arg = none

[kdb/setter/keyname]
description = "name of the key to write"
args = indexed
args/index = 0

[kdb/setter/value]
description = "value to be written"
args = indexed
args/index = 1

[kdb/dynamic/#]
description = "dynamically call a user-supplied command"
args = remaining
```

- If we invoke `kdb -v`, keys below `kdb/getter`, `kdb/setter` and `kdb/dynamic` are not touched. The result is:
  - `kdb = ""`
  - `kdb/printversion = 1`
  - `kdb/getter = ""`
  - `kdb/setter = ""`
- If we invoke `kdb get -v name`, keys below `kdb/setter` and `kdb/dynamic` are not touched. The result is:
  - `kdb = getter`
  - `kdb/getter = ""`
  - `kdb/getter/verbose = 1`
  - `kdb/getter/keyname = name`
  - `kdb/setter = ""`
- If we invoke `kdb -v set -v`, keys below `kdb/getter` and `kdb/dynamic` are not touched. The result is:
  - `kdb = setter`
  - `kdb/printversion = 1`
  - `kdb/setter = ""`
  - `kdb/setter/verbose = 1`
  - `kdb/getter = ""`
- If we invoke `kdb -v custom -v -x z`, keys below `kdb/getter` and `kdb/setter` are not touched. The result is:
  - `kdb = ""`
  - `kdb/printversion = 1`
  - `kdb/getter = ""`
  - `kdb/setter = ""`
  - `kdb/dynamic/#0 = custom`
  - `kdb/dynamic/#1 = -v`
  - `kdb/dynamic/#2 = -x`
  - `kdb/dynamic/#3 = z`

To determine what code to execute, an application would just start with the parent key `kdb` in the example above. It would
then repeatedly look at the current key's value, append that to the current key and continue, until the current key's value
was the empty string `""`. Each of the examined keys corresponds to one of the sub-commands in the invocation and the keys
directly below those, contain the relevant options (and for the last sub-command also parameters).

The C code for this example is located in `examples/optsCommands.c`.

### Precedence

The order of precedence is simple:

- If a short option for a key is found, it will always be used.
- If none of the short options for a key are found, we look for long options.
- If neither short nor long options are found, environment variables are considered.

### Limitations

- Both options and environment variables can only be specified on a single key. If you need to have the value of one option/environment variable in multiple keys, you may use `fallback`s.
- `-` cannot be used as a short option, because it would collide with the "option end marker".
- `help` cannot be used as a long option, because it would collide with the help option.

## Help Message

When the help option `--help` is encountered in `argv`, the parser only reads the specification, but does not create any keys in the `proc:/` namespace.
It will however, generate a standard help message that you can print.

To find you, whether `--help` was encountered, check if the KeySet returned by `kdbGet()` contains the special key `proc:/elektra/gopts/help` with value `1`.
If it does, the auto-generated help message is stored in the key `proc:/elektra/gopts/help/message`.

```c
int main (int argc, char ** argv)
{
  // setup ommitted for brevity
  KeySet * ks = ksNew (0, KS_END);

  // error handling ommitted for brevity
  kdbGet (kdb, ks, parentKey);

  Key * help = ksLookupByName (ks, "proc:/elektra/gopts/help");
  if (help != NULL && strcmp (keyString (help), "1") == 0)
  {
    printf ("%s\n", keyString (ksLookupByName (ks, "proc:/elektra/gopts/help/message")));
    // cleanup omitted for brevity
    return 0;
  }
}
```

> **Note**: The key `proc:/elektra/gopts/help` will always be generated.
> Only if its value is set to `1`, the `--help` option was encountered.

### Structure of the Help Message

The help message consists of a usage line and an options list.
The program name for the usage line is taken from `argv[0]`.
If the value contains a slash (`/`) it will be considered a path and only the part after the last slash will be used.

The options list will contain exactly one entry for each key that has at least one option. Each entry has two parts. First
all the options for the key are listed and then (possibly on the next line, if there are a lot of options), the description
for the key is listed. The description is taken from the `opt/help` or alternatively the `description` metakey.

**Note:** `opt/help` is specified _only once per key_.
That means even if the key uses `opt/#0`, `opt/#1`, etc. (unlike most
other metadata) the description will always be taken from `opt/help` directly, because there can only be one description.
In general, we recommend using `description`, because it is used by other parts of Elektra as well.
`opt/help` is intended to provide a less verbose description more suitable for the command-line.

### Sub-Commands

If sub-commands are in use, the generated help message will apply to the invoked sub-command only.
For example `./app --help` generates the general help message for `./app` containing only options valid for the root command.
But `./app more --help` generates the help message for the sub-command `more` and contains options valid to this sub-command.

### Modifying the Help Message

The standard help message can be modified in a few different ways:

- The usage line can be replaced by a custom string (see below).
- A custom string can be inserted between the usage line and the options list (see below).
- An option can be hidden from the help message by setting `opt/hidden` to `"1"`.
  This hides both the long and short form of the option.
  If you want to hide just one form, use an array of two options and hide just one index.
- If the option has an `"optional"` or `"required"` argument, the string `ARG` will be used as a placeholder by default.
  You can change this, by setting `opt/arg/help` for the corresponding option.

#### Custom Usage Line

To use a custom usage line, you can either [manually generate the help message](#manual-help-message) or you can pass the key `/help/usage` in the `goptsConfig` KeySet.
The value of `/help/usage` will be used to replace the default usage line.

Please note, that this will replace the whole usage line including the program name taken from `argv[0]`.

#### Adding a Prefix Text

If you just want to add information above the options list, but not replace the whole usage line, you can do so by adding a prefix text.
This can be done by [manually generating the help message](#manual-help-message) or by passing the key `/help/prefix` in the `goptsConfig` KeySet.
The value of `/help/prefix` will be inserted between the usage line and the options list.

## Examples

The following specification describes a command line interface similar to the one used by `rm`. (It is based on `rm (GNU coreutils) 8.30`).

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
opt/#0/arg/help = WHEN
opt/#1 = I
opt/#1/flagvalue = once
opt/#1/arg = none
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
opt/arg/help = all
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

```bash
FILES="one.txt:other.log" VERBOSE=1 erm -fi --recursive
```

The following keys will be created (assuming the specification is mounted at `spec:/sw/org/erm/#0/current`):

- `proc:/sw/org/erm/#0/current/force = "1"`
- `proc:/sw/org/erm/#0/current/interactive = "always"`
- `proc:/sw/org/erm/#0/current/recursive = "1"`
- `proc:/sw/org/erm/#0/current/verbose = "1"`
- `proc:/sw/org/erm/#0/current/files [array] = "#1"`
- `proc:/sw/org/erm/#0/current/files/#0 = "one.txt"`
- `proc:/sw/org/erm/#0/current/files/#1 = "other.log"`

Calling `FILES="abcd.txt" erm 123.txt 456.txt` meanwhile will result in:

- `proc:/sw/org/erm/#0/current/files [array] = "#1"`
- `proc:/sw/org/erm/#0/current/files/#0 = "123.txt"`
- `proc:/sw/org/erm/#0/current/files/#1 = "456.txt"`

NOTE: `proc:/sw/org/erm/#0/current/files [array] = "#1"` means the `array` metadata of `proc:/sw/org/erm/#0/current/files` is `#1`.

You can find a full working example [here](/examples/gopts.c). However, it uses a hard coded specification which is a bit harder to read.

<a id="elektra-opts"></a>

## Advanced Use: Calling `elektraGetOpts` directly

The actual command line parser is implemented in `elektraGetOpts`.

```c
int elektraGetOpts (KeySet * ks, int argc, const char ** argv, const char ** envp, Key * parentKey);
```

To access this function, you first need to link your application against `libelektra-opts` and then include the header `kdbopts.h`.

> **Note:** `libelektra-opts` is an internal library and `kdbopts.h` an internal header.
> We do not make and guarantees to the API stability of the functions declared in `kdbopts.h`.
> We will do our best to keep the API compatible, but the only way to have guaranteed API stability and backwards compatibility is to only use the parser via `gopts`.

Calling `elektraGetOpts` directly has some disadvantages.
The main one being that there is no way to validate the values of command-line options.
When you use `gopts`, you can for example add the metadata `type=long` to a key with a command-line option specification and the `type` plugin will validate that the value generated by the command-line option parser is actually of type `long`.
But since `elektraGetOpts` has no way of delegating to plugins (as it is independent of `kdbGet`), you need to do all validation manually.

So why would you want to call `elektraGetOpts` directly?
The advantage of calling `elektraGetOpts` directly is that you have more control over where the specification comes from.
With `gopts` we need to mount the specification before starting our application (at least before calling `kdbOpen`).
But if we call `elektraGetOpts` directly, we can pass whatever KeySet we want.
This can be useful, if you are writing a custom configuration tool (like the standard `kdb` tool, but specific to your use case).
Such tools normally don't need (or want) to be configured via persistent config files, but often have an advanced command-line interface.
Using the fairly feature-rich parser implemented in `elektraGetOpts` could be a good option here.

<a id="manual-help-message"></a>

## Advanced Use: Manually Generating the Help Message

When using `gopts` you automatically get a generated help message via the key `proc:/elektra/gopts/help/message`, whenever the `--help` option is used.
But if you want to show the help message in another case, or you cannot use the auto-generated message for some reason (e.g. you want to add a prefix text, but the content of this text is not known before calling `kdbOpen()`), then you need to generate the help message manually.

If you are using `elektraGetOpts` directly, manually generating the help message is the only option.

The help message can be generated with `elektraGetOptsHelpMessage`.

```c
char * elektraGetOptsHelpMessage (Key * helpKey, const char * usage, const char * prefix);
```

To access this function, you first need to link your application against `libelektra-opts` and then include the header `kdbopts.h`.

> **Note:** `libelektra-opts` is an internal library and `kdbopts.h` an internal header.
> We do not make and guarantees to the API stability of the functions declared in `kdbopts.h`.
> We will do our best to keep the API compatible, but the only way to have guaranteed API stability and backwards compatibility is to only use the parser via `gopts`.

Calling `elektraGetOptsHelpMessage` allocates a new string that will contain the generated help message.
You need to free the string with `elektraFree`, once you are done with it.

The parameter `helpKey` has to be the same key as passed to `elektraGetOpts` as `parentKey`, if you called `elektraGetOpts` directly.
If you used `gopts`, you should pass the key `proc:/elektra/gopts/help`.

The parameter `usage` is used to replace the default usage line, if it is not `NULL`.
The string `prefix` is inserted between the usage line and the options list, if it is not `NULL`.

With `gopts` this could look something like this:

```c
int main (int argc, char ** argv)
{
  // setup omitted for brevity
  kdbGet (kdb, ks, parentKey);

  if (/* custom condition */)
  {
    Key * help = ksLookupByName (ks, "proc:/elektra/gopts/help", 0);
    char * helpMessage = elektraGetOptsHelpMessage (help, "custom usage", "custom prefix");
    printf ("%s\n", helpMessage);
    elektraFree (helpMessage);
  }
}
```
