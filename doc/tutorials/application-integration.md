# Introduction

In Elektra different forms of application integrations are possible:

1. A lightweight integration where configuration files are
   integrated in a global key database.
   This will not be discussed here, if you are interested
   [please continue reading about mounting](/doc/tutorials/mount.md)
2. Integration techniques without modifying the applications.
   This will also not be discussed here, if you are interested please read:
   - [Intercept Environment](/src/bindings/intercept/env/README.md)
   - [Intercept File System](/src/bindings/intercept/fs/README.md)
3. Integration where applications directly use Elektra to read and
   store settings.
   1. Using the low-level API.
   2. Using the high-level API.

In this tutorial we will discuss (3.1), i.e., how to
extend an application to directly access Elektra’s
key database. If you are new to Elektra, we recommend you familiarize
yourself with the basic concepts using this guide, but when it comes
to elektrifying your application (3.2) is mostly likely the better option.
So take a look at [how to use the high-level API](highlevel.md).

When the application is fully integrated in Elektra’s ecosystem
following benefits arise:

- Benefits that shared libraries have, e.g.
  - All applications profit from fixes, optimization and new features
  - Less memory consumption, because the libraries executable
    instructions are only loaded once
  - Faster development time, because many non-trivial problems (e.g.
    OS-dependent resolving of configuration file names with atomic
    updates) are already solved and tested properly
- The administrator can choose:
  - the configuration file syntax (e.g. XML or JSON)
  - notification and logging on configuration changes
  - defaults on absence of values using specifications
  - and all other features [that plugins provide](/src/plugins/)
- The parsing result is guaranteed to be the same because the same
  parser will be used.
- Other applications can use your configuration as override or as
  fallback (see below)

## Elektrify

We call the process of making applications aware of other's configuration
"to elektrify". This tutorial is suited both for new and existing applications.

As first step, locate places where configuration is parsed or generated.
Afterwards, use Elektra’s data structures instead at these locations.
Before we are going to describe how to do this, we will describe
some possibilities to keep all advantages your previous configuration
system had.

You can keep code you want within Elektra as plugins. This allows your
application, and other applications participating in Elektra’s ecosystem to
access your configuration. Doing this, the syntax of the configuration file
stays the same as before. You can keep the same validation as you had before.
The application profits from Elektra’s infrastructure solving basic issues like
getting configuration from other parts of the system, update and conflict
detection, and resolving of the file name. In particular we gain a lot because
every other program can also access the configuration of your software.

If you do not have the code or want to get rid of it, you can use a variety of
[already implemented plugins](/src/plugins) to extend the functionality of the
configuration system. There are plenty of plugins that parse and generate
configuration files in different formats, do syntactic checks, do
notifications (e.g. via dbus), and write out events in their log files.

New applications do not have the burden to stay compatible with
the configuration system they had to before. So they will prefer
to use more standard plugins and contribute to make them flawless.
But they can also use self-written plugins for adding needed behavior
or cross-cutting concerns.

To sum up, if a developer wants to **elektrify** software, he or she can
do that without any need for changes to the outside world regarding the
format and semantics of the configuration. In the interconnected
world it is a matter of time until other software also wants to
access the configuration, and with elektrified software it is possible
for every application to do so.

## Get Started

As first step in a C-application you need to create an in-memory `Key`. Such a
`Key` is Elektra’s atomic unit and consists of:

- a unique name
- a value
- metadata

`Key`s are either associated with entries in configuration files or used
as arguments in the API to transport some information.

Thus a key is in-memory and does not need any of the other Elektra objects.
We always can create one (the tutorial will use the C-API, but it describes
general concepts useful for other languages in the same way):

```c
Key *parentKey = keyNew("/sw/org/myapp/#0/current", KEY_END);
```

- The first argument of `keyNew` is the name of the key.
  It consists of different parts, `/` is the hierarchy-separator:
  - `sw` is for software
  - `org` is a URL/organization name to avoid name clashes with other
    application names. Use only one part of the URL/organization,
    so e.g. `kde` is enough.
  - `myapp` is the name of the most specific component that has its own
    configuration
  - `#0` is the major version number of the configuration (increment
    if you need to introduce incompatible changes).
  - `current` is the [profile](/src/plugins/profile/README.md)
    to use. Administrators need it
    if they want to start up applications with different configurations.
- `KEY_END` as C needs a proper termination of variable
  length arguments.

The key name is standardized to make it easier to locate configuration.

- [Read more about key-functions in API doc.](https://doc.libelektra.org/api/latest/html/group__key.html)
- [Read more about key names here.](/doc/help/elektra-key-names.md)

Now we have the `Key` we will use to pass as argument.
First we open our key database (KDB):

```c
KDB *repo = kdbOpen(parentKey);
```

A `Key` is seldom alone, but they are often found in groups, as typical in
configuration files. To represent many keys (a set of keys) Elektra
has the data structure `KeySet`. Because the `Key`'s name is unique we
can lookup keys in a `KeySet` without ambiguity. Furthermore, we
can iterate over all `Key`s in a `KeySet` without a hassle.
To create an empty `KeySet` we use:

```c
KeySet *conf = ksNew(200, KS_END);
```

- 200 is an approximation for how many `Key`s we think we will have in
  the `KeySet` `conf`, intended for optimization purposes.
- After the first argument we can list built-in keys that should be
  available in any case.
- The last argument needs to be `KS_END`.

Now we have everything ready to fetch the latest configuration:

```c
kdbGet(repo, conf, parentKey);
```

Note it is important for applications that the parentKey starts with a slash `/`.
This ensures pulling in all keys of the so-called [namespace](/doc/help/elektra-namespaces.md).
Such a name cannot physically exist in configuration files, but they are
the most important key names to actually work with configuration within
applications as we will see when introducing `ksLookup`.

## Lookup

To lookup a key, we use:

```c
Key *k = ksLookupByName(conf,
	"/sw/org/myapp/#0/current/section/subsection/key",
	0);
```

We see in this example that only Elektra paths are hard coded in
the application, no configuration file or similar.

As already mentioned keys starting with slash `/` do not exist
in configuration files, but are "representatives", "proxies" or
"logical placeholders" for keys from any other [namespace](/doc/help/elektra-namespaces.md).

So that every tool has a consistent view to the key database
it is vital that every application does a `ksLookup` for every
key it uses. So even if your application iterates over keys,
always remember to do a [cascading](cascading.md) lookup for every single key!

Thus we are interested in the value we use:

```c
char *val = keyString(k);
```

We need to convert the configuration value to the data type we
need.

To do this manually has severe drawbacks:

- hard coded names might have typos or might be inconsistent
- tedious handling if key or value might be absent
- always calling `ksLookup` which gets tiresome for arrays
- converting to needed data type is error-prone

So (larger) applications should not directly use `KeySet`, but
instead use code generation to provide a type-safe frontend.

For more information about that, continue reading
[here](https://master.libelektra.org/src/tools/pythongen).

## Specification

Now, we have a fully working configuration system without any hard coded
information (such as configuration files). We already gained something.
But, we did not discuss how we can actually achieve application integration,
the goal of Elektra.

Elektra 0.8.11 introduces the so-called specification for the
application's configuration, located below its own [namespace](/doc/help/elektra-namespaces.md)
`spec`. The specification itself also consists of (meta) key-value pairs.

Keys in `spec` allow us to specify which keys the application reads,
which fallback they might have and which is the default value using
metadata.

### Links

The implementation of links are in `ksLookup`.
When using cascading keys (those starting with `/`), the following features
are now available (in the metadata of respective `spec`-keys):

- `override/#`: use these keys _in favor_ of the key itself (note that
  `#` is the syntax for arrays, e.g. `#0` for the first element,
  `#_10` for the 11th and so on)
- `namespace/#`: instead of using all namespaces in the predefined order,
  one can specify namespaces to search in a given order
- `fallback/#`: when no key was found in any of the (specified) namespaces
  the `fallback`-keys will be searched
- `default`: the value to use if nothing else was found

You can use those features like following:

```sh
kdb set /overrides/test "example override"
sudo kdb meta set spec:/test override/#0 /overrides/test
```

This technique provides complete transparency how a program will fetch a
configuration value. In practice that means that:

```sh
kdb get "/sw/org/myapp/#0/current/section/subsection/key"
```

, will give you the _exact same value_ as the application gets when it uses the
above lookup C code.

What we do not see in the program above are the default values and
fallbacks. They are also present in the specification (namespace `spec`).

So lets say, that another application `otherapp` has the
value we actually want. We want to improve the integration. In the case that we
do not have a value for `/sw/org/myapp/#0/current/section/subsection/key`,
we want to use `/sw/otherorg/otherapp/#0/current/section/subsection/key`.

So we specify:

```sh
kdb meta set spec:/sw/org/myapp/#0/current/section/subsection/key \
    "fallback/#0" /sw/otherorg/otherapp/#0/current/section/subsection/key
```

Voila, we have done a system integration between `myapp` and `otherapp`!

Note that the fallback, override and cascading works on _key level_,
and not like most other systems have implemented, on configuration _file level_.

To make this work within your application make sure to always call
`ksLookup` before using a value from Elektra.

### Specfiles

We call the files, that contain a complete schema for configuration
below a specific path in form of metadata, _Specfiles_.

Particularly a _Specfile_ contains metadata that defines

- the mount points of paths,
- the plugins to load and
- the behavior of these plugins.

(note that the `\\` are due to [Markdown Shell Recorder][], do not copy them to your shell)
[markdown shell recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

```sh
sudo kdb mount tutorial.ecf spec:/sw/org/myapp/#0/current"
cat << HERE | kdb import spec:/sw/org/myapp/#0/current ni  \
[]                                         \
 mountpoint = my-config-file.ini           \
 infos/plugins = ini validation            \
                                           \
[section/subsection/key]                   \
fallback/#0=/sw/otherorg/otherapp/#0/current/section/subsection/key  \
description = A description of the key     \
HERE
kdb meta-ls spec:/sw/org/myapp/#0/current # verify if specification is present now
#> infos/plugins
#> mountpoint
```

Now we apply this _Specfile_ to the key database to all keys below
`/sw/org/myapp/#0/current`:

```sh
kdb spec-mount /sw/org/myapp/#0/current
```

Then the configuration of our application will be in my-config-file.ini
(because of `mountpoint` in the specification) and it will use the INI
format (because of `infos/plugins` in the specification).
`section/subsection/key` contains the specification of what we
already specified imperatively before.

For a description which metadata is available, have a look in
[METADATA.ini](/doc/METADATA.ini).

## Conclusion

Elektra does not hard code any configuration data in your application.
Using the `default` specification, we even can startup applications without
any configuration file _at all_ and still do not have anything hard coded
in the applications binary.
Furthermore, by using cascading keys for `kdbGet()` and `ksLookup()`
Elektra gives you the possibility to specify how to retrieve configuration data.
In this specification you can define to consider or prefer configuration data
from other applications or shared places. Doing so, we can achieve configuration
integration.

## See Also

- [how to validate configuration with the specification](validation.md)
