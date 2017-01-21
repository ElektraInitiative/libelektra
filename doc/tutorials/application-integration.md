# Introduction #

In Elektra different forms of application integrations are possible:

1. A lightweight integration where only configuration files are
   integrated in a global key database.
   This will not be discussed here, if you are interested about this
   [please continue reading about mounting](/doc/tutorials/mount.md)
2. Integration techniques without modifying the applications.
   This will also not be discussed here, if you are interested read
   - [Intercept Environment](/src/bindings/intercept/env/README.md)
   - [Intercept File System](/src/bindings/intercept/fs/README.md)
3. Integration where applications directly use Elektra to read and
   store settings.

In this tutorial we will only discuss (3).

For full integration the application needs to be patched
to directly access Elektra's key database.
When the application is fully integrated in Elektra's ecosystem
additional benefits arise:

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
  - and all other features [the plugins provide](/src/plugins/)
- Other applications can use your configuration as override or as
    fallback

## Elektrify ##

The process to make applications aware of other's configuration is called
"to elektrify". This tutorial is both for new and existing applications.

As first step, places where configuration is parsed or generated must be located.
Afterwards, Elektra's data structures must be used instead at these
locations. Before we are going to describe exactly how to do this, we will describe
some possibilities you will have to keep all the good things your previous
configuration system had.

Put shortly: you can keep code you want within Elektra as plugins.
Then not only your own application will use this code to access configuration,
but every other application participating in Elektra's ecosystem, too.
Doing this, the syntax of the configuration file stays exactly
the same as it was before. You can keep the same validation as you
had before. Nevertheless, the application immediately profits from
Elektra's infrastructure solving basic issues like getting configuration
from other parts of the system, update and conflict detection, and
resolving of the file name. In particular we gain a lot because
every other program can also access the configuration of your software.

If you do not have the code or want to get rid of it, a huge variety of
[already implemented plugins](/src/plugins)
can be utilised to extend the functionality of the configuration
system.  There are plenty of plugins that parse and generate
configuration files in many formats, do syntactic checks, do
notifications (e.g. via dbus), and write out events in their log files.

New applications simply do not have the burden to stay compatible with
the configuration system they had to before.  So they will prefer
to use more standard plugins and contribute to make them flawless.
But they can also use self-written plugins for adding needed behavior
or cross-cutting concerns.

To sum up, if a developer wants to **elektrify** software, he or she can
do that without any need for changes to the outside world regarding the
format and semantics of the configuration.  But in the interconnected
world it is only a matter of time until other software also wants to
access the configuration, and with elektrified software it is possible
for every application to do so.


## Get Started ##

As first step in a C-application you need to create an in-memory `Key`. Such a `Key` is
Elektra's atomic unit and consists of:

- a unique name
- a value
- metadata

`Key`s are either associated with entries in configuration files or used
as arguments in the API to transport some information.

Thus a key is only in-memory and does not need any of the other Elektra objects.
We always can create one (the tutorial will use the C-API, but it describes
quite general concepts useful for other languages in the same way):

```c
Key *parentKey = keyNew("/sw/org/myapp/#0/current", KEY_END);
```

- The first argument of `keyNew` is the name of the key.
 It consists of different parts, `/` is the hierarchy-separator:
  - `sw` is for software
  - `org` is a URL/organisation name to avoid name clashes with other
      application names. Use only one part of the URL/organisation,
      so e.g. `kde` is enough.
  - `myapp` is the name of the most specific component that has its own
      configuration
  - `#0` is the major version number of the configuration (to be incremented
    if you need to introduce incompatible changes).
  - `current` is the [profile](/src/plugins/profile/README.md)
      to be used. This is needed by administrators
      if they want to start up multiple applications with different
      configurations.
- `KEY_END` is needed because C needs a proper termination of variable
    length arguments.

The key name is standardized to make it easier to locate configuration.

- [Read more about key-functions in API doc.](http://doc.libelektra.org/api/current/html/group__key.html)
- [Read more about key names here.](/doc/help/elektra-key-names.md)


Now we have the `Key` we will use to pass as argument.
First we open our key database (KDB). This is done by:

```c
KDB *repo = kdbOpen(parentKey);
```

A `Key` is seldom alone, but they are often found in groups, as typical in
configuration files. To represent many keys (a set of keys) Elektra
has the data structure `KeySet`. Because the `Key`'s name is unique we
can easily lookup keys in a `KeySet` without ambiguity. Additionally, we
can can iterate over all `Key`s in a `KeySet` without a hassle.
To create an empty `KeySet` we use:

```c
KeySet *conf = ksNew(200, KS_END);
```

- 200 is an approximation for how many `Key`s we think we will have in
    the `KeySet` `conf`. It is for optimization purposes only.
- After the first argument we can list build-in keys that should be
  available in any case.
- The last argument needs to be `KS_END`.

Now we have everything ready to fetch the latest configuration:

```c
kdbGet(repo, conf, parentKey);
```

Note it is important for applications that the parentKey starts with a slash `/`.
Only then all keys of the so-called [namespace](/doc/help/elektra-namespaces.md)
are pulled in.
Such a name cannot physically exist in configuration files, but they are
the most important key names to actually work with configuration within
applications as we will see when introducing `ksLookup`.

## Lookup ##

To lookup a key, we simply use, e.g.:

```c
Key *k = ksLookupByName(conf,
	"/sw/org/myapp/#0/current/section/subsection/key",
	0);
```

We see in this example that only Elektra paths are hardcoded in
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

Finally, we need to convert the configuration value to the datatype we
need.

Obviously, to do this manually has severe drawbacks:

- names are hardcoded: might have typos or might be inconsistent
- tedious handling if key or value might be absent
- always calling `ksLookup` which gets tiresome for arrays
- converting to needed data type is error prone

So (larger) applications should not directly use `KeySet`, but
instead use code generation to provide a type-safe front-end.

For more information about that, continue reading
[here](https://github.com/ElektraInitiative/libelektra/tree/master/src/tools/gen).



## Specification ##

Now, we have a fully working configuration system without any hard-coded
information (such as configuration files). So we already gained something.
But, we did not discuss how we can actually achieve application integration,
the goal of Elektra.

Elektra 0.8.11 introduces the so called specification for the
application's configuration. It is located below its own [namespace](/doc/help/elektra-namespaces.md)
`spec` (next to user and system).

Keys in `spec` allow us to specify which keys are read by the application,
which fallback they might have and which is the default value using
metadata. The implementation of these features happened in `ksLookup`.
When cascading keys (those starting with `/`) are used following features
are now available (in the metadata of respective `spec`-keys):

- `override/#`: use these keys *in favour* of the key itself (note that
    `#` is the syntax for arrays, e.g. `#0` for the first element,
    `#_10` for the 11th and so on)
- `namespace/#`: instead of using all namespaces in the predefined order,
    one can specify which namespaces should be searched in which order
- `fallback/#`: when no key was found in any of the (specified) namespaces
    the `fallback`-keys will be searched
- `default`: this value will be used if nothing else was found

They can be used like this:

```sh
kdb set /overrides/test "example override"
sudo kdb setmeta spec/test override/#0 /overrides/test
```

This technique provides complete transparency how a program will fetch a configuration
value. In practice that means that:

```sh
kdb get "/sw/org/myapp/#0/current/section/subsection/key"
```

, will give you the *exact same value* as the application gets when it uses the
above lookup C-code.

What we do not see in the program above are the default values and
fallbacks. They are only present in the so-called specification (namespace `spec`).
Luckily, the specification consists of (meta) key/value pairs, too. So we do not have
to learn something new.

So lets say, that another application `otherapp` has exactly the
value we actually want. We want to improve the integration. In the case that we do not have a value for `/sw/org/myapp/#0/current/section/subsection/key`,
we want to use `/sw/otherorg/otherapp/#0/current/section/subsection/key`.

So we specify:

```sh
kdb setmeta spec/sw/org/myapp/#0/current/section/subsection/key \
    "fallback/#0" /sw/otherorg/otherapp/#0/current/section/subsection/key
```

Voila, we have done a system integration between `myapp` and `otherapp`!

Note that the fallback, override and cascading works on *key level*,
and not like most other systems have implemented, on configuration *file level*.

To make this work within your application make sure to always call
`ksLookup` before using a value from Elektra.


## Conclusion ##

Elektra does not hardcode any configuration data in your application.
Using the `default` specification, we even can startup applications without
any configuration file *at all* and still do not have anything hardcoded
in the applications binary.
Furthermore, by only using cascading keys for `kdbGet()` and `ksLookup()`
Elektra gives you the possibility to specify how configuration data
should be retrieved. In this specification you can define that
configuration data from other applications or shared places should
be considered or even preferred. Doing so, we can achieve configuration
integration.

## SEE ALSO ##

- [for advanced techniques e.g. transformations](http://www.libelektra.org/ftp/papers/kps2015sharing.pdf)
