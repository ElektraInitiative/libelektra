# Mounting

Elektra provides a global key database, that can integrate configuration in various formats.

Conversely configuration managed by Elektra can be integrated into applications.
The best way of integrating Elektra into applications is to [elektrify](/doc/help/elektra-glossary.md) them.

A simpler form of integration is to let Elektra directly use configuration files as they are present on the system.
Thus applications can read the configuration files and changes in the key database will be picked up by applications.

The heart of the approach is the so called _mounting_ of configuration files into the key database.

Let us start with a motivating example first:

## Mount the Lookup Table for Hostnames

We mount the lookup table with the following command:

```sh
sudo kdb mount --with-recommends /etc/hosts system/hosts hosts
```

1. `/etc/hosts` is the configuration file we want to mount
2. `system/hosts` is the path it should have in the key database, also known as **mount point**
3. `hosts` is the _storage plugin_ that can read and write this configuration format.

> Consider using mount with the option `--with-recommends`, which loads all plugins recommended by the _hosts_ plugin.
> You can see the recommended plugins of _hosts_ if you look at the output of `kdb plugin-info hosts`.
> Hosts recommends the _glob_, _network_ and _error_ plugins.
> Using `--with-recommends`, more validation is done when modifying keys in `system/hosts`.

Now we use `kdb file`, to verify that all configuration below `system/hosts` is stored in `/etc/hosts`:

```sh
kdb file system/hosts
#> /etc/hosts
```

After mounting a file, we can modify keys below `system/hosts`.
We need to be root, because we modify `/etc/hosts`.

```sh
sudo kdb set system/hosts/ipv4/mylocalhost 127.0.0.33
```

These changes are reflected in `/etc/hosts` instantly:

```sh
cat /etc/hosts | grep mylocalhost
#> 127.0.0.33	mylocalhost
```

Applications will now pick up these changes:

```sh
ping -c 1 mylocalhost
# RET:2
```

We are also safe against wrong changes:

```sh
sudo kdb set system/hosts/ipv4/mylocalhost ::1
# RET:5
# ERROR:51
sudo kdb set system/hosts/ipv4/mylocalhost 300.0.0.1
# RET:5
# ERROR:51
```

We can undo these changes with:

```sh
# remove the key ...
sudo kdb rm system/hosts/ipv4/mylocalhost

# ... and unmount
sudo kdb umount system/hosts
```

> ###### Why do you Need Superuser Privileges to Mount Files?
>
> Elektra manages its mount points in configuration below **system/elektra/mountpoints**.
> The file that holds this configuration is, in the same way as `/etc/hosts` before, only writable by administrators:
>
>     $ kdb file system/elektra/mountpoints
>     /etc/kdb/elektra.ecf
>
> Because of that only root can mount files.

## Resolver

The configuration file path you supplied to `kdb mount` above is actually not an
absolute or relative path in your file system, but gets resolved to one by Elektra.
The plugin that is responsible for this is the [_Resolver_](/src/plugins/resolver/README.md).

When you mount a configuration file the resolver first looks at the namespace of
your mount point. Based on that namespace and if the supplied path was relative or
absolute the resolver then resolves the supplied path to a path in the file system.
The resolving happens dynamically for every `kdb` invocation.

You can display the mounted configuration files with `kdb mount`.
Also here you only see the unresolved paths.

If you supplied an absolute path (e.g. `/example.ini`) it gets resolved to this:

| namespace | resolved path         |
| --------- | --------------------- |
| `spec`    | `/example.ini`        |
| `dir`     | `${PWD}/example.ini`  |
| `user`    | `${HOME}/example.ini` |
| `system`  | `/example.ini`        |

If you supplied a relative path (e.g. `example.ini`) it gets resolved to this:

| namespace | resolved path                                  |
| --------- | ---------------------------------------------- |
| `spec`    | `/usr/share/elektra/specification/example.ini` |
| `dir`     | `${PWD}/.dir/example.ini`                      |
| `user`    | `${HOME}/.config/example.ini`                  |
| `system`  | `/etc/kdb/example.ini`                         |

If this differs on your system, the resolver has a different configuration.
Type `kdb plugin-info resolver` for more information about the resolvers.

There are different resolvers. For instance on non-POSIX systems paths must be resolved differently.
In this case one might want to use the [wresolver](/src/plugins/wresolver/README.md) plugin.
Another useful resolver is the [blockresolver](/src/plugins/blockresolver/README.md), which integrates only a block of a configuration file into Elektra.

But resolvers are not the only plugins Elektra uses:

## Plugins

Configuration files can have many different formats (`ini`, `json`, `yaml`, `xml`, `csv`, ... to name but a few).

One of the goals of Elektra is to provide users with a unified interface to all those formats.
Elektra accomplishes this task with _storage plugins_.

> In Elektra [Plugins](/doc/tutorials/plugins.md) are the units that encapsulate functionality.
> There are not only plugins that handle storage of data, but also plugins that modify your values ([iconv](/src/plugins/iconv/README.md)).
> Furthermore there are plugins that validate your values ([validation](/src/plugins/validation/README.md), [mathcheck](/src/plugins/mathcheck/README.md), ...), log changes in the key set ([logchange](/src/plugins/logchange/README.md)) or do things like executing commands on the shell ([shell](/src/plugins/shell/README.md)).
> You can get a complete list of all available plugins with `kdb plugin-list`.
> Although an individual plugin does not provide much functionality, plugins are powerful because they are designed to be used together.

When you mount a file you can tell Elektra which plugins it should use for reading and
writing to configuration files.

Let us mount a projects git configuration into the dir namespace:

```sh
# create a directory for our demonstration
mkdir -p example && cd $_

# this creates the .git/config file
git init

# mount git’s configuration into Elektra
sudo kdb mount /.git/config dir/git ini multiline=0
```

As git uses the `ini` format for its configuration we use the [ini plugin](/src/plugins/ini/README.md).
You can pass parameters to plugins during the mount process. This is what
we did with `multiline=0`. Git indents the entries in its configuration
files and the default behavior of the `ini` plugin is to interpret these indented
entries as values that span multiple lines. The passed parameter disables
this behavior and makes the ini-plugin compatible with git configuration.

Now let us see how smoothly the ini plugin sets and gets the git configuration.

```sh
# set a user name ...
git config user.name "Rob Banks"

# ... and read it with kdb
kdb get dir/git/user/name
#> Rob Banks

# set a user email with kdb ...
kdb set dir/git/user/email "rob.banks@dot.com"

# and read it with git
git config --get user.email
#> rob.banks@dot.com
```

#### Meta Data

Elektra is able to store [metadata](/doc/help/elektra-metadata.md) of keys, provided the format of the file that holds the configuration supports this feature.
The ini plugin does support this feature, and so does the [ni](/src/plugins/ni/README.md) and the [dump](/src/plugins/dump/README.md) plugin among others.

> Actually the ini plugin creates some metadata on its own. This metadata contains information about the ordering of keys or comments, if a key has some.
> But unlike the ni and the dump plugin we can't store arbitrary metadata with the ini plugin.

Meta data comes in handy if we use other plugins, than just the ones that store and retrieve data.
I chose the `ni` plugin for this demonstration, because it supports metadata and is human readable.
So let us have a look at the [type](/src/plugins/type/README.md) and [mathcheck](/src/plugins/mathcheck/README.md) plugins.

```sh
# mount the backend with the plugins ...
kdb mount example.ni user/example ni type

# ... and set a value for the demonstration
kdb set user/example/enumtest/fruit apple
#> Create a new key user/example/enumtest/fruit with string "apple"
```

By entering `kdb plugin-info type` in the commandline, we can find out how to use this plugin.
It turns out that this plugin allows us to define a list of valid values for our keys via the metavalue `check/enum`.

```sh
kdb meta-set user/example/enumtest/fruit check/type enum
kdb meta-set user/example/enumtest/fruit check/enum "#2"
kdb meta-set user/example/enumtest/fruit check/enum/#0 apple
kdb meta-set user/example/enumtest/fruit check/enum/#1 banana
kdb meta-set user/example/enumtest/fruit check/enum/#2 grape
kdb set user/example/enumtest/fruit tomato
# RET:5
# this fails because tomato is not in the list of valid values
```

You can have a look or even edit the configuration file with `kdb editor user/example ni` to see how the value and metadata is stored:

```ini
enumtest/fruit = apple

[enumtest/fruit]
check/type = enum
check/enum = #2
check/enum/#0 apple
check/enum/#1 banana
check/enum/#2 grape
```

The example shows an important problem: the configuration file is now changed in ways that might not be acceptable for applications.
We have at least two ways to avoid that:

1. Encode metadata as comments
2. Encode metadata in its own `spec` namespace, completely separate to the configuration files the application will see

If you want to find out more about validation I recommend reading [this](/doc/tutorials/validation.md) tutorial next.

#### Backends

The plugins together with the configuration file form a _backend_. The backend determines how Elektra stores data below a mount point.
You can examine every mount points backend by looking at the configuration below `system/elektra/mountpoints/<mount point>/`.

## Limitations

One drawback of this approach is, that an application can bypass Elektra and change configuration files directly. If for example Elektra is configured to [validate](/doc/tutorials/validation.md) new configuration values before updating them, this is something you do not want to happen.

Another drawback is that mounting is static. In a previous example we mounted the `/.git/config` file into `dir/git`. Now the `dir` namespace of every directory stores the configuration below `dir/git` in this directories `/.git/config` file. And this mount point is the same for all users and all directories.
So you can't have different configuration files for the same mount points in other directories.
Because of the same reason you cannot have different configuration file names or syntax for the same mount point in the `user` namespace.

This is one of the reasons why Elektra promotes this [naming convention](/doc/help/elektra-key-names.md) for keys:

> Key names of software-applications should always start with:
> `/<type>/<org>/<name>/<version>/<profile>`

> - **type** can be `sw` (software), `hw` (hardware) or `elektra` (for internal configuration)
> - **org** is an URL/organization name. E.g. `kde`
> - **name** the name of the component that has this configuration
> - **version** is the major version number. E.g. If you version is 6.3.8 than this would be `#6`
> - **profile** is the name of the profile to be used. E.g.: `production`, `development`, `testing`, ...

Furthermore, one cannot simply change the configuration file format, because it must be one the application understands. Thus one loses quite some flexibility (for instance if this file format doesn't support meta keys, as already mentioned).

These limitations are the reasons why [elektrifing](/doc/help/elektra-glossary.md) applications provides even better integration.
Go on reading [how to elektrify your application](application-integration.md).
