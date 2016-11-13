# Mounting #

Elektra provides a global key database, that can integrate configuration in various formats.

Conversely configuration managed by Elektra can be integrated into applications.
The best way of integrating Elektra into applications is to [elektrify](/doc/help/elektra-glossary.md) them.

A simpler form of integration is to synchronize Elektras configuration with configuration files.
Thus applications can read the configuration files and changes in the key database will be picked up by applications.

The heart of the approach is the so called _mounting_ of configuration files into the key database.

Let us start with a motivating example first:

## Mount the lookup table for hostnames

We mount the lookup table with the following command:

    sudo kdb mount /etc/hosts system/hosts hosts

1. `/etc/hosts` is the configuration file we want to mount
2. `system/hosts` is the path it should have in the key database, also known as **mountpoint**
3. `hosts` is the _storage plugin_ that can read and write this configuration format.

> Consider using mount with the option `--with-recommends`, which loads all plugins recommended by the _hosts_ plugin.
> You can see the recommendet plugins of _hosts_ if you look at the output of `kdb info hosts`. Hosts recommends the _glob_ and _network_ plugins.

Now we use `kdb file`, to verify that all configuration below `system/hosts` is stored in /etc/hosts:

    $ kdb file system/hosts
    /etc/hosts

After mounting a file, we can modify keys below `system/hosts`.
We need to be root, because we modify `/etc/hosts`.

    $ sudo kdb set system/hosts/ipv4/mylocalhost 127.0.0.33

These changes are reflected in `/etc/hosts` instantly:

    $ cat /etc/hosts
    ...
    mylocalhost 127.0.0.33       localhost
    ...

Applications will now pick up these changes:

    $ ping mylocalhost

We can undo these changes with:

    # remove the key ...
    $ kdb rm system/hosts/ipv4/mylocalhost

    # ... and unmount
    $ sudo kdb umount system/hosts

> ###### Why you need superuser privileges to mount files ######
> Elektra manages its mountpoints in configuration below **system/elektra/mountpoints**.
> The file that holds this configuration is, in the same way as `/etc/hosts` before, only writable by administrators:
>
>     $ kdb file system/elektra/mountpoints
>     /etc/kdb/elektra.ecf
>
> Because of that only root users can mount files.

## Resolver ##

The configuration file path you supply to `kdb mount` is actually not an
absolute or relative path in your filesystem, but gets resolved to one by Elektra.
The plugin that is responsible for this is the [_Resolver_](src/plugins/resolver/README.md).

When you mount a configuration file the resolver first looks at the namespace of
your mountpoint. Based on that namespace and if the supplied path was relative or
absolute the resolver than resolves the supplied path to a path in the file system.

If you supplied an absolute path (e.g. `/example.ini`) it gets resolved to this:

| Namespace        | resolved Path       |
| ---------------- |-------------------- |
| spec             | /example.ini        |
| dir              | ${PWD}/example.ini  |
| user             | ${HOME}/example.ini |
| system           | /example.ini        |

If you supplied a relative path (e.g. `example.ini`) it gets resolved to this:

| Namespace        | resolved Path               |
| ---------------- |---------------------------- |
| spec             | /usr/share/elektra/specification/example.ini |
| dir              | ${PWD}/.dir/example.ini     |
| user             | ${HOME}/.config/example.ini |
| system           | /etc/kdb/example.ini        |
(If this differs on your system, the resolver has a different configuration. You
can then check the default paths below `system/elektra/modules/resolver/constants`)

When you display the mounted configuration files with `kdb mount` you also see the unresolved paths.

There are different resolvers. For instance on non-POSIX systems paths must be resolved differently.
In this case one might want to use the [wresolver](src/plugins/wresolver/README.md) plugin.
Another useful resolver is the [blockresolver](src/plugins/blockresolver/README.md), which integrates only a block of a configuration file into Elektra.

But resolvers are not the only plugins Elektra uses:

## Plugins ##

Configuration files can have many different formats (ini, json, yaml, xml, csv, ... to name but a few).

One of the goals of Elektra is to provide users with a unified interface to all those formats.
Elektra accomplishes this task with _storage plugins_.

> In Elektra [Plugins](doc/tutorials/plugins.md) are the units that encapsulate functionality.
> There are not only plugins that handle storage of data, but also plugins that modify your values ([iconv](src/plugins/iconv/README.md)).
> Furthermore there are plugins that validate your values ([validation](src/plugins/validation/README.md), [enum](src/plugins/enum/README.md), [boolean](src/plugins/boolean/README.md), [mathcheck](src/plugins/mathcheck/README.md), ...), log changes in the key set ([logchange](src/plugins/logchange/README.md)) or do things like executing commands on the shell ([shell](src/plugins/shell/README.md)).
> Although an individual plugin does not provide much functionality, plugins are powerful because they are designed to be used together.

When you mount a file you can tell Elektra which plugins it should use for reading and
writing to configuration files.

Let us mount a projects git configuration into the dir namespace:

    # create a directory for our demonstration
    mkdir example && cd $_

    # this creates thie .git/config file
    git init

    # mount gits configuration into Elektra
    sudo kdb mount /.git/config dir/git ini multiline=0

As git uses the ini format for its configuration we use the [ini plugin](src/plugins/ini/README.md).
You can pass parameters to plugins during the mount process. This is what
we did with `multiline=0`. Git intends the entries in its configuration
files and the default behaviour of the ini plugin is to interpret these indented
entries as values that span mutiple lines. The passed parameter disables
this behaviour and makes the ini-plugin compatible with git configuration.

Now let us see how smoothly the ini plugin sets and gets the git configuration.

    # set a user name ...
    git config user.name "Rob Banks"

    # ... and read it with kdb
    kdb get dir/git/user/name
    Rob Banks

    # set a user email with kdb ...
    kdb set dir/git/user/email "rob.banks@dot.com"

    # and read it with git
    git config --get user.email
    rob.banks@dot.com


#### Meta data ####

// TODO: work on this subchapter:
[] find nice example
[] link to validation.md

Elektra is able to store [meta data](doc/help/elektra-metadata.md) of keys, provided the format of the file that holds the configuration supports this feature.
The ini plugin doesn't support this feature, but the [ni](src/plugins/ni/README.md) and the [dump](src/plugins/dump/README.md) plugin do.
Meta data comes in handy if we use other plugins, than just the ones that store and retrieve data:

    // TODO
    $ sudo kdb mount example.ni user/example ni enum iconv
    $ kdb setmeta /example/fruit check/enum "'apple', 'banana', 'grape'"
    $ kdb set /example/vegetable apple
    # works
    $ kdb set /example/vegetable tomato
    # error


#### Backends ####
The plugins together with the configuration file form a _backend_. The backend determines how Elektra stores data below a mountpoint.

You can examine every mountpoints backend by looking at the configuration below `system/elektra/mountpoints/<mountpoint>/`.

## Limitations ##

One drawback of this approach is, that an application can bypass Elektra and change configuration files directly. If for example Elektra is configured to [validate](/doc/tutorial/validation.md) new configuration values before updating them, this is something you do not want to happen.

Another drawback is that mounting is static. In a previous example we mounted the `/.git/config` file into `dir/git`. Now the `dir` namespace of every directory stores the configuration below `dir/git` in this directories `/.git/config` file. And this mountpoint is the same for all users.
So you can't have different configuration files for the same mountpoints in other directories.
Because of the same reasons, if you mount a configuration file in the `user` namespace, this configuration file will be at the same location (relative to the users home directory) for every user.

Furthermore one cannot simply change the configuration file format, because it must be one the application understands. Thus one loses quite some flexibility (for instance if this file format doesn't support meta keys).
