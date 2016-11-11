# Mounting #

Elektra provides a global key database where configuration of all applications can be integrated.

There are different forms of integration. In this tutorial we will discuss a light form of integration that can be achieved without [elektrifying](/doc/help/elektra-glossary.md) applications.

The idea is to synchronize Elektras configuration with configuration files. Thus applications can read the configuration files and changes in the key database will be picked up by applications.

The heart of the approach is the so called _mounting_ of configuration files into the key database.

Let us start with a motivating example first:

## Mount the lookup table for hostnames

We mount the lookup table with the following command:

    sudo kdb mount /etc/hosts system/hosts hosts

1. `/etc/hosts` is the configuration file we want to mount
2. `system/hosts` is the path it should have in the key database, also known as **mountpoint**
3. `hosts` is the _storage plugin_ that can read and write this configuration format.

Without arguments `kdb mount` lists the currently mounted backends.

    $ kdb mount
    /etc/hosts on system/hosts with name system/hosts

Which verifies, that all configuration below `system/hosts` is stored in /etc/hosts.

After mounting a file, we can modify keys below `system/hosts`.
We need to be root, because we modify `/etc/hosts`.

    sudo kdb set system/hosts/ipv4/mylocalhost 127.0.0.33

These changes are reflected in `/etc/hosts` instantly:

    $ cat /etc/hosts
    ...
    mylocalhost 127.0.0.33       localhost
    ...

Applications will now pick up these changes:

    $ ping mylocalhost

###### Why you need superuser privileges to mount files ######
Elektra manages its mountpoints in configuration below **system/elektra/mountpoints**.
The file that holds this configuration is per default only writable by administrators:

    $ kdb file system/elektra/mountpoints
    /etc/kdb/elektra.ecf

Because of that only root users can mount files.

> At the moment a workaround for this is to add your user
> to a group that can modify Elektras configuration file.
>
>     $ sudo groupadd elektra
>     $ KDB_SYSTEM=$(dirname $(kdb file system))
>     $ sudo chown -R :elektra $(dirname $(kdb file system))
>     $ sudo chmod -R g+w $(dirname $(kdb file system))
>
> Bear in mind that users in this group con modify all configuration of Elektra
> that is stored in files the user is permitted to write.

## Resolver ##

The configuration file path you supply to `kdb mount` is actually not an
absolute or relative path in your filesystem, but gets resolved to one by Elektra.
The plugin that is responsible for this is the _Resolver_.

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
can then check the default paths under `system/elektra/modules/resolver/constants`)

## Plugins ##

Configuration files can have many different formats (ini, json, yaml, xml, csv, ... to name but a few).

One of the goals of Elektra is to provide users with a unified interface to all those formats.
Elektra accomplishes this task with _storage plugins_.
When you mount a file you can tell Elektra which plugins it should use for reading and
writing to configuration files.

Lets mount a projects git configuration into the dir namespace:

    # create a directory for our demonstration
    mkdir example && cd $_

    # this creates thie .git/config file
    git init

    # mount gits configuration into Elektra
    sudo kdb mount /.git/config dir/git ini multiline=0

As git uses the ini format for its configuration we use the ini plugin.
You can pass parameters to plugins during the mount process. This is what
we did with `multiline=0`. Git intends the entries in its configuration
files and the default behaviour of the ini plugin is to interpret these indented
entries as values that span mutiple lines. With the passed parameter disable
this behaviour and make the ini-plugin compatible to git.

Alternatively we could have mounted another file format:

    $ sudo kdb mount /package.json dir/example yajl iconv from=UTF-8,to=ISO-8859-1

// TODO


## Limitations ##

One drawback of this approach is, that an application can bypass Elektra and change configuration files directly. If for example Elektra is configured to [validate](/doc/tutorial/validation.md) new configuration values before updating them, this is something you do not want to happen.

Furthermore one cannot simply change the configuration file format, because it must be one the application understands. Thus one loses quite some flexibility.

    ```
    mkdir exampleapp && cd $_
    git init
    sudo kdb mount /.git/config dir/git ini multiline=0


    kdb mount /etc/fstab

    kdb ls -0 system/filesystems | xargs -0 -I% sh -c "echo '%'; kdb get '%'"
    kdb mount /etc/fstab system/filesystems fstab struct type path
    ```
