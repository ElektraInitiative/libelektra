# kdb-mount-java(1) -- Mounting Java Plugins

## SYNOPSIS

`kdb mount-java [<path> <mount point>] [<plugin> [<config>] [..]]`

All options except `<plugin>` are passed directly to `kdb mount`.
For details on these options see [`kdb-mount(1)`](kdb-mount.md).

The `<plugin>` arguments are preprocessed before they are passed to `kdb mount`.
If a `<plugin>` argument starts with `java:`, it is interpreted as a fully-qualified Java class name.
The script replaces these `java:*` arguments with the appropriate arguments for `kdb mount` (see below).

`<plugin>` arguments that start with `kdb:` or have no prefix, are passed to `kdb mount` as is (after removing the optional `kdb:` prefix).
The `kdb:` prefix exists to avoid potential name collisions between a Java plugin and C plugin.

## DESCRIPTION

This helper command exists, because (unlike a "normal" C plugin) mounting a Java plugin requires multiple arguments.
This is because you actually need to mount the `process` plugin with the correct configuration for a Java plugin.

The following command mounts the C plugins `dump` and `type` together with the Java plugin `org.libelektra.plugin.WhitelistPlugin` with the file `config.file` at `user:/tests/process`:

```sh
sudo kdb mount-java config.file user:/tests/process kdb:dump java:org.libelektra.plugin.WhitelistPlugin type
```

This internally expands to (something like) this complicated snippet:

```sh
KDB_VERSION="$(kdb --version | sed -nE 's/KDB_VERSION: (.+)/\1/gp')"
sudo kdb mount config.file user:/tests/process dump process "executable=$(command -v java)" 'args=#3' 'args/#0=-cp' "args/#1=/usr/share/java/libelektra-$KDB_VERSION-all.jar:/usr/share/java/process-$KDB_VERSION.jar:/usr/share/java/whitelist-$KDB_VERSION.jar" 'args/#2=org.libelektra.process.PluginProcess' 'args/#3=org.libelektra.plugin.WhitelistPlugin' type
```

As you can see, every argument for a Java plugin is replaced by several arguments.
Even worse, the arguments depend on how Elektra is installed on your system.
The `mount-java` knows where Elektra's JARs are installed (`/usr/share/java` above) and constructs a classpath argument for the JVM.
It also chooses the `PluginProcess` class as the main class and finally instructs this class to load the `WhitelistPlugin`.

However, `mount-java` isn't magic and it only works this smoothly for plugins that come bundled with Elektra.
If you have external plugins, you must also specify the additional classpath for these plugins like this:

```sh
export CLASSPATH=/path/to/foo.jar:/path/to/bar.jar
sudo --preserve-env=CLASSPATH kdb mount-java config.file user:/tests/foobar java:org.example.Foo java:org.example.Bar java:org.libelektra.plugin.WhitelistPlugin
```

Here we added `/path/to/foo.jar` and `/path/to/bar.jar` to the classpath, so that the classes `org.example.Foo` and `org.example.Bar` can be found.
We can still use `org.libelektra.plugin.WhitelistPlugin`, because Elektra's JARs are always added to the classpath.

## SEE ALSO

- [kdb-mount(1)](kdb-mount.md).
