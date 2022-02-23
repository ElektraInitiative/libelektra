# Mounting Java Plugins

Mounting a Java plugin is a bit more complicated than mounting a "normal" C plugin.
This is because you actually need to mount the `process` plugin with the correct configuration for a Java plugin.

The following snippet mounts the C plugins `dump` and `type` together with the Java plugin `org.libelektra.plugin.WhitelistPlugin` with the file `config.file` at `user:/tests/process`:

```sh
KDB_VERSION="$(kdb --version | sed -nE 's/KDB_VERSION: (.+)/\1/gp')"
sudo kdb mount config.file user:/tests/process dump process "executable=$(command -v java)" 'args=#3' 'args/#0=-cp' "args/#1=/usr/share/java/libelektra-$KDB_VERSION-all.jar:/usr/share/java/process-$KDB_VERSION.jar:/usr/share/java/whitelist-$KDB_VERSION.jar" 'args/#2=org.libelektra.process.ProcessApp' 'args/#3=org.libelektra.plugin.WhitelistPlugin' type
```

This can get complicated very quickly.
For one, you need to know that Elektra's JARs are installed in `/usr/share/java` and then you need to add 7 long arguments for every Java plugin.
To make this easier, we provide a helper script which can be used like this:

```sh
sudo kdb mount-java config.file user:/tests/process kdb:dump java:org.libelektra.plugin.WhitelistPlugin type
```

The line above has exactly the same effect as the previous snippet.
However, here you don't need to know where the JARs are installed, and you just need to prefix the class names of Java Plugins with `java:`.
Additionally, you can (but only have to in case of ambiguities) prefix C plugins with `kdb:`.

The `mount-java` works as is for all plugins that come bundled with Elektra.
If you have external plugins, you must also specify the classpath for these plugins like this:

```sh
CLASSPATH=/path/to/foo.jar:/path/to/bar.jar sudo kdb mount-java config.file user:/tests/foobar java:org.example.Foo java:org.example.Bar java:org.libelektra.plugin.WhitelistPlugin
```

Here we added `/path/to/foo.jar` and `/path/to/bar.jar` to the classpath, so that the classes `org.example.Foo` and `org.example.Bar` can be found.
We can still use `org.libelektra.plugin.WhitelistPlugin`, because Elektra's JARs are always added to the classpath.
