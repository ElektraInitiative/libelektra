- infos = Information about the jni plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = maintained unittest configurable global memleak experimental discouraged
- infos/description = generic Java plugin

## Introduction

Allows you to write plugins in Java.

This plugin needs the JNA bindings to work.
Furthermore, it requires Java 8 or later.

While the plugin internally uses JNI (thus the name), the Java
binding for your Java plugin may use something different, e.g. JNA.
The requirements for the Java bindings are:

- needs to have the classes `elektra/Key` and `elektra/KeySet` with
  - a constructor that takes a C-Pointer as long (J)
  - a method "release" that gives up ownership (set internal pointer to NULL)

The Java plugin itself needs to have the following methods:

- constructor without arguments (i.e. default constructor)
- open with argument `elektra/KeySet` (the plugin's conf) and `elektra/Key`
- close with argument `elektra/Key`
- get with arguments `elektra/KeySet` and `elektra/Key`
- set with arguments `elektra/KeySet` and `elektra/Key`
- error with arguments `elektra/KeySet` and `elektra/Key`

## Installation

### Java Prerequisites on Debian 9

openjdk-8 and 9 do not work reliable: jvm crashes without usable backtrace.

When using non-standard paths, you have to set JAVA_HOME before invoking cmake.
(For example when you unpack Oracle Java to `/usr/local` or `/opt`.)
For example:

```sh
JAVA_HOME=/usr/local/jdk-9.0.1
```

### Java Prerequisites on Debian 8

Please install java8 as package, e.g.
[for debian](http://www.webupd8.org/2014/03/how-to-install-oracle-java-8-in-debian.html)
and then let cmake actually find jdk8:

```sh
cd /usr/lib/jvm/ && sudo rm -f default-java && sudo ln -s java-8-oracle default-java
```

and for the run-time, create the file
`/etc/ld.so.conf.d/java-8-oracle.conf` with the content (for amd64):

```
/usr/lib/jvm/default-java/jre/lib/amd64
/usr/lib/jvm/default-java/lib/amd64
/usr/lib/jvm/default-java/jre/lib/amd64/server
```

and run:

```sh
sudo ldconfig
```

### Java Prerequisites on macOS

macOS includes an old apple specific version of java, based on 1.6.
However, for the jni plugin version 1.8 of Java is required, so either the openjdk or the oracle jdk has to be installed.

Please install oracle's jdk8 via their provided installer.
After that, you have to set the JAVA_HOME environment variable to the folder where the jdk is installed, usually like

```sh
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_112.jdk/Contents/Home/"
```

As macOS handles linked libraries differently, there is no ldconfig command.
Instead you can export an environment variable to tell elektra the location of the java dynamic libraries.

```sh
export DYLD_FALLBACK_LIBRARY_PATH="/Library/Java/JavaVirtualMachines/jdk1.8.0_112.jdk/Contents/Home/jre/lib:/Library/Java/JavaVirtualMachines/jdk1.8.0_112.jdk/Contents/Home/jre/lib/server/"
```

Afterwards, the jni plugin should be included in the build and compile successfully.

#### Troubleshooting

If it should still not find the correct jni version, or says the jni version is not 1.8, then it most likely still searches in the wrong directory for the jni header file.
It has been experienced that if the project has been built already without this environment variable set, the java location is cached.
As a result, it will be resolved wrong in future builds, even though the environment variable is set.
To resolve this, it should be enough to delete the CMakeCache.txt file in the build directory and reconfigure the build.

### Enabling the Plugin

Then enable the plugin using (`ALL;-EXPERIMENTAL` is default):

```sh
cmake -DPLUGINS="ALL;-EXPERIMENTAL;jni" /path/to/libelektra
```

Running

```sh
kdb-full
```

should work then (needs BUILD_FULL cmake option), if you get one of these:

```
kdb-full: error while loading shared libraries: libmawt.so: cannot open shared object file: No such file or directory
kdb-full: error while loading shared libraries: libjawt.so: cannot open shared object file: No such file or directory
```

You missed one of the ldconfig steps.

## Plugin Config

You need to pass :

- classname the classname to use as plugin, e.g. `elektra/plugin/Echo`
- classpath the classpath where to find JNA, the package elektra and
  other classes needed

Additionally, you can set:

- option allows you to pass an option to the jvm, default: `-verbose:gc,class,jni`
- ignore allows you to ignore broken options, default: `false`
- print allows you to print java exceptions for debugging purposes

E.g.

```sh
kdb plugin-info -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print= jni
kdb plugin-check -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print= jni
kdb mount -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/src/bindings/jna,print= file.properties /jni jni classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print=
```

Or if `.jar` is already installed:

```sh
bin/kdb mount -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/share/java/elektra.jar,print= file.properties /jni jni classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/share/java/elektra.jar,print=
```

Additionally, the java implementation can request any other additional
configuration, read about it below in the section (specific java plugin).
If you are reading this page on GitHub, you won't see it, because the
plugins dynamically append text after the end of this page.

## Development

To know how the methods of your class are called, use:

```sh
javap -s YourClass
```

Also explained
[here](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html#wp15773)

[JNI Functions](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html)
[Invocation](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/invocation.html)

## Issues

Argumentation for discouraged:

- You cannot use the plugin with openjdk:
  You get a linker error because of some missing private SUN symbols.
  In Debian9 it crashes with openjdk8/9.
- Only a single java plugin can be loaded
- When this plugin is enabled, valgrind detects memory problems even if
  the plugin is not mounted.
