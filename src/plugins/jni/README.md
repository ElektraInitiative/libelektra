- infos = Information about the jni plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = maintained tested/unit configurable hook memleak experimental
- infos/description = generic Java plugin

## Introduction

Allows you to write plugins in Java.

This plugin needs the JNA bindings to work.
Furthermore, it requires Java 11 or later.

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

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-java`.
To actually mount plugins, you will additionally need `java-elektra`.
Furthermore, at least JNA version 5.5 is required.

## Plugin Config

You need to pass :

- classname the classname to use as plugin, e.g. `elektra/plugin/Echo`
- classpath the classpath where to find JNA, the package elektra and
  other classes needed

Additionally, you can set:

- option allows you to pass an option to the jvm, default: `-verbose:gc,class,jni`
- ignore allows you to ignore broken options, default: `false`
- print allows you to print java exceptions for debugging purposes

If Elektra and a recent jna.jar (adapt path below) is already installed, following should output some debug logs and this README:

```sh
kdb plugin-info -c classname=org/libelektra/plugin/Echo,classpath=.:/usr/share/java/jna.jar:/usr/share/java/libelektra.jar,print= jni
```

> Note: The Java implementation of the plugin can request any other additional
> plugin configuration, read about it in the end of the output of plugin-info.
> Plugins dynamically append text after the end of this page.

You can also mount plugins (see [open issues](https://issues.libelektra.org/3881)):

```sh
kdb mount -c classname=org/libelektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/share/java/libelektra.jar,print= file.properties /jni jni classname=org/libelektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/share/java/libelektra.jar,print=
```

## Compiling

If you do not want to use pre-compiled versions, you can compile the plugin yourself.
Start by enabling the plugin using (`ALL;-EXPERIMENTAL` is default):

```sh
cmake -DPLUGINS="ALL;-EXPERIMENTAL;jni" /path/to/libelektra
```

### on Debian 10 / Ubuntu 20.04 LTS

Install package `openjdk-11-jdk` and make sure that no older Java versions are present in `/usr/lib/jvm`.

If you have manually installed Java, you might get errors related to `Could NOT find JNI` during `cmake`.
In this case, please consider setting your `JAVA_HOME` environment variable accordingly.

### on macOS

Older macOS include an old apple specific version of Java, based on 1.6.
However, for the jni plugin JDK version 11 is required, so either the openjdk or the oracle jdk has to be installed.

For example, install oracle's jdk11 via their provided installer.
After that, you have to set the JAVA_HOME environment variable to the folder where the jdk is installed, usually like

```sh
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home/"
```

As macOS handles linked libraries differently, there is no ldconfig command.
Instead you can export an environment variable to tell Elektra the location of Java's dynamic libraries.

```sh
export DYLD_FALLBACK_LIBRARY_PATH="/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home/jre/lib:/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home/jre/lib/server/"
```

Afterwards, the jni plugin should be included in the build and compile successfully.

### Running the JNI test

Make sure to run the test after compiling the plugin.
Change to your Elektra's build folder and execute the following command for running the JNI plugin test and verify it works:

```
ctest -V -R testmod_jni
```

### Troubleshooting

If it should still not find the correct jni version, or says the jni version is not 11, then it most likely still searches in the wrong directory for the jni header file.
It has been experienced that if the project has been built already without this environment variable set, the Java location is cached.
As a result, it will be resolved wrong in future builds, even though the environment variable is set.
To resolve this, it should be enough to delete the CMakeCache.txt file in the build directory and reconfigure the build.

## Development

To know how the methods of your class are called, use:

```sh
javap -s YourClass
```

Also explained
[here](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html#wp15773)

[JNI Functions](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html)
[Invocation](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/invocation.html)

## Open Issues

- Only a single Java plugin can be loaded
- Java plugins cannot be used from an Java application
- If this plugin is enabled, valgrind detects memory problems even if
  the plugin is not mounted.
