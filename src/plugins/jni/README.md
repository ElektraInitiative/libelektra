- infos = Information about the jni plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements =
- infos/description =

# Generic Java plugin #

## Introduction ##

Allows you to write plugins in java.

Needs Java 8 or later. While the plugin internally uses JNI, the Java
binding for your java-plugin may use something different, e.g. JNA.
The requirements for the java bindings are:

- needs to have the classes elektra/Key and elektra/KeySet with
 - an constructor that takes a C-Pointer as long (J)
 - an method "release" that gives up ownership (set internal pointer to NULL)

The java plugin itself needs to have following methods:

- constructor without arguments (e.g. default constructor)
- open with argument elektra/KeySet (the plugin's conf) and elektra/Key
- close with argument elektra/Key
- get with arguments elektra/KeySet and elektra/Key
- set with arguments elektra/KeySet and elektra/Key
- error with arguments elektra/KeySet and elektra/Key

## Plugin Config ##

You need to pass :
- classname the classname to use as plugin, e.g. elektra/plugin/Echo
- classpath the classpath where to find JNA, the package elektra and
  other classes needed

Additionally, you can set:

- option allows you to pass a option to the jvm, default: -verbose:gc,class,jni
- ignore allows you to ignore broken options, default: false
- print allows you to print java exceptions for debugging purposes

E.g.

    kdb info -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print= jni
    kdb check -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print= jni
    kdb mount -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/src/bindings/jna,print= file.properties /jni jni classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print=

Additionally, the java implementation can request any other additional
configuration, read about it below in the section (specific java plugin).
If you are reading this page on github, you won't see it, because the
plugins dynamically append text after the end of this page.

## Development ##

To know how the methods of your class are called, use:

    javap -s YourClass

Also explained
[here](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html#wp15773)

[JNI Functions](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html)
[Invocation](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/invocation.html)


## Issues ##

- In Debian Wheezy you cannot use openjdk:
  you get a linker error because of some missing private SUN symbols.
  Maybe just the cmake mechanism to find java is broken.
- Only a single java plugin can be loaded
- when this plugin is enabled, valgrind detects memory problems even if
  the plugin is not mounted.


# Specific Java Plugin #
