- infos = Information about the jni plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements =
- infos/description =

## Introduction ##

Allows you to write plugins in java.

Needs Java 8 or later. While the plugin internally uses JNI, the Java
binding for your java-plugin may use something different, e.g. JNA.
The requirements for the java bindings are:

- needs to have the classes Elektra/Key and Elektra/KeySet with
 - an constructor that takes a C-Pointer as Long (J)
 - an method "release" that gives up ownership (set internal pointer to NULL)

The java plugin itself needs to have following methods:

- constructor without arguments
- open with argument Elektra/Key
- close with argument Elektra/Key
- get with arguments Elektra/KeySet and Elektra/Key
- set with arguments Elektra/KeySet and Elektra/Key
- error with arguments Elektra/KeySet and Elektra/Key

## Plugin Config ##

You need to pass classname and classpath, e.g.:

    classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/home/markus/Projekte/Elektra/libelektra/src/bindings/jna
    classname=Elektra/DemoPlugin

additional you can set:

- option allows you to pass a option to the jvm, default: -verbose:gc,class,jni
- ignore allows you to ignore broken options, default: false
- print allows you to print java exceptions for debugging purposes

E.g.

    option=-verbose:gc,ignore=,print=


## Development ##

To know how the methods of your class are called, use:

    javap -s YourClass

Also explained
[here](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html#wp15773)

[JNI Functions](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html)
[Invocation](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/invocation.html)


## Issues ##

In Debian Wheezy you cannot use openjdk:
you get a linker error because of some missing private SUN symbols.
Maybe just the cmake mechanism to find java is broken.

## Usage ##

