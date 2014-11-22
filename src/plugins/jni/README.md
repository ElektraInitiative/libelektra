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

