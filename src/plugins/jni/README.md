- infos = Information about the jni plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/placements =
- infos/status = maintained unittest configurable memleak experimental -500
- infos/description = generic java plugin

## Introduction ##

Allows you to write plugins in java.

Needs Java 8 or later. While the plugin internally uses JNI, the Java
binding for your java-plugin may use something different, e.g. JNA.
The requirements for the java bindings are:

- needs to have the classes `elektra/Key` and `elektra/KeySet` with
 - a constructor that takes a C-Pointer as long (J)
 - a method "release" that gives up ownership (set internal pointer to NULL)

The java plugin itself needs to have the following methods:

- constructor without arguments (i.e. default constructor)
- open with argument `elektra/KeySet` (the plugin's conf) and `elektra/Key`
- close with argument `elektra/Key`
- get with arguments `elektra/KeySet` and `elektra/Key`
- set with arguments `elektra/KeySet` and `elektra/Key`
- error with arguments `elektra/KeySet` and `elektra/Key`

## Installation ##

Please install java8 as package, e.g.
[for debian](http://www.webupd8.org/2014/03/how-to-install-oracle-java-8-in-debian.html)
and then let cmake actually find jdk8:

    cd /usr/lib/jvm/ && sudo rm -f default-java && sudo ln -s java-8-oracle default-java

and for the runtime, create the file
`/etc/ld.so.conf.d/java-8-oracle.conf` with the content (for amd64):

    /usr/lib/jvm/default-java/jre/lib/amd64
    /usr/lib/jvm/default-java/lib/amd64
    /usr/lib/jvm/default-java/jre/lib/amd64/server

and run:

    sudo ldconfig

Then enable the plugin using (`ALL;-EXPERIMENTAL` is default):

    cmake -DPLUGINS="ALL;-EXPERIMENTAL;jni" /path/to/libelektra

Running

    kdb-full

should work then (needs BUILD_FULL cmake option), if you get one of these:

    kdb-full: error while loading shared libraries: libmawt.so: cannot open shared object file: No such file or directory
    kdb-full: error while loading shared libraries: libjawt.so: cannot open shared object file: No such file or directory

You missed one of the ldconfig steps.

## Plugin Config ##

You need to pass :
- classname the classname to use as plugin, e.g. `elektra/plugin/Echo`
- classpath the classpath where to find JNA, the package elektra and
  other classes needed

Additionally, you can set:

- option allows you to pass a option to the jvm, default: `-verbose:gc,class,jni`
- ignore allows you to ignore broken options, default: `false`
- print allows you to print java exceptions for debugging purposes

E.g.

    kdb info -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print= jni
    kdb check -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print= jni
    kdb mount -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/src/bindings/jna,print= file.properties /jni jni classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/lib/java:/path/to/libelektra/src/bindings/jna,print=

Or if `.jar` is already installed:

    bin/kdb mount -c classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/share/java/elektra.jar,print= file.properties /jni jni classname=elektra/plugin/PropertiesStorage,classpath=.:/usr/share/java/jna.jar:/usr/share/java/elektra.jar,print=

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

(Argumentation for -500 in status)

- In Debian Wheezy you cannot use openjdk:
  you get a linker error because of some missing private SUN symbols.
  Maybe just the cmake mechanism to find java is broken.
- Only a single java plugin can be loaded
- when this plugin is enabled, valgrind detects memory problems even if
  the plugin is not mounted.

