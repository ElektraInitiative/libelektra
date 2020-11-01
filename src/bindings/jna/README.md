- infos =
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/status = maintained
- infos/provides =
- infos/description =

# JNA

A Java binding using JNA. This binding requires Elektra to be installed on
the system to work.

## Usage

To use the bindings in a Java project, we have to include the jar file
libelektra-\$VERSION.jar in the project. The version number is the same
one as used for Elektra. This jar is created upon build of Elektra if
you enable the jna bindings, e.g., with `cmake -DBINDINGS=jna`, see also
[COMPILE](/doc/COMPILE.md#bindings). Internally, `mvn` will be used to
actually compile the plugin.

Please note that the [jni plugin](/src/plugins/jni) serves a different purpose. We
use the jni plugin to develop plugins for Elektra itself, whereas the jna
bindings allow to use Elektra to access configuration in Java projects. The jni
plugin is _not_ required for the jna bindings to work. But, to develop
jni plugins, jna can be used. [Here](libelektra4j/plugin) are example
plugins, which need jni at runtime.

### Command line

#### Linux

For using the binding as standalone (to write applications using Elektra),
make sure that CLASSPATH includes jna.jar and libelektra.jar (or this directory
which contains the libelektra4j subdirectory that corresponds to the
libelektra.jar), e.g.:

```sh
export CLASSPATH=".:/usr/share/java/libelektra4j.jar:/usr/share/java/jna.jar"
```

Then you can compile and run [HelloElektra](HelloElektra.java):

```sh
javac HelloElektra.java && java HelloElektra
```

#### macOS

Using the bindings via the command line works the same way as in Linux. The
difference is that in macOS the jar file gets generally installed to
`/usr/local/share/java`.

### Maven

To use the jna bindings via maven, first you have to install the jna bindings
to your local maven repository. When you have built Elektra with the jna
bindings included, they should have been automatically installed to
´/usr/share/java/´ along with a pom file for the library. To install it to your
local maven repository from that location, execute the following command
(Note: use `/usr/local/...` if it was installed there):

```sh
mvn org.apache.maven.plugins:maven-install-plugin:install-file \
    -Dfile=/usr/share/java/libelektra4j.jar \
    -DpomFile=/usr/share/java/libelektra4j.pom.xml
```

Then you can simply add libelektra as dependency using:

```xml
    <dependency>
      <groupId>org.libelektra</groupId>
      <artifactId>libelektra4j</artifactId>
      <version>0.9.3</version>
    </dependency>
```

[Here](../../examples/external/java/read-keys-example/pom.xml) is a full example using maven,
which should work out of the box if above `mvn` command was executed.

### Using Elektra Plugins

The `PluginLoader` can be used to load a native Elektra plugin which is written in C/C++.
You can load a native Elektra Plugin like the following:

```java
PluginLoader pluginLoader = new PluginLoader();
Plugin errorPlugin = pluginLoader.loadElektraPlugin("error");
```

Now you can pass a KeySet and let the Plugin do its work. E.g., the code below tests if the `error` plugin.

```java
Key errorKey = Key.create("user/tests/myError");
errorKey.setMeta(errorMeta, OutOfMemoryException.errorNumber());
final KeySet ks = KeySet.create(10, KeySet.KS_END);
ks.append(errorKey);
errorPlugin.kdbSet(ks, parentKey);
\\ OutOfMemoryException is thrown
```

Another example is the `range` plugin which throws the equivalent Java exception:

```java
PluginLoader pluginLoader = new PluginLoader();
Plugin rangePlugin = pluginLoader.loadElektraPlugin("range");
Key rangeKey = Key.create("user/tests/myError", "30");
rangeKey.setMeta("check/range", "1-20");
final KeySet ks = KeySet.create(10, KeySet.KS_END);
ks.append(rangeKey);
rangePlugin.kdbSet(ks, parentKey);
//org.libelektra.exception.SemanticValidationException: Sorry, module range issued error C03200:
//Value '30' of key 'user/tests/myError' not within range 1-20
//Configfile: user/tests/javabinding
//Mountpoint: user/tests/javabinding
//At: .../elektra/src/plugins/range/range.c:447
```

Note that the `PluginLoader` can also load Plugins written in Java such as the `PropertiesStorage` plugin:

```java
PluginLoader pluginLoader = new PluginLoader();
Plugin propertiesStoragePlugin = pluginLoader.loadJavaPlugin(PropertiesStorage.PLUGIN_NAME);
```

### Implementing Plugins

The `Plugin` interface can be used to develop your _own_ Elektra plugins written in Java.
You have to implement the necessary methods which are of interest to you such as
`set(KeySet ks, Key parentKey)` for plugins which should change the key database.
We added a tutorial with more details for you [here](../../../doc/tutorials/java-plugins.md).
You can see various examples in the [plugin folder](src/main/java/org/libelektra/plugin) like the `PropertiesStorage` plugin
which can be used to save and load `.properties` files into Elektra.

## Testing

### Command Line

You can run unit tests after importing jUnit, JNA and the libelektra4j java
library into a project (eclipse, netbeans, intelliJ, ...).

Tested library versions are:

- JNA: 4.5.0
- jUnit: 4.12 [jUnit 3 is not supported]
- hamcrest-core: 1.13 (required by newer jUnit versions)

Tested JDK versions are:

- Oracle JDK 1.8.0_112 on macOS 10.12 Sierra
- OpenJDK 1.8.0_121 on Arch Linux
- OpenJDK 1.9 on Ubuntu
- Oracle JDK 1.9 build 9+181 on macOS 10.12 Sierra

Both libraries should work on version 4.0 too, though.

It should also be possible to run the tests by command line:

1.  Compile the library and tests (run in root directory; make sure junit4 and
    jna are installed and/or path is correct). Execute the following commands inside
    the libelektra4j folder:

    ```sh
    mkdir ./target (if it does not exist yet)
    javac -cp <path to junit and jna and hamcrest*> -d \
          ./target src/main/java/org/libelektra/*.java \
          src/main/java/org/libelektra/plugin/*.java \
          src/test/java/org/libelektra/*.java
    ```

    If you copied the jna.jar, junit.jar and hamcrest-core.jar directly to the
    jna directory, the correct path would be `./jna.jar:./junit.jar:./hamcrest-core.jar`
    (separated by : on Mac and Linux, by ; on Windows), otherwise specify the
    appropriate locations.

    For Linux users they are usually in `/usr/share/java/jna.jar:/usr/share/java/junit4.jar`

2.  Run all jUnit tests (please note that the -cp parameter now also has to
    include the target directory we created in the first step, where the compiled
    classfiles are):

    ```sh
    java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.AllTests
    ```

    Or run all tests on their own:

    ```sh
    java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.KeyTest
    java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.KeySetTest
    ```

### Maven

When using Maven, the unit tests of the bindings will be automatically executed
if you run all tests on a build (the target is called testjna_maven).

## Limitations

- no binary keys
- no Java iterator for metadata
