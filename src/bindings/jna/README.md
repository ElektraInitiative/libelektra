- infos =
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/status = maintained
- infos/provides =
- infos/description =

# JNA

A Java binding using JNA. This binding requires Elektra to be installed on
the system to work.

In preparation for introducing a more high level dynamic proxy based API, the JNA binding is currently structured as Gradle mulit-project setup with just one `java-library` project.

## Usage

To use the bindings in a Java project, we have to include the jar file
libelektra-\$VERSION.jar in the project. The version number is the same
one as used for Elektra. This jar is created upon build of Elektra if
you enable the JNA bindings, e.g., with `cmake -DBINDINGS=jna`, see also
[COMPILE](/doc/COMPILE.md#bindings). Internally, Gradle will be used to
actually compile the plugin.

Please note that the [jni plugin](/src/plugins/jni) serves a different purpose. We
use the jni plugin to develop plugins for Elektra itself, whereas the JNA
bindings allow to use Elektra to access configuration in Java projects. The jni
plugin is _not_ required for the JNA bindings to work. But, to develop
jni plugins, JNA can be used. [Here](libelektra/plugin) are example
plugins, which need jni at runtime.

## Installing the JNA binding to the local Maven repository

Release versions of the Elektra JNA bindings are published to Maven Central with group id `org.libelektra`, artefact id `libelektra` and the same version as Elektra.

If you want to depend on a modified binding or just want to install it to your local maven repository, change your current working directory to the JNA binding folder `/src/bindings/jna`. Either use the bundled Gradle wrapper (`./gradlew` for unix style OS or `./gradlew.bat` for windows) or ensure a recent Gradle version is available on your system and execute:

```sh
gradle publishToMavenLocal
```

This will update the local Maven repository with your current version of the JNA binding as `SNAPSHOT` version. To specify a desired version number (e.g. 1.0.0-mymod) execute the following command instead:

```sh
gradle -PreleaseVersion=1.0.0-mymod publishToMavenLocal
```

You can verify success by listing the directory `~/.m2/repository/org/libelektra/libelektra/1.0.0-mymod/`.

### Command line

#### Linux

For using the binding as standalone (to write applications using Elektra),
make sure that CLASSPATH includes jna.jar and libelektra.jar (or this directory
which contains the libelektra subdirectory that corresponds to the
libelektra.jar), e.g.:

```sh
export CLASSPATH=".:/usr/share/java/libelektra.jar:/usr/share/java/jna.jar"
```

Then you can compile and run [HelloElektra](HelloElektra.java):

```sh
javac HelloElektra.java && java HelloElektra
```

#### macOS

Using the bindings via the command line works the same way as in Linux. The
difference is that in macOS the jar file gets generally installed to
`/usr/local/share/java`.

### Using a build system

To use the JNA bindings via a build system supporting Maven dependencies, first you have to install the JNA bindings to your local Maven repository (see instructions above).
Alternatively you can also depend on a released version published to Maven Central.

When you have built Elektra with the JNA binding included, it will also be automatically installed to
`/usr/share/java/` along with a pom file for the library.

#### Maven

Simply add libelektra as dependency using:

```xml
    <dependency>
      <groupId>org.libelektra</groupId>
      <artifactId>libelektra</artifactId>
      <version>YOUR_DESIRED_VERSION_HERE</version>
    </dependency>
```

[Here](../../../examples/external/java/read-keys-example/pom.xml) is a full example using Maven, which should work out of the box if you execute `mvn compile` in the example directory `../../../examples/external/java/read-keys-example/` and start the compiled `App.class`.

#### Gradle

Simply add libelektra as dependency to you Gradle `java` or `java-library` project using:

```groovy
repositories {
    mavenLocal()
    mavenCentral()
}

dependencies {
    implementation "org.libelektra:libelektra:YOUR_DESIRED_VERSION_HERE"
}
```

[Here](../../../examples/external/java/read-keys-example-gradle/build.gradle) is a full example using Gradle, which should work out of the box if you execute `./gradlew run` in the example directory `../../../examples/external/java/read-keys-example-gradle/`.

### Using Elektra Plugins

The `PluginLoader` can be used to load a native Elektra plugin which is written in C/C++.
You can load a native Elektra Plugin like the following:

```java
PluginLoader pluginLoader = new PluginLoader();
Plugin errorPlugin = pluginLoader.loadElektraPlugin("error");
```

Now you can pass a KeySet and let the Plugin do its work. E.g., the code below tests if the `error` plugin.

```java
Key errorKey = Key.create("user:/tests/myError");
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
Key rangeKey = Key.create("user:/tests/myError", "30");
rangeKey.setMeta("check/range", "1-20");
final KeySet ks = KeySet.create(10, KeySet.KS_END);
ks.append(rangeKey);
rangePlugin.kdbSet(ks, parentKey);
//org.libelektra.exception.SemanticValidationException: Sorry, module range issued error C03200:
//Value '30' of key 'user:/tests/myError' not within range 1-20
//Configfile: user:/tests/javabinding
//Mountpoint: user:/tests/javabinding
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

You can run unit tests after importing jUnit, JNA and the libelektra java
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
    the libelektra folder:

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

## Limitations

- no binary keys
- no Java iterator for metadata

## Release publishing to Maven Central

Release publishing is done via `buildAndPublishMaven()` in [Jenkinsfile.release](../../../scripts/jenkins/Jenkinsfile.release).

## Contributing

Please only use Javadoc compliant tags when documenting the code.
