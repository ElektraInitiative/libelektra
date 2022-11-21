- infos =
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/status = maintained
- infos/provides =
- infos/description =

# JNA

A Java binding using JNA. This binding requires Elektra to be installed on
the system to work.

In preparation for introducing a more high level dynamic proxy based API, the JNA binding is currently structured as Gradle mulit-project setup with just one `java-library` project.

You can run the `HelloElekra` example by executing:

```sh
./gradlew hello:run
```

If this fails, make sure you have at least Java version 11 or higher installed and the `JAVA_HOME` environment variable points to its installation directory.

## Usage

To use the bindings in a Java project, we have to include the jar file
libelektra-\$VERSION.jar in the project. The version number is the same
one as used for Elektra.

### Build

This jar is created upon building Elektra if you include the JNA bindings during the build process, e.g., with `cmake -DBINDINGS=jna`. Check out [COMPILE](/doc/COMPILE.md#bindings) for more details on how to include bindings in the build process.
To make sure the build will be successfull, make sure the following requirements are met:
- `gradle` must be installed on your machine, otherwise jna will not be included during compilation.
- Use a JDK version <=17. Higher versions will result in _Unsupported class file major version 62/63_ errors.

Internally, Gradle will be used to actually compile the plugin.

### JNI

Please note that the [JNI plugin](/src/plugins/jni) serves a different purpose. We
use the JNI plugin to develop plugins for Elektra itself, whereas the JNA
bindings allow to use Elektra to access configuration in Java projects. The JNI
plugin is _not_ required for the JNA bindings to work. But, to develop
JNI plugins, JNA can be used. [Here](libelektra/src/main/java/org/libelektra/plugin/) are example
plugins, which need JNI at runtime.

### Command line

#### Linux

For using the binding as standalone (to write applications using Elektra),
make sure that CLASSPATH includes jna.jar and libelektra.jar (or this directory
which contains the libelektra subdirectory that corresponds to the
libelektra.jar), e.g.:

```sh
export CLASSPATH=".:/usr/share/java/libelektra.jar:/usr/share/java/jna.jar"
```

Then you can compile and run [HelloElektra](hello/src/main/java/HelloElektra.java):

```sh
javac HelloElektra.java && java HelloElektra
```

#### macOS

Using the bindings via the command line works the same way as in Linux. The
difference is that in macOS the jar file gets generally installed to
`/usr/local/share/java`.

### Using a build system

To use the JNA bindings via a build system supporting Maven dependencies, you can depend on a released version published to Maven Central (available since version 0.9.5).

Alternatively you can install the JNA bindings to your local Maven repository (see instructions below).

When you have built Elektra with the JNA binding included, it will also be automatically installed to
`/usr/share/java/` along with a pom file for the library.

#### Maven

Simply add libelektra as dependency using:

```xml
    <dependency>
      <groupId>org.libelektra</groupId>
      <artifactId>libelektra</artifactId>
      <version>0.9.11</version>
    </dependency>
```

[Here](../../../examples/external/java/read-keys-example/pom.xml) is a full example using Maven, which should work out of the box by executing `mvn compile exec:java` in the example directory `../../../examples/external/java/read-keys-example/`.

#### Gradle

Simply add libelektra as dependency to you Gradle `java` or `java-library` project using:

```groovy
repositories {
    mavenLocal()
    mavenCentral()
}

dependencies {
    implementation "org.libelektra:libelektra:0.9.11"
}
```

[Here](../../../examples/external/java/read-keys-example/build.gradle) is a full example using Gradle, which should work out of the box by executing `gradle run` in the example directory `../../../examples/external/java/read-keys-example/`.

### Installing the JNA binding to the local Maven repository

Release versions of the Elektra JNA bindings are published to Maven Central with group id `org.libelektra`, artefact id `libelektra` and the same version as Elektra.

If you want to depend on a modified binding or just want to install it to your local maven repository, change your current working directory to the JNA binding folder `/src/bindings/jna`. Either use the bundled Gradle wrapper (`./gradlew` for Unix style OS or `./gradlew.bat` for windows) or ensure a recent Gradle version is available on your system and execute:

```sh
gradle publishToMavenLocal
```

This will update the local Maven repository with your current version of the JNA binding as `SNAPSHOT` version. To specify a desired version number (e.g. 1.0.0-mymod) execute the following command instead:

```sh
gradle -PreleaseVersion=1.0.0-mymod publishToMavenLocal
```

You can verify success by listing the directory `~/.m2/repository/org/libelektra/libelektra/1.0.0-mymod/`.

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
You can see various examples in the [plugin folder](libelektra/src/main/java/org/libelektra/plugin) like the `PropertiesStorage` plugin
which can be used to save and load `.properties` files into Elektra.

## Testing

### Command Line

You can run unit tests by invoking:

```sh
./gradlew test
```

## Release publishing to Maven Central

Release publishing is done via `buildAndPublishMaven()` in [Jenkinsfile.release](../../../scripts/jenkins/Jenkinsfile.release).

## Upgrading Gradle

To upgrade the Gradle version used by the JNA bindings, the following steps should be considered:

- [Update the gradle wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html#sec:upgrading_wrapper) for the Java binding
- Search all [Dockerfiles](../../../scripts/docker) and replace the Gradle version there
- [Run formatter](../../../doc/tutorials/run_reformatting_script_with_docker.md)

## Contributing

Please only use Javadoc compliant tags when documenting the code.
