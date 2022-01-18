This is a fully working example for how to use Elektra with Maven or Gradle for reading keys.

First Make sure to install the Elektra Java binding to your local maven repository:

```sh
cd ./src/bindings/jna
./gradlew publishLibelektraPublicationToMavenLocal
```

Alternatively change the `libelektra` dependency in `build.gradle` to a published version, fitting your Elektra installation.
(See [JNA binding](../../../../src/bindings/jna/README.md) for more information.)

Then change to the example app folder:

```sh
cd ../../../examples/external/java/read-keys-example
```

Use a local Gradle installation to run the example app:

```sh
gradle run
```

Alternatively use a local Maven installation to run the example app:

```sh
mvn compile exec:java
```
