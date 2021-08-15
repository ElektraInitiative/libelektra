# Whitelist Java JNI Plugin

This `checker` plugin enforces set key values to adhere to a whitelist specified in the keys specification. To specify a whitelist just add the following to your key's specification entry:

```
check/whitelist/#0 = some-allowed-value
check/whitelist/#1 = another-allowed-value
check/whitelist/#9 = yet-nother-allowed-value
```

`#` indices do not have to be consecutive. Whitelist entries are case sensitive and are not checked or normalized when read.

Keys below `check/whitelist` not adhering to the schema above will be ignored and correspondig warnings will be issued.

The pattern matches `check/whitelist/#_*\\d+` and therefore is not equivalent to Elektra array indices.

Binary keys are not supported.

## Build

The plugin is a subproject of the Java binding Gradle multi-project.

To assemble the plugin's JAR:

```sh
cd src/bindings/jna
gradle :plugins:whitelist:assemble
```

To execute its unit tests:

```sh
cd src/bindings/jna
gradle :plugins:whitelist:test
```

To do both in one step:

```sh
cd src/bindings/jna
gradle :plugins:whitelist:build
```
