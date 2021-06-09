# Whitelist Java JNI Plugin

This `checker` plugin enforces set key values to adhere to a whitelist specified in the keys specification. To specify a whitelist just add the following to your key's specification entry:

```
check/whitelist/#0 = some-allowed-value
check/whitelist/#1 = another-allowed-value
check/whitelist/#19 = yet-nother-allowed-value
```

`#` indices do not have to be consecutive. Whitelist entries are case sensitive and are not checked or normalized when read.

## Build

The plugin is a sub-project of the JNA bindings Gradle multi-project.

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
