# How-To: Write a Java Plugin

This file serves as a tutorial on how to get started with writing a Java plugin.

## Basics

If you want to know more about plugins in general, please check out the [How-To: Write a Plugin](/doc/tutorials/plugins.md) page.
If you need a tutorial for using Key and KeySet in Java, please check out the [How-To: Java kdb](/doc/tutorials/java-kdb.md) page.

## Two Technologies used in Java Plugins

Before we will take a look into how to write a plugin in Java, it is important to note, that there are two technologies needed:

- `process` plugin
- JNA binding

### `process` Plugin

The `process` plugin is a special plugin, which allows using an external application as the implementation of a plugin.

To achieve this, the `process` plugin spawns a child process for the external executable and then uses a simple protocol to relay any requested operations to this child process.
The details of how this protocol works are not important for writing a Java plugin.
All the details of the protocol are abstracted via the `PluginProcess` class and the `Plugin` interface.

If you do want to know the details of the `process` protocol, take a look at the [README](../../src/plugins/process/README.md) of the `process` plugin.

### JNA Binding

Java Native Access is Java technology (library), which like JNI allows a developer to run native code using only Java by providing access to native shared libraries. In order to use the JNA binding, it should be installed first. You can find more information on [JNA’s GitHub page](https://github.com/java-native-access/jna).

We provide a Java binding for Elektra's API via JNA.
For more general information about the binding take a look at [this document](java-kdb.md).

### Writing a Plugin

To write a plugin, you need to create an implementation of the `org.libelektra.Plugin` interface.
This is very similar writing any other plugin, except that you use Java instead of C.

The standard API functions of the plugin API all have corresponding methods in the `Plugin` interface:

- `int open(KeySet config, Key errorKey)` is the Java version of [`elektraPluginOpen`](https://doc.libelektra.org/api/latest/html/group__plugin.html#ga23c2eb3584e38a4d494eb8f91e5e3d8d)
- `int get(KeySet keySet, Key parentKey)` is the Java version of [`elektraPluginGet`](https://doc.libelektra.org/api/latest/html/group__plugin.html#gacb69f3441c6d84241b4362f958fbe313)
- `int set(KeySet keySet, Key parentKey)` is the Java version of [`elektraPluginSet`](https://doc.libelektra.org/api/latest/html/group__plugin.html#gae65781a1deb34efc79c8cb9d9174842c)
- `int error(KeySet keySet, Key parentKey)` is the Java version of [`elektraPluginError`](https://doc.libelektra.org/api/latest/html/group__plugin.html#gad74b35f558ac7c3262f6069c5c47dc79)
- `int close(Key parentKey)` is the Java version of [`elektraPluginClose`](https://doc.libelektra.org/api/latest/html/group__plugin.html#ga1236aefe5b2baf8b7bf636ba5aa9ea29)

Additionally, there is a `@Nonnull String getName()` method.
This method must return the unique name of the plugin.
In the C API this would be taken from the contract, but the `process` protocol requires this separately, so we need a separate method for it.

Otherwise, there are a few differences between implementing a plugin in C and in Java:

1. In C it is possible to implement only `elektraPluginGet` and leave the other functions unimplemented.
   Because of interface inheritance in Java, it is required to implement all 5 method `open`, `get`, `set`, `error` and `close`.
   Whether a method is actually supported, must be communicated via the contract.
2. In C the parent key of the contract depends on the plugins name.
   For example, the contract for `dump` can be found under `system:/elektra/modules/dump` and the `dump` plugin returns it as such.
   However, in Java the parent key for the contract is always `system:/elektra/modules/java` (you may use the constant `Plugin.PROCESS_CONTRACT_ROOT`).
   The keys will be transformed via the `process` protocol and plugin to match the normal expectations.
3. In C all functions a plugin exports (including `open`, `get`, `set`, `error`, `close`, but also additional ones) are registered in the contract under `system:/elektra/modules/<plugin>/exports/<function>` with a function pointer key.
   Because we cannot provide a C function pointer to a Java function and because `process` uses a child process for the Java code, we cannot export functions like that.
   This means a Java plugin cannot export additional functions.
   However, we must still define which functions are supported by the plugin.
   To this end, a Java plugin must set `system:/elektra/modules/java/exports/has/<function> = 1` (where `<function>` is one of `open`, `get`, `set`, `error`, `close`) for all supported functions.
4. Methods that are not supported, should simply be implemented as
   ```java
   throw new UnsupportedOperationException();
   ```
   This is safe, because the method will not be called if the other steps are followed correctly.
   The exception here is the `get` method.
   It must still be implemented and return the contract, when the parent is the same as or below `system:/elektra/modules/java`.
   Otherwise, it is safe to throw `UnsupportedOperationException`.

Otherwise, the rules for return values and plugin behavior are the same as for a C plugin.

You can find a few examples for Java plugins in the folder [`src/bindings/jna/plugins/`](../../src/bindings/jna/plugins/).
We use separate Gradle projects for every plugin, but that is not a requirement.
But creating separate JAR files for each plugin, keeps down the size of the binaries that need to be loaded to mount a plugin.

### Usage of Plugin

See [man page of `kdb-mount-java(1)`](../help/kdb-mount-java.md).
