# How-To: Java kdb

## Introduction

When programming in Java it is possible to access the key database, changing values of existing keys or adding new ones and a few other things. It is also possible to write plugins for Elektra in Java but we will focus on using the Java binding in this tutorial.

## First Steps

In order to use `kdb` you need to include the dependency in your project. [Here](../../src/bindings/jna/README.md) you can find a detailed tutorial on how to do that.

After that you can start loading a `KDB` object as follows:

```java
var key = Key.createNameless();
try (KDB kdb = KDB.open(key)) {
    // code to manipulate keys
} catch (KDB.KDBException e) {
    e.printStackTrace();
} finally {
    key.release(); // optional clean-up
}
```

Note that `KDB` implements `AutoClosable` which allows [`try-with-resouces`](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html).

The key being passend to `KDB::open` is used to store warnings and error information. If an error occurs, it will be mapped to the appropriate specialization of `KDBException`.

The following code is equivalent and preferred, if you do not want to reuse an existing key for transferring warnings and error information:

```java
try (KDB kdb = KDB.open()) {
    // code to manipulate keys
} catch (KDB.KDBException e) {
    e.printStackTrace();
    e.releaseErrorKey(); // optional clean-up
}
```

## A word about releasing native ressources

There are 3 kinds of native resources having to be cleaned up properly to prevent memory leaks: `KeySet`, `Key` and `KDB` or rather their backing native key set, key or KDB session.

Fortunately the only resource you are strictly required to release is `KDB` via either `KDB::close` or `try-with-resouces`. For `KeySet` and `Key` the garbage collector cleans them up automatically using a [`Cleaner`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/ref/Cleaner.html). Nonetheless, it might be a good idea to clean up these native resources as soon as they are no longer needed, since we do not have any control of when or if the garbage collector cleans up after us.

## Fetching keys

First let's retrieve a key which is already part of the key database. The first thing we need to do is to create a `KeySet` your keys are going to be stored in:

```java
var keySet = KeySet.create();
```

Now we load all keys located below a sepcific parent key:

```java
var parentKey = Key.create("user:/");
kdb.get(keySet, parentKey);
```

Now we can simply fetch the desired key's value as follows:

```java
String value = keySet.lookup("user:/my/presaved/key").map(Key::getString).orElseThrow();
```

So for example if you had executed the command below via shell, before starting the application:

```bash
kdb set user:/my/presaved/key it_works!
```

`value` would equals `it_works!`.

Afterward, we may clean-up no longer needed native resources:

```
parentKey.release();
keySet.release();
```

## Saving Keys

Next let's save a new key to the key database. Again, first we need need to create an empty `KeySet`. We also **need to fetch** all keys for the namespace before we will be able to save a new key.

```java
var keyNamespace = Key.create("user:/");                 // create key representing the namespace to fetch
var keySet = kdb.get(keyNamespace);                      // fetch all keys for the namespace into a new key set
var keyToStore = Key.create("user:/somekey", "myValue"); // create key with value to store
keySet.append(keyToStore);
kdb.set(keySet, keyToStore);

// optional clean-up
keySet.release();
keyNamespace.release();
keyToStore.release();
```

If you try to save a key without fetching it beforehand, a `KDBException` will be thrown, telling you to call get before set.

The _user_ namespace is accessible without special rights, but if you try to write to _system_ you will need to have root privileges. Take a look at [TESTING.md](/doc/TESTING.md) to see how to access the system namespace as non-root user. This should only be done in testing environments though as it is not intended for productive systems.

## Examples

### Traversing Keys in a `KeySet`

```java
var keyNamespace = Key.create("user:/");            // select a namespace from which all keys should be fetched
try (KDB kdb = KDB.open()) {
    var keySet = kdb.get(keyNamespace);             // fetch all keys int new key set
    for (int i = 0; i < keySet.length(); i++) {     // traverse the set
        var currentKey = keySet.at(i);
        System.out.println(String.format("%s: %s",
                currentKey.getName(),               // fetch the key's name
                currentKey.getStringAndRelease())); // fetch the key's value and release the key returned by KeySet::at
    }
    keySet.release(); // optional clean-up
} catch (KDB.KDBException e) {
    e.printStackTrace();
    e.releaseErrorKey(); // optional clean-up
} finally {
    keyNamespace.release(); // optional clean-up
}
```

First we create a new `KDB` session and fetch all keys for the desired namespace, in this example the whole `user` namespace. Since all all keys are put in the passed `keySet` variable we can then iterate through it.
The `at(int)` method return a new `Key` object for the native key with the corresponding position within the `keySet`.

Of course, alternatively a for each loop can be used:

```java
var keyNamespace = Key.create("user:/");            // select a namespace from which all keys should be fetched
try (KDB kdb = KDB.open()) {
    var keySet = kdb.get(keyNamespace);             // fetch all keys int new key set
    for (var currentKey : keySet) {                 // traverse the set
        System.out.println(String.format("%s: %s",
                currentKey.getName(),               // fetch the key's name
                currentKey.getStringAndRelease())); // fetch the key's value and release the key returned by KeySet::at
    }
    keySet.release(); // optional clean-up
} catch (KDB.KDBException e) {
    e.printStackTrace();
    e.releaseErrorKey(); // optional clean-up
} finally {
    keyNamespace.release(); // optional clean-up
}
```

### Read Multiple Keys From KDB

[This](../../examples/external/java/read-keys-example) example shows how to read multiple keys. It provides comments for further clarification. Further information can be found [here](../../examples/external/java/read-keys-example/README.md).

## Java Plugin Tutorial

For the tutorial on how to write java plugins, please check out [this](/doc/tutorials/java-plugins.md) page.
