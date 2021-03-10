# How-To: Java kdb

## Introduction

When programming in Java it is possible to access the key database, changing values of existing keys or adding new ones and a few other things. It is also possible to write plugins for Elektra in Java but we will focus on using the Java
binding in this tutorial.

## First Steps

In order to use `kdb` you need to include the dependency in your project. [Here](../../src/bindings/jna/README.md) you can find a detailed tutorial on how to do that.

After that you can start loading a `KDB` object as follows:

```java
Key key = Key.create("user:/kdbsession/javabinding");
try (KDB kdb = KDB.open(key)) {
    //Your code to manipulate keys
} catch (KDB.KDBException e) {
    e.printStackTrace();
}
```

Note that `KDB` implements `AutoClosable` which allows [try-with-resouces](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html).

You can also pass a `Key` object with an empty string on the first line. The passed key (`user:/kdbsession/javabinding` in this case) is being used for the session and stores warnings and error information.

## Fetching keys

First I will show you how you can retrieve a key which is already part of the database. The first thing we need to do is to create a `KeySet` in which our keys will be stored.

```java
KeySet set = KeySet.create();
```

Now we load all keys and provide a parent key from which all keys below will be loaded

```java
kdb.get(set, Key.create("user:/"));
```

Now we can simply fetch the desired key's value as follows:

```java
String str = set.lookup("user:/my/presaved/key").getString()
```

So for example if you had executed the command below via shell, before starting the application:

```bash
kdb set user:/my/test it_works!
```

The method call would return `it_works!`.

```java
String str = set.lookup("user:/my/test").getString()
System.out.println(str) //prints "it works!"
```

## Saving Keys

Next I will show you how to save a new key into the database. First we need need to create an empty `KeySet` again. We also **need to fetch** all keys for the namespace before we will be able to save a new key.

```java
KeySet set = KeySet.create();
Key namespace = Key.create("user:/");
kdb.get(set, namespace);    //Fetch all keys for the namespace
set.append(Key.create("user:/somekey", "myValue"));
kdb.set(set, key);
```

If you try to save a key without fetching it beforehand, a `KDBException` will be thrown, telling you to call get before set.

The _user_ namespace is accessible without special rights, but if you try to write to _system_ you will need to have root
privileges. Take a look at [TESTING.md](/doc/TESTING.md) to see how to access the system namespace as non-root user. This should only be done in testing
environments though as it is not intended for productive systems.

## Examples

### Traversing Keys in a `KeySet`

```java
Key key = Key.create("user:/errors");
try (KDB kdb = KDB.open(key)) {
    KeySet set = KeySet.create();
    Key namespace = Key.create("user:/");       //Select a namespace from which all keys should be fetched
    kdb.get(set, namespace);                  //Fetch all keys into the set object
    for (int i = 0; i < set.length(); i++) {  //Traverse the set
        String keyAndValue = String.format("%s: %s",
                set.at(i).getName(),          //Fetch the key's name
                set.at(i).getString());       //Fetch the key's value
        System.out.println(keyAndValue);
    }
} catch (KDB.KDBException e) {
    e.printStackTrace();
}
```

First we create a new `KDB` object and fetch all keys for the desired namespace, in this example the `user` namespace. Since it saves all
keys in our passed `set` variable we can then iterate through it by a simple for loop.
The `at(int)` method gives us the key on the corresponding position which we will print out in this example.

### Read Multiple Keys From KDB

[This](../../examples/external/java/read-keys-example) example shows how to read multiple keys. It provides comments for further clarification.
Before building and running this example, add first your jar file like described [here](../../examples/external/java/read-keys-example/README.md).
Then build the project with this command from the root directory:

```sh
mvn clean package
```

Afterwards run it with (change VERSION in the command below!):

```sh
java -cp target/read-keys-example-jar-with-dependencies.jar:lib/libelektra5j-VERSION.jar org.libelektra.app.App
```

## Java Plugin Tutorial

For the tutorial on how to write java plugins, please check out [this](/doc/tutorials/java-plugins.md) page.
