# How-To: Java kdb

## Introduction

When programming in Java it is possible to access the kdb database, changing values of existing keys or adding new ones and a few other things.

## First Steps

In order to use `kdb` you will need include the dependency in your project. [Here](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/jna) you can find a detailed tutorial on how to do that.

After that you can start loading an `KDB` object as follows:
```java
Key key = Key.create("user/kdbsession/javabinding");
try (KDB kdb = KDB.open(key)) {
    //Your code to manipulate keys
} catch (KDB.KDBException e) {
    e.printStackTrace();
}
```

Note that KDB implements `AutoClosable` which allows for [try-with-resouces](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html).


You can also pass a `Key` object with an empty string. The passed key is being used for the session and stores warnings and error informations.

## Fetching keys

First I will show how to get a key which was already saved in the database. The first thing we need to do is to create a `KeySet` in which our key(s) will be loaded.
```java
KeySet set = KeySet.create(0);
```
The `0` means that initially we allocate no space for any keys.

Now we load all keys and provide a parent key from which all keys below will be loaded
```java
kdb.get(set, Key.create("user"));
```
Now we can simply fetch the desired key's value as follows:
```java
String str = set.lookup("user/my/presaved/key").getString()
```

## Saving Keys

Next I will show how to save a new key into the database. First we need need to create an empty `KeySet` again. We also **need to fetch** all keys for the namespace before we will be able to save a new key. 

```java
KeySet set = KeySet.create(0);
Key namespace = Key.create("user");
kdb.get(set, namespace);    //Fetch all keys for the namespace
set.append(Key.create("user/somekey", "myValue"));
kdb.set(set, key);
```

If you try to save a key without fetching it beforehand, a `KDBException` will be thrown, telling you to call get before set.

The *user* namespace is accessible without special rights, but if you try to write to *system* you will need to have the application run as root or give write access to `/etc/kdb` for the corresponding user which executes the application.

## Examples

### Traversing Keys in a `KeySet`

```java
Key key = Key.create("user/errors");
try (KDB kdb = KDB.open(key)) {
    KeySet set = KeySet.create(0);
    Key namespace = Key.create("user");
    kdb.get(set, namespace);
    for (int i = 0; i < set.length(); i++) {
        String keyAndValue = String.format("%s: %s",
                set.at(i).getName(),
                set.at(i).getString());
        System.out.println(keyAndValue);
    }
} catch (KDB.KDBException e) {
    e.printStackTrace();
}
```