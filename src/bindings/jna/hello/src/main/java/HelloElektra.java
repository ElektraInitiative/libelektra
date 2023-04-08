import org.libelektra.*;
import org.libelektra.plugin.Echo;

/** Simple hello world to see how Elektra can be used in Java. */
public class HelloElektra {

  public static void main(String[] args) {
    // Example 1: create a Key and print it
    System.out.println("Example 1");
    final Key key = Key.create("user:/hello_world", "Hello World");
    System.out.println(key); // to get name
    System.out.println(key.getString());
    System.out.println();

    // Example 2: adding basename for Key
    System.out.println("Example 2");
    Key subKey = Key.create("user:/hello_world", "Sub Key");
    subKey.addBaseName("sub_key");
    System.out.println(subKey.getName());
    System.out.println(subKey.getString());
    System.out.println();

    // Example 3: create a KeySet and print it
    System.out.println("Example 3");
    final KeySet ks = KeySet.create(10, Key.create("user:/hello_world2", "Hello World2"), key);
    for (Key k : ks) {
      System.out.println("iter: " + k.getName() + " " + k.getString());
    }
    System.out.println(ks);
    System.out.println();

    // Example 4: duplicating a KeySet
    System.out.println("Example 4");
    final KeySet ks2 = ks.dup();
    ks2.copy(ks);
    System.out.println(ks2.size());
    ks2.append(ks);
    ks2.append(key);
    System.out.println();

    // Example 5: reading from the KDB
    System.out.println("Example 5");
    try (final KDB kdb = KDB.open(key)) {
      kdb.get(ks, key);
      ks.lookup(key).ifPresent(k -> System.out.println(k.getString()));
    } catch (KDBException e) {
      System.out.println(e);
    }
    System.out.println();

    // Example 6: manually calling a Plugin (normally not needed)
    System.out.println("Example 6");
    final Echo dp = new Echo();
    dp.open(ks, key);
    dp.get(ks, key);
    dp.close(key);
    System.out.println();

    // Example 7: Keys support different types
    System.out.println("Example 7");
    final Key b = Key.create("user:/boolean", "true");
    System.out.println(b.getBoolean());
    b.setBoolean(false);
    System.out.println(b.getBoolean());
    System.out.println();

    // Example 8: iterating over keyname parts of a Key
    System.out.println("Example 8");
    Key n = Key.create("user:/weird\\/name///\\\\/is/\no/_\\\\problem");
    n.keyNameIterator().forEachRemaining(s -> System.out.println("itername: " + s));
    System.out.println();

    // Example 9: cutting part of a KeySet
    System.out.println("Example 9");
    Key cutpoint = Key.create("user:/cutpoint"),
        ka = Key.create("user:/cutpoint/hello", "hiback"),
        kb = Key.create("user:/cutpoint/hello2", "hellotoo"),
        kc = Key.create("user:/outside/cutpoint/hello", "hellothere");
    KeySet whole = KeySet.create(ka, kb, kc);
    System.out.println("Whole:");
    System.out.println(whole);

    KeySet cut = whole.cut(cutpoint);
    System.out.println("Cut:");
    System.out.println(cut);
    System.out.println("Rest:");
    System.out.println(whole);
    System.out.println();

    exampleSetMetaKeys();

    exampleSetArrayMetaKey();

    exampleCheckKeyAvailableInKDB();

    exampleUpdateMetaKey();

    exampleKeyIterDumpMetaData();
  }

  private static void exampleSetMetaKeys() {
    // Example 9: Set and get meta keys
    System.out.println("Example 9");
    Key key = Key.create("user:/key/with/meta");
    key.setMeta("example", "anExampleValue");
    var returnedMeta = key.getMeta("example").orElseThrow(AssertionError::new);
    System.out.println("Value of meta key 'example': " + returnedMeta.getString());
    System.out.println();
  }

  private static void exampleSetArrayMetaKey() {
    // Example 10: Create an array using meta keys
    System.out.println("Example 10");
    Key array = Key.create("user:/array");
    // Create an array with length 2
    array.setMeta("array", "#1");

    Key firstEntry = Key.create("user:/array/#0/test");
    firstEntry.setString("first");

    Key secondEntry = Key.create("user:/array/#1/test");
    secondEntry.setString("second");

    KeySet ks = KeySet.create(array, firstEntry, secondEntry);

    ks.forEach(
        key -> {
          System.out.print(key + " = " + key.getString());

          System.out.print(" | Meta: ");
          for (ReadableKey metaKey : key) {
            System.out.print(metaKey + " = " + metaKey.getString());
          }
          System.out.println();
        });
    System.out.println();
  }

  private static void exampleCheckKeyAvailableInKDB() {
    // Example 11: Check whether the keys are already available in the key database
    System.out.println("Example 11");
    Key parentKey = Key.create("user:/e11");
    Key existingKey = Key.create("user:/e11/exists", "e11Val");
    Key notExistingKey = Key.create("user:/e11/doesNotExist", "e11ValNot");

    try (KDB kdb = KDB.open()) {
      var keySet = kdb.get(parentKey);
      keySet.clear();
      keySet.append(existingKey);
      kdb.set(keySet, parentKey);
    } catch (KDBException e) {
      System.out.println(e);
    }

    // now retrieve them
    try (KDB kdb = KDB.open()) {
      KeySet keySet = kdb.get(parentKey);

      var ek = keySet.lookup(existingKey);

      if (ek.isPresent()) {
        Key loadedExistingKey = ek.get();
        System.out.println(
            loadedExistingKey
                + " is present and its value "
                + loadedExistingKey.getString()
                + " loaded.");
      } else {
        System.out.println(existingKey + " is not present. Setting key.");
        keySet.append(existingKey);
        kdb.set(keySet, parentKey);
      }

      var nek = keySet.lookup(notExistingKey);

      if (nek.isPresent()) {
        Key loadedNotExistingKey = nek.get();
        System.out.println(
            loadedNotExistingKey
                + " is present and its value "
                + nek.get().getString()
                + " loaded.");
      } else {
        System.out.println(notExistingKey + " is not present. Setting key.");
        keySet.append(notExistingKey);
        kdb.set(keySet, parentKey);
      }

    } catch (KDBException e) {
      System.out.println(e);
    }
    System.out.println();
  }

  private static void exampleUpdateMetaKey() {
    // Example 12: Update meta data on a key
    System.out.println("Example 12");
    Key key = Key.create("user:/key/with/meta");
    key.setMeta("example", "anExampleValue");
    key.getMeta("example")
        .ifPresent(k -> System.out.println("Set new meta data: " + k.getString()));
    key.setMeta("example", "anUpdatedExampleValue");
    key.getMeta("example")
        .ifPresent(k -> System.out.println("Updated meta data: " + k.getString()));
    System.out.println();
  }

  private static void exampleKeyIterDumpMetaData() {
    // Example 13: Update meta data on a key
    System.out.println("Example 13");
    Key k1 = Key.create("user:/ex13/Hallo");
    k1.setMeta("lang", "GER");
    Key k2 = Key.create("user:/ex13/Hello");
    k2.setMeta("lang", "ENG");
    Key k3 = Key.create("user:/ex13/Hola");
    k3.setMeta("lang", "ESP");

    final KeySet ks = KeySet.create(10, k1, k2, k3);
    for (Key key : ks) {
      key.getMeta("lang")
          .ifPresent(
              k -> System.out.println("Language of Key " + key.getName() + " is " + k.getString()));
    }
    System.out.println();
  }
}
