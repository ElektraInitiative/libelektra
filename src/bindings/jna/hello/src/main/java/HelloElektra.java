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
      Key k = ks.lookup(key).orElseThrow(AssertionError::new);
      System.out.println(k.getString());
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
  }

  private static void exampleSetMetaKeys() {
    // Example 9: Set and get meta keys
    System.out.println("Example 9");
    Key key = Key.create("user:/key/with/meta");
    key.setMeta("example", "anExampleValue");
    var returnedMeta = key.getMeta("example").orElseThrow(AssertionError::new);
    System.out.println("Value of meta key 'example': " + returnedMeta.getString());
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
  }
}
