import org.libelektra.dsl.keyOf

fun main() {
    // Example 1: create a Key and print it
    println("Example 1")
    var key = keyOf("user:/hello_world", "Hello World")
    println(key); // to get name
    println(key.getString());
    println();

    var key = keyOf {
        name("user:/hello_foo")
        value("Hello Foo")
        metaKey("meta:/meta1", "value1")
        metaKey("meta:/meta2", "value2")
    }
    println(key); // to get name
    println(key.getString());
    println();

    // Example 2: create a KeySet and print it
    println("Example 2");
    var ks = keySetOf{
        key(key)
        key("user:/hello_world2", "Hello World2")
    }
    for (Key k : ks) {
        println("iter: " + k.getName() + " " + k.getString());
    }
    println(ks);
    println();

    // Example 3: reading from the KDB
    println("Example 3")

    withKDB{
        set(key)
        ks = get(key)
        val k: Key = ks.lookup(key).orElseThrow { AssertionError() }
    }
    println()

}
