import kotlinx.serialization.Serializable
import org.libelektra.Key
import org.libelektra.ReadableKey
import org.libelektra.dsl.keyOf
import org.libelektra.dsl.keySetOf
import org.libelektra.kdbExt.withKDB
import org.libelektra.keyExt.get
import org.libelektra.keyExt.getMetaOrNull
import org.libelektra.keyExt.isEmpty
import org.libelektra.keySetExt.convert
import org.libelektra.keySetExt.serialformat.KeySetFormat

fun main() {
    // Example 1: create a Key and print it
    println("Example 1")

    var key = keyOf("user:/hello_world", "Hello World")
    println(key) // to get name
    println(key.get<String>())
    println()

    key = keyOf("user:/hello_foo") {
        value = "Hello Foo"
        metaKey("meta:/meta1", "value1")
        metaKey("meta:/meta2", "value2")
    }
    println(key) // to get name
    println(key.get<String>()) // get value

    // Getting meta keys
    println(key.getMetaOrNull("meta:/meta1"))
    println(key.getMetaOrNull("meta:/non-existing"))
    println()

    // Example 2: create a KeySet and print it
    println("Example 2")
    var ks = keySetOf {
        key(key)
        key("user:/hello_world2") {
            value = "Hello World2"
        }
    }
    ks.forEach {
        println("iter: ${it.name} = ${it.string}")
    }
    println(ks)
    println()

    // Example 3: reading from the KDB
    println("Example 3")

    withKDB {
        ks = get(key)
    }
    println(ks)
    println()

    // Example 4: checking for empty keys
    println("Example 4")

    key = keyOf("user:/hello")
    if (key.isEmpty()) {
        println("Key is empty!")
    }
    println()

    // Example 5: converting to a class (serialization plugin is needed for this feature, org.jetbrains.kotlin.plugin.serialization)
    println("Example 5")

    @Serializable
    data class ServerConfig(val ip: String, val port: Int, val hostname: String?)

    ks = givenAServerConfigKeySet()

    val myConfig = ks.convert<ServerConfig>()
    println(myConfig)
    val port = myConfig.port
    println(port)
    println()

    // Example 6: converting to a list
    println("Example 6")

    ks = givenAStringArrayKeySet()

    // providing a parentKey is optional (and not needed in this case)
    val names: List<String> = ks.convert(parentKey = "/names")
    println(names)
    println()

    // Example 7: converting to a map
    println("Example 7")

    ks = givenAKeySetWithUnknownKeys()

    val myMap = ks.convert<Map<String, String>>()

    println(myMap)
    println()

    // Example 8: encoding to KeySet
    println("Example 8")

    @Serializable
    data class UserConfig(val name: String, val permissions: Int, val additionalInfo: Map<String, String>)

    val myUserConfig = UserConfig(
            name = "john",
            permissions = 777,
            additionalInfo = mapOf(
                    "isAdmin" to "true",
                    "lastLogin" to "2022-05-05"
            )
    )

    ks = KeySetFormat.encodeToKeySet(myUserConfig, parentKey = "user:/my/user")

    ks.forEach {
        println("${it.name} = ${it.string}")
    }
    println()

    // Example 9: adding basename for Key
    println("Example 9")
    val subKey = Key.create("user:/hello_world", "This is a sub-key's value")
    subKey.addBaseName("sub_key")
    println(subKey.name)
    println(subKey.string)
    println()

    // Example 10: separate key sets into alphanumeric and numeric values
    println("Example 10")

    ks = givenAServerConfigKeySet()

    val listPair = ks.partition { k -> k.string.matches("-?[0-9]+(\\.[0-9]+)?".toRegex()) }

    println("numeric: ")
    listPair.first.forEach {
        println("${it.name} = ${it.string}")
    }
    println("alphanumeric: ")
    listPair.second.forEach {
        println("${it.name} = ${it.string}")
    }
    println()

    // Example 11: update metadata of a key
    println("Example 11")
    key = Key.create("user:/key/with/meta")
    key.setMeta("exampleKey", "useless meta value info")
    key.getMeta("exampleKey")
        .ifPresent { rk: ReadableKey -> println("This is " + rk.string) }
    key.setMeta("exampleKey", "very helpful data")
    key.getMeta("exampleKey")
        .ifPresent { rk: ReadableKey -> println("This is " + rk.string) }
    println()

}

fun givenAServerConfigKeySet() = keySetOf(
        keyOf("user:/server", "This is my server"),
        keyOf("user:/server/ip", "192.168.0.1"),
        keyOf("user:/server/port", 8080),
        keyOf("user:/server/hostname")
)

fun givenAStringArrayKeySet() = keySetOf(
        keyOf("user:/names") {
            metaKey("meta:/array", "#2")
        },
        keyOf("user:/names/#0", "john"),
        keyOf("user:/names/#1", "jane"),
        keyOf("user:/names/#2", "adam"),
)

fun givenAKeySetWithUnknownKeys() = keySetOf(
        keyOf("user:/uno", "123"),
        keyOf("user:/dos", 2),
        keyOf("user:/tres", 3.0),
)


