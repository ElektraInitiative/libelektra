This library is an extension of the Java Elektra Library to leverage the full potential of Kotlin.  
It provides several extension functions, factory methods, utilities and serialization.

_The library is currently not published anywhere. If that ever happens, it will be installed with Gradle_

# User-facing API

## KeySet to Class conversion

You can convert a KeySet to a data class and vice versa.  
Supports primitive values, additional data classes marked with @Serializable, collections and maps.

```kotlin
val ks = keySetOf(
        keyOf("/server/ip", "10.0.0.1"),
        keyOf("/server/port", 8080)
)

@Serializable
data class ServerConfig(val ip: String, val port: Int)

val config = ks.convert<ServerConfig>()
// or ks.convert<ServerConfig>(parentKey = "/server")
// or KeySetFormat.decodeFromKeySet(ks)
// or KeySetFormat.decodeFromKeySet(ks, parentKey = "/server")

// modify config...

val newKeySet: KeySet = KeySetFormat.encodeToKeySet(config)
// yields
// /ip = 10.0.0.1
// /port = 8080
```

If the parent Key also has an important value, add a property with serial name `parentValue` (or name it `parentValue`) to the data class.

```kotlin
val ks = keySetOf(
        keyOf("/server", "server of foo"),
        keyOf("/server/ip", "10.0.0.1"),
        keyOf("/server/port", 8080)
)

@Serializable
data class ServerConfig(val ip: String, val port: Int, @SerialName("parentValue") val description: String)

val config = ks.convert<ServerConfig>()
// Config(ip = "10.0.0.1", port = 8080, description = "server of foo")
```

It is also possible to add an elektra array easily with the following construct:

```kotlin
KeySetFormat.encodeToKeySet(listOf("something", "another"))
```


## KDB try-with-resources shortcut

You can open, use and close KDB with this function:

```kotlin
withKdb {
    // context of KDB object
    val keySet = get("user:/test")
}
```

## Key utilities

Creating keys:

```kotlin
// With simple function
val parentKey = keyOf(name = "user:/test", value = "foo")
val childKey = keyOf(name = "user:/test/age", value = 45)
val keyWithMeta = keyOf(
        "user:/test/name",
        "john",
        keyOf("meta:/type", "string")
)

// With builder
val key = keyOf("/test") {
    value = "1234"
}

val key = keyOf("/test") {
    value = "1234"
    metaKey("meta:/meta1", "value1")
    metaKey("meta:/meta2", "value2")
}
```

Empty checks:

```kotlin
// True when the key has no value (= null) or 0 bytes are stored
key.isEmpty()
key.isNotEmpty()
```

Using key name parts:

```kotlin
key.nameParts.forEach {
    println(it)
}

key.nameParts.filter {
    it.startsWith("foo")
}

key.nameParts.map {
    it.uppercase()
}

key.nameParts.toList()
```

Getting meta keys:

```kotlin
val meta: ReadableKey? = key.getMetaOrNull("meta:/metakey")
```

Array check and utilities:

```kotlin

// Returns last array index parsed as integer or null when the key has no array meta key
val lastIndex: Int? = key.lastArrayIndexOrNull()

// Parses last array index as integer from array meta key
val lastIndexFromMeta: Int = metaKey.parseIndex()

// Converts an integer to correct elektra array index (#_24 in this case)
val index: String = 24.toElektraArrayIndex()
```

Type-safe get and set operations:

```kotlin
// get returns the type inferred from usage (only for primitives and strings)
val d: Double = key.get()
val i = key.get<Int>()

// Returns value when not empty, or null otherwise
val stringOrNull = key.getOrNull<String>()

// Sets key value according to type
key.set("foo")
key.set(123)
key.set(0.0)
key.set(123L)
// Sets key to empty value
key.set(null)
```

## KeySet utilities

Creating KeySets:

```kotlin
// Simple function
val emptyKeySet = keySetOf()

val singleKeySet = keySetOf(
        keyOf("/test")
)

// Builder
val complexKeySet = keySetOf {
    key(keyOf("/test", "value"))

    key(keyOf("/test/foo", "bar"))
}
```

## KeySet serialization

KeySets can be serialized for all KotlinX Serialization formats.  
e.g. Json:

```kotlin
val ks = keySetOf(
        keyOf("/my/name", "john"),
        keyOf("/my/age", 18)
)

val json = Json.encodeAsString(KeySetSerializer(), ks)
/* yields:
* {
*   "/my/name": "john",
*   "/my/age": "18"
* }
*/
```
