This library is an extension of the Java Elektra Library to leverage the full potential of Kotlin.  
It provides several extension functions, factory methods, utilities and serialization.

_The library is currently not published anywhere. If that every happens, it will be installed with Gradle_

# User-facing API

## KeySet serialization

We will use the following KeySet as an example for this section:

/test  
/test/foo = "foo"  
/test/bar/name = "john"  
/test/bar/age = 34  
/test/bar/address = "Main"  
/test/bar/address/number = 123  
/test/bar/address/street = "Sesame Street"  
/test/colors (meta:/array = #2)  
/test/colors/#0 "red"  
/test/colors/#1 "green"  
/test/colors/#2 "blue"

Converting to JSON:

```kotlin
keySet.toJSON()

// Results in:
"""
{
    "test": {
        "foo": "foo",
        "bar": {
            "name": "john",
            "age": "34",
            "address": {
                "parentValue": "Main",
                "number": "123",
                "street": "Sesame Street"
            },
        },
        "colors": ["red", "green", "blue"]
    }
}
"""

keySet.toJSON("/test/bar/address")

// Results in :
"""
{
    "address": {
        "parentValue": "Main",
        "number": "123",
        "street": "Sesame Street"
    }
}
"""
```

All values are set as Strings.  
Key values of the resulting JSON properties are stored in a special property "parentValue", since they would disappear otherwise.  
Elektra arrays are converted to JSON arrays.

The JSON conversion also enables **serialization to Kotlin data classes** via Kotlinx Serialization.

```kotlin
@Serializable
data class TestProperties(
        val foo: String,
        val bar: BarProperties,
        val colors: List<String>
)

@Serializable
data class BarProperties(
        val name: String,
        val age: Int,
        val address: Address
)

// We can omit the parentValue here if we want (in fact, any property can be omitted)
@Serializable
data class Address(
        val number: Int,
        val street: String
)

val myProps: TestProperties = keySet.convert()
// or val myProps = keySet.convert<TestProperties>()

val colors: List<String> = keySet.convert("/test/colors")
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
val key = keyOf {
    name("/test")
    value("1234")
}

val key = keyOf {
    name("/test")
    value("1234")
    metaKey("/meta1", "value1")
    metaKey("/meta2", "value2")
}
```

Empty checks:

```kotlin
// True when the stored value is equal to ""
key.isEmpty()
key.isNotEmpty()
```

Keyname iterator:

```kotlin
// Always starts with "\u0001"
key.forEachKeyName {
    print(it)
}
```

Conversion from Optional to nullable type:

```kotlin
key.getMeta("/metakey").orNull()
```

Array check and utilities:

```kotlin
// True when key has a metakey "array" with a correctly formatted array index as value
val isArray: Boolean = key.isArray()

// Returns last array index parsed as integer (crashes when key.isArray() == false)
val lastIndex: Int = key.getLastArrayIndex()

// Parses last array index as integer from array metakey
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

// Returns value when not empty, or default otherwise
val longOrDefault = key.getOrDefault<Long>(25L)
val floatOrDefault = key.getOrDefault<Long> {
    val x = calcDefaultLong()
    transformDefault(x) // result will be used as default
}

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
