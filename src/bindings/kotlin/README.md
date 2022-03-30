This is an extension of the Java/JNA binding to provide convenience methods for Kotlin.

# Interactions with users

## Functional paradigm
We can abstract the try-with-resources access for kdb using lambdas in Kotlin:

General kdb usage:

```kotlin
withKdb {
    get(..)
    set(..)
}
```

Plugin calls:
```kotlin
withPlugin("range") {
    val rangeKey = keyOf("user:/tests/myError" to "30").apply {
        setMeta("check/range", "1-20")
        // or alternatively
        meta = "check/range" to "1-20"
    }
    // or
    val rangeKey = keyOf(
        value = "user:/tests/myError" to "30",
        meta = "check/range" to "1-20"
    )
 
    val ks = keySetOf(allocationHint = 10, rangeKey)
    kdbSet(ks, parentKey)
}
```

Shifting the imperative style of Java to a more Kotlin-idiomatic and functional style.
We omitted the error handling in this example, but the function withKdb would also throw the KDBException.

## Type inference and assertion
Kotlin also improves the type system in comparison to Java.
We envision to implement the following:

```kotlin
key.get<String>()
key.get<Boolean>()

// Automatic type inference
val foo: Boolean = key.get()
key.set("bar")
key.set(true)

data class MyProperties(val foo: String, val bar: Boolean)

// Existing keys in kdb:
// user:/parent/foo
// user:/parent/bar
val props: MyProperties = keySetOf("user:/parent").get()
val props = keySetOf("user:/parent").as<MyProperties>()
val props = fromKdb<MyProperties>("user:/parent")
val props = kdb.decode<MyProperties>("user:/parent")
val props = kdb.build<MyProperties>("user:/parent")
```


## Functional KeySet traversal
```kotlin
keySet.forEach { key ->
    
    // Key: /foo/bar = bloo
    key.parts.forEach { part -> 
        // Prints: foo bar
        print(part)
    }
}

keySet.values.forEach { value ->
    print(value)
}
```

## Builder functions:

```kotlin
keySetOf(
    keyOf("user:/foo"),
    keyOf("user:/bar")
)
```
# The Architecture

For now, this binding sits on top of the Java/JNA binding, i.e. it has a dependency on it.
The binding uses Java functions and extend them to provide 'kotlinized' versions of the Elektra API.

The main points of extensions are:
- KeySet
- Key
- KDB
- Plugin