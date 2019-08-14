# Language Bindings

## Introduction

In this section, we will explain the how to write a language binding for Elektra.

## TODO

1. which parts of Elektra bindings make sense (application, plugin, tools, ...)
2. how to integrate bindings into CMake (if possible and useful)
3. which parts of the bindings can and should differ for every language. This includes:
   1. iterators
   2. conversion to native types (strings, int, ...)
   3. operator overloading (if available)
   4. other programming language integrations (streams, hash-codes, identity, ...)
   5. returned errors from kdb functions (what this issue here is about)

## Error Handling

Since v0.9.0, Elektra has a new error code system. You might want to take a look in the [design decision](../decisions/error_codes.md)
first to understand the concept of the error codes. These codes are hierarchically structured
and are therefore perfectly suitable for inheritance if the language supports it.

Some error codes like the `Permanent Errors` are generalizations and used for developers who want to catch
all specific types of errors (e.g., it does not matter if it is a Resource or Installation Error but
the developer wants to check for both). Such errors should not be able to "instantiate" or emitted
back to Elektra as we want to force developers to take a more specific category. In case of
Java for example the `Permanent Error` is an abstract class.
Here is an example of how Java has implemented it:

```java
public abstract class PermanentException extends Exception {...}
	public class ResourceException extends PermanentException {...}
		public class MemoryAllocationException extends ResourceException {...}
	public class InstallationException extends PermanentException {...}
...
```

You can see a list of all errors that can be used in the [specification file](../../src/error/specification).
The hierarchy itself is depicted in the [design decision](../decisions/error_codes.md).

If you have a language which does not support inheritance this way like GoLang, you can still use the
error code itself since the hierarchy is integrated in it. For example you can check if the code starts with
`C01...` to catch all `Permanent Errors`.

### Error Message

In Elektra every error has a predefined format. You can take a look at the [related design decision](../decisions/error_message_format.md)
to see how it looks like.

Every Exception/Error struct/etc. should have separate accessors to individual parts of the message.
These include:

1. Module (getModule())
2. Error Code (getErrorCode())
3. Reason (getReason())
4. Configfile (getConfigFile())
5. Mountpoint (getMountpoint())
6. Debuginformation (text looks like "At: file:line") (getDebugInformation())

In case of an error at least the following part has to be returned:

```
Sorry, module getModule() issued error getErrorCode():
getReason()
```

Please also keep the wording identical for consistency.
Take a look how the Java Binding implemented it in the KDBException (TODO: Provide link after merge).
