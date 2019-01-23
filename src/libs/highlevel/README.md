# High-Level API

## Introduction

The goal of the high-level API is to increase the usability of libelektra for developers who want to integrate Elektra into their
applications. Applications usually do not want to use low-level APIs. `KDB` and `KeySet` are useful for plugins and to implement APIs, but
not to be directly used in applications. The high-level API should be extremely easy to get started with and at the same time it
should be hard to use it in a wrong way. This tutorial gives an introduction for developers who want to elektrify their application
using the high-level API.

The API supports all CORBA Basic Data Types, except for `wchar`, as well as the `string` type (see also [Data Types](#DataTypes)).

## Setup

First you have to add `elektra-highlevel`, `elektra-kdb` and `elektra-ease` to the linked libraries of your application. To be able to
use it in your source file, just include the main header with `#include <elektra.h>` at the top of your file.

The API contains one header that is not automatically included from `elektra.h`. You can use it with `#include <elektra/conversion.h>`.
The header provides the functions Elektra uses to convert your configuration values to and from strings. In most cases, you won't need
to use these functions directly, but they might still be useful sometimes (e.g. in combination with `elektraGetType` and `elektraGetRawString`).

## Quickstart

The quickest way to get started is to adapt the following piece of code to your needs:

```c
ElektraError * error = NULL;
Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", NULL, &error);
if (elektra == NULL)
{
	printf ("An error occurred: %s", elektraErrorDescription (error));
	elektraErrorReset (&error);
	return -1;
}

int myint = elektraGetLong (elektra, "myint");

if (myint < 10)
{
	elektraSetBoolean (elektra, "smallint", true, &error);
	if (error != NULL)
	{
		printf ("An error occurred: %s", elektraErrorDescription (error));
		elektraErrorReset (&error);
	}
}

elektraClose (elektra);
```

The getter and setter functions follow the simple naming scheme `elektra`(`Get`/`Set`)[Type]. Additionally for each one there is an array
element variant with the suffix `ArrayElement`. For more information see [below](#reading-and-writing-values).

You also can find a more complex example [here](../../../examples/highlevel/ReadMe.md).

## Core Concepts

### Struct `Elektra`

`Elektra` is the handle you use to access the underlying KDB (hierarchical key database) that stores the configuration key-value pairs.
All key-value read and write operations expect this handle to be passed as in as a parameter. To create the handle, you simply write:

```c
ElektraError * error = NULL;
Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", NULL, &error);
```

Please replace `"/sw/org/myapp/#0/current"` with an appropriate value for your application (see [Namespaces](/doc/tutorials/namespaces.md)
for more information). You can use the parameter `defaults` to pass a KDB `KeySet` containing `Key`s with default values to the `Elektra`
instance.

The passed in `ElektraError` can be used to check for initialization errors. You can detect initialization errors by comparing the result of
`elektraOpen` to NULL:

```c
if (elektra == NULL)
{
  // handle the error, e.g. print description
  elektraErrorReset(&error);
}
```

If an error occurred, you MUST call `elektraErrorReset` before using the same error pointer in any other function calls (e.g. elektraSet*
calls). It is also safe to call `elektraErrorReset`, if no error occurred.


In order to give Elektra the chance to clean up all its allocated resources, you have to close your instance, when you are done using it,
by calling:

```c
elektraClose (elektra);
```

*NOTE:* Elektra is only thread-safe when you use one handle per thread or protect your handle. If you have multiple threads accessing
key-values, create a separate handle for each thread to avoid concurrency issues.

### Struct `ElektraError`

The library is designed to shield developers from the many errors one can encounter when using KDB directly. However it is not possible
to hide all those issues. As with every library, things can go wrong and there needs to be a way to react to errors once they have occurred
at runtime. Therefore the high-level API introduces a struct called `ElektraError`, which encapsulates all information necessary for the
developer to handle runtime-errors appropriately in the application.

Functions that can produce errors, during correct use of the API, accept an `ElektraError` pointer as parameter, for example:

```c
Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error);
```

In most cases you'll want to set the error variable to `NULL` before passing it to the function. You can do this either by declaring and
initializing a new variable with `ElektraError * error = NULL` or by reusing an already existing error variable by resetting it with
`elektraErrorReset (&error)`.

Notice, that you should always check if an error occurred by comparing it to `NULL` after the function call.

If an error happened, it is often useful to show an error message to the user. A description of what went wrong is provided in the
`ElektraError` struct and can be accessed using `elektraErrorDescription (error)`. A complete list of the provided accessors for
error-details can be found in [elektra_error.c](/src/libs/highlevel/elektra_error.c).

To avoid leakage of memory, you have to call `elektraErrorReset (&error)` (ideally as soon as you are finished resolving the error):

```c
ElektraError * error = NULL;

// Call a function and pass the error variable as an argument.
// ...

if (error != NULL)
{

  // An error occurred, do something about it.
  // ...

  elektraErrorReset (&error);
}
```

#### Low-level Errors

Errors which do not originate inside the high-level API itself are wrapped into a `ElektraError` struct with error code
`ELEKTRA_ERROR_CODE_LOW_LEVEL`. The high-level Error API provides methods (`elektraKDBError*`) to access the properties of the low-level
error. You can also access the key to which the error was originally attached, as well as any possible low-level warnings.

### Configuration

Currently there is only one way to configure an `Elektra` instance:

```c
void elektraFatalErrorHandler (Elektra * elektra, ElektraErrorHandler fatalErrorHandler);
```

This allows you to set the callback called by Elektra, when a fatal error occurs. Technically a fatal error could occur at any time, but
the most common use case for this callback is inside of functions that do not take a separate `ElektraError` argument. For example,
this function will be called, when any of the getter-functions is called on a non-existent key which is not part of any specification,
and therefore has no specified default value.

The handler will also be called whenever you pass `NULL` where a function expects an `ElektraError **`. In this case the error code will be
`ELEKTRA_ERROR_CODE_NULL_ERROR`.

The default callback simply logs the error with `ELEKTRA_LOG_DEBUG` and then calls `exit()` with the error code of the error. It is highly
recommended you either use `atexit()` in you application or set a custom callback, to make sure you won't leak memory.

The callback must interrupt the thread of execution in some way (e.g. by calling `exit()` or throwing an exception in C++). It must
not return to the calling function. If it does return, the behavior is undefined.

## <a name="DataTypes"></a> Data Types

The API supports the following types, which are taken from the CORBA specification:

* **String**: a string of characters, represented by `KDB_TYPE_STRING` in metadata
* **Boolean**: a boolean value `true` or `false`, represented by `KDB_TYPE_BOOLEAN` in metadata, in the KDB the raw value `"1"` is
               regarded, as true, any other value is considered false
* **Char**: a single character, represented by `KDB_TYPE_CHAR` in metadata
* **Octet**: a single byte, represented by `KDB_TYPE_OCTET` in metadata
* **(Unsigned) Short**: a 16-bit (unsigned) integer, represented by `KDB_TYPE_SHORT` (`KDB_TYPE_UNSIGNED_SHORT`) in metadata
* **(Unsigned) Long**: a 32-bit (unsigned) integer, represented by `KDB_TYPE_LONG` (`KDB_TYPE_UNSIGNED_LONG`) in metadata
* **(Unsigned) Long Long**: a 64-bit (unsigned) integer, represented by `KDB_TYPE_LONG_LONG` (`KDB_TYPE_UNSIGNED_LONG_LONG`) in metadata
* **Float**: whatever your compiler treats as `float`, probably IEEE-754 single-precision, represented by `KDB_TYPE_FLOAT` in metadata
* **Double**: whatever your compiler treats as `double`, probably IEEE-754 double-precision, represented by `KDB_TYPE_DOUBLE` in metadata
* **Long Double**: whatever your compiler treats as `long double`, not always available, represented by `KDB_TYPE_LONG_DOUBLE` in metadata


## <a name="reading-and-writing-values"></a> Reading and Writing Values

### Key Names

When calling `elektraOpen` you pass the parent key for your application. Afterwards getters and setters get passed in only the part below
that key in the KDB. For example, if you call `elektraOpen` with `"/sw/org/myapp/#0/current"`, you can access your applications
configuration value for the key `"/sw/org/myapp/#0/current/message"` with the provided getters and setters by passing them only
`"message"` as the name for the configuration value.

### Read Values from the KDB

A typical application will want to read some configuration values at start. This should be made as easy as possible for the developer.
Reading configuration data in most cases is not part of the business logic of the application and therefore should not "pollute" the
applications source code with cumbersome setup and file-parsing code. This is exactly where Elektra comes in handy, because you can leave
all the configuration file handling and parsing to the underlying layers of Elektra and just use the high-level API to access the desired
data. Reading values from KDB can be done with elektra-getter functions that follow a simple naming scheme:

`elektraGet` + the type of the value you want to read.

For example, you can get the value for the key named "message" like this:

```c
const char * message = elektraGetString (elektra, "message");
```

Sometimes you'll want to access arrays as well. You can access single elements of an array using the provided array-getters following
again a simple naming scheme:

`elektraGet` + the type of the value you want to read + `ArrayElement`.

For example, you can get the third value for the array "message" like this:

```c
const char * message = elektraGetStringArrayElement (elektra, "message", 3);
```

To get the size of the array you would like to access you can use the function `elektraArraySize`:

```c
size_t arraySize = elektraArraySize (elektra, "message");
```

For some background information on arrays in Elektra see the [Application Integration](/doc/tutorials/application-integration.md) document.
Notice that both the getters for primitive types and the getters for array types do not accept error parameters. The library expects you to
run a correct Elektra setup. If the configuration is well specified, no runtime errors can occur when reading a value. Therefore the
getters do not accept an error variable as argument. If there is however a severe internal error, or you try to access a key which you have
not specified correctly, then the library will call the error callback set with `elektraFatalErrorHandler` to prevent data inconsistencies
or exceptions further down in your application.

You can find the complete list of the available functions for all supported value types in [elektra.h](/src/include/elektra.h)

### Writing Values to the KDB

Sometimes, after having read a value from the KDB, you will want to write back a modified value. As described in
[Read values from the KDB](#read-values-from-the-kdb) we follow a naming scheme for getters. The high-level API provides setters follow
an analogous naming scheme as well. For example, to write back a modified "message", you can call `elektraSetString`:

```c
elektraSetString (elektra, "message", "This is the new message", NULL);
```

The counterpart for array-getters again follows the same naming scheme:

```c
elektraSetStringArrayElement (elektra, "message", "This is the third new message", NULL);
```

Because even the best specification and perfect usage as intended can not fully prevent an error from occurring, when saving the
configuration, all setter-functions take an additional `ElektraError` argument, which will be set if an error occurs. Currently setters
should not call the fatal error handler, however, there are no guarantees that this will remain as such. For example a valid reason for
a setter to call the fatal error handler instead of returning an error, would be problems with memory allocation (from which an application
is unlikely to recover).

### Enum Values

Reading enum values is a special case, because the compiler is not able to infer the enum type from the key alone. Therefore you can
either use the function `int elektraGetEnumInt (Elektra * elektra, char * keyName)`, and deal with the raw integer yourself, or use the
convenience macro `elektraGetEnum(elektra, keyname, enumType)`, which calls `elektraGetEnumInt` and then casts to `enumType`.

```c
typedef enum { A, B, C } MyEnum;

ElektraError * error = NULL;
Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", NULL, &error);

// Read raw int value
int value = elektraGetEnumInt (elektra, "message");
MyEnum enumValue = elektraGetEnum(elektra, "message", MyEnum);

// enumValue == (MyEnum) value

elektraClose (elektra);
```

To write an enum value use `void elektraSetEnumInt (Elektra * elektra, char * name, int value, ElektraError ** error)`. A convenience
macro is not provided, because you can simply pass the enum value as an `int` argument.

Similar functions and macros are provided for array elements:

```c
int elektraGetEnumIntArrayElement (Elektra * elektra, char * keyName, size_t index);

elektraGetEnumArrayElement(elektra, keyname, index, enumType);

void elektraSetEnumIntArrayElement (Elektra * elektra, char * name, size_t index, int value, ElektraError ** error);
```

### Raw Values

You can use `const char * elektraGetRawString (Elektra * elektra, const char * name)` to read the raw (string) value of a key. No type checking
or type conversion will be attempted. Additionally this function is guaranteed to not call the fatal error handler. It will simply return
`NULL`, if the key was not found.

If you want to set a raw value (e.g. if you want to extend the API with your own custom types), use
`void elektraSetRawString (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error)`. Obviously you have
to provide a type for the value you set, so that the API can perform type checking, when reading the value next time.

Similar functions are provided for array elements:

```c
const char * elektraGetRawStringArrayElement (Elektra * elektra, const char * name, size_t index);

void elektraSetRawStringArrayElement (Elektra * elektra, const char * name, size_t index, const char * value, KDBType type, ElektraError ** error);
```

#### Type Information

The type information is stored in the `"type"` metakey. `KDBType elektraGetType (Elektra * elektra, const char * keyname)` (or
`KDBType elektraGetArrayElementType (Elektra * elektra, const char * name, size_t index)` for array elements) lets you access this
information. A setter is not provided, because changing the type of a key without changing its value rarely, if ever, makes sense.

### Binary Values

The high-level API does not support binary key values at this time.

## Example

```c
#include <stdio.h>
#include <elektra.h>

int main ()
{
  ElektraError * error = NULL;
  Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", NULL, &error);

  if (elektra == NULL)
  {
    printf ("Sorry, there seems to be an error with your Elektra setup: %s\n", elektraErrorDescription (error));
    elektraErrorReset (&error);

    printf ("Will exit now...\n");
    exit (EXIT_FAILURE);
  }

  const char * message = elektraGetString (elektra, "message");

  printf ("%s", message);

  elektraClose (elektra);

  return 0;
}
```
