
# High-Level API

## Introduction

The goal of the high-hevel API is to increase the usability of libelektra for developers who want to integrate Elektra into their applications. Projects usually do not want to use low-level APIs.
`KDB` and `KeySet` are useful for plugins and to implement APIs but not to be directly used in applications. 
The high-level API should be extremely easy to get started with and at the same time it should be hard to use it in a wrong way. 
This tutorial gives an introduction for developers who want to elektrify their application using the high-level API.

## Setup

First you have to add `elektra-highlevel` to the linked libraries of your application. To be able to use it in your source file, just include the main header with `#include <elektra.h>` at the top of your file.

## Core Concepts

### Struct `Elektra`
`Elektra` is the handle you use to access the underlying KDB (hierchical key database) that stores the configuration key-value pairs. All key-value read and write operations expect this handle to be passed as in as a parameter. To create the handle, you simply write:

```c
ElektraError * error = NULL;
Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", &error);
```

Please replace `"/sw/org/myapp/#0/current"` with an appropriate value for your application (see [Namespaces](/libelektra/doc/tutorials/namespaces.md) for more information).

The passed in `ElektraError` can be used to check for initialization errors. You can detect initialization errors by comparing it to NULL after calling `elektraOpen`:

```c
if (error != NULL) 
{
  // An error occured
}
```

In order to give Elektra the chance to clean up its all allocated ressources, you have to close your instance, when you are done using it, by calling:

```c
elektraClose (elektra);
```

Notice that Elektra is is only thread-safe when you use a handle per thread or protect your handle. If you have mutliple threads accessing key-values, create a separate handle for each thread to avoid concurrency issues.

### Struct `ElektraError`
The library is designed to hide as many problems a developer can encounter when usign KDB directly. However it is not possible to hide all those issues. As with every library, things can go wrong and there needs to be a way react to errors once they have occurred at runtime. Therefore the high-level API introduces a simplified struct called `ElektraError`, which encapsulates all information neccessary for the developer to handle runtime-errors appropriately in the application.

Functions that can produce errors accept an ElektraError pointer as parameter, for example:

```c
Elektra * elektraOpen (const char * application, ElektraError ** error);
```

In most cases you'll want to set the error variable to NULL before passing it to the function. You can do this either by declaring and initializing a new variable with `ElektraError * error = NULL` or by reusing an already existing error variable by resetting it with `elektraErrorReset (&error)`. 

Notice, that you should always check if an error occurred by comparing it to NULL after the function call. 

If an error happened, it is often useful to show an error message to the user. A description what went wrong is provided in the ElektraError struct and can be accessed by `elektraErrorDescription (error)`. A complete list of the provided accessors for error-details can be found in [elektra_error.h](/libelektra/libs/highlevel/elektra_error.h).

To avoid leakage of memory, you have to call `elektraErrorReset (&error)` a soon as you are finished resolving the error:

```c
ElektraError * error = NULL;

// Call a function and pass the error variable as an argument.
// ...

if (error != NULL)
{

  // An error occured, do something about it.
  // ...
  
  elektraErrorReset (&error);
}
```

## Reading and writing values

### Key names
When using `KDB` and `KeySet` directly you would have to specify the whole key name to access a value. In the high-level API you do not have to do this everytime you access a value. Instead, you pass a parent key to `elektraOpen` and use getters and setters which get passed in only the part below that key in the KDB. For example, if you call `elektraOpen` with `"/sw/org/myapp/#0/current"`, you can access your applications configuration value for the key `"/sw/org/myapp/#0/current/message"` with the provided getters and setters by passing them only `"message"` as the name for the configuration value.

### Read values from the KDB
A typical application will want to read some configuration values at start. This should be made as easy as possibible for the developer. Reading configuration data in most cases is not part of the business logic of the application and therefore should not "pollute" the applications source code with cumbersome setup and file-parsing code. This is exactly where Elektra comes in handy, because you can leave all the configuration file handling and parsing to the underlying layers of Elektra and just use the high-level API to access the desired data. Reading values from KDB can be done with elektra-getter functions that follow a simple naming scheme: 

`elektraGet` + the type of the value you want to read.

As an example here is the signature of the function to get a string value:

```c
const char * elektraGetString (Elektra * elektra, const char * name);
```

Sometimes you'll want to access arrays as well. You can access single elements of an array using the provided array-getters following again a simple naming scheme: 

`elektraGet` + the type of the value you want to read + `ArrayElement`.

As an example here is the signature of the function to get a string value from an array:

```c
const char * elektraGetStringArrayElement (Elektra * elektra, const char * name, size_t index);
```

To get the size of the array you would like to access you can use this function:

```c
size_t elektraArraySize (Elektra * elektra, const char * name);
```

For some background information on arrays in Elektra see the [Application Integration](/libelektra/doc/tutorials/application-integration.md) document.

Notice that both the getters for primitive types and the getters for array types do not accept error parameters. The library relies on that you are running a correct Elektra setup. If the configuration is well specified, no runtime errors can occur when reading a value. Therefore the getters do not accept an error variable as argument. If there is however a severe internal error, or you try to access a key which you have not specified correctly, then the library will call `exit(EXIT_FAILURE)` to prevent data inconsistencies or exceptions further down in your application.

You can find the complete list of the available functions for all supported value types in [elektra.h](/libelektra/libs/highlevel/elektra.h)

### Writing values to the KDB

Sometimes, after having read a value from the KDB, you will want to write back a modified value. As descibed in [Read values from the KDB](#read-values-from-the-kdb) we follow a naming scheme for getters. The high-level API provides setters folling an analogous naming scheme as well. For example, to write back a modified string, you can call this function:

```c
void elektraSetString (Elektra * elektra, const char * name, const char * value);
```

The counterpart for array-gettes again follows the same naming scheme:

```c
void elektraSetStringArrayElement (Elektra * elektra, const char * name, const char * value, size_t index, ElektraError ** error);
```

Be sure not to access indexes outside of the arrays bounds. The same rules a described in [Read values from the KDB](#read-values-from-the-kdb) apply here, meaning, that you are responsible for providing a complete and correct specification (see [Application Integration](/libelektra/doc/tutorials/application-integration.md)). If you try to access a key that you have not specified, the library will call `exit(EXIT_FAILURE)`.

## Example
```c
#include <stdio.h>
#include <elektra.h>

int main ()
{
  ElektraError * error = NULL;
  Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", &error);

  if (error != NULL) 
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
