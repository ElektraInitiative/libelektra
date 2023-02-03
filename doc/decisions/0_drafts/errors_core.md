# Errors without `errorKey`

## Problem

We often use a `Key * errorKey` argument to return error information via metadata.
In `libelektra-core`, however, that would be inappropriate and sometimes even impossible (e.g., `keyNew`).

Not using a `Key * errorKey` argument can be limiting, however.
Depending on the solution, there can be limited error values, i.e., it may not be possible to use a different error value for every possible error case.
A prime example of this, a function that returns a pointer and wants to indicate an error via that pointer, only has to option to return `NULL`.
So if there is more than one possible error, the error cases have to be distinguished another way.

Therefore, we need a general concept for errors, which we use when an `errorKey` cannot be used.

## Constraints

- Using special return values as errors sometimes is limited.
  For example, only `NULL` is an invalid pointer and sometimes even that is a legitimate non-error result.
- It should be easy to detect error cases.
- It must be possible to distinguish between errors, once an error is detected.
- It should be hard to miss the fact that errors are possible.
  Note, however, this does not mean checking for an error should be enforced.
  There are many cases, where the caller knows an error is impossible, because of the previous code path.
  The requirement is analogous to the `Key * errorKey` argument.
  The presence of the argument makes it obvious that errors _can_ happen, but you can easily omit the error check when you want to.
- Adding error indications should not make the API less convenient to use.
- It should be [hard to use the API the wrong way](doc/DESIGN.md).

## Assumptions

- In many cases, it is possible to avoid error cases through the preceding code path.
  For example, a ubiquitous case is that functions return an error, if they receive a `NULL` pointer.
  Often this case can be excluded by static analysis, i.e., because of the preceding code it is impossible that the pointer is `NULL`.

  ```c
  int foo (KeySet * ks, Key * key) {
    if (key == NULL) {
      return -1;
    }

    // [...] could be lots of code, but the code doesn't change `Key * key`

    const Key * found1 = ksLookupByName (ks, "/foo", 0);
    if (found1 != NULL) {
      // no need to worry about key == NULL, because we checked earlier
      // also no need to worry about ks == NULL, because the ksLookupByName would have returned NULL
      const Key * found2 = ksLookup (ks, key, 0);
    }
  }
  ```

- It is not always necessary to report separate error codes, for _every_ error case.
  In many cases, the caller can distinguish after the fact, e.g., `keySetName` fails when the given name is invalid and when the given `Key` has a read-only name.
  This can be distinguished after the fact, by checking whether the `Key` has a read-only name with `keyIsLocked`.

- Callers normally don't care about the exact error, unless they can do something about it.
  This could be:

  - reporting the error to a user
  - switching to a different code path
  - (during debugging) changing the code

- Functions can be written in a such a way that they detect and report all errors without permanent effects.

- The functions that cannot use a `Key * errorKey`, are side-effect-free.
  Therefore, the only possible cause of errors are invalid arguments.

## Considered Alternatives

### Extra argument

All functions that report errors take a `int * error` argument to which errors can be written.

This makes the API much more convoluted to use.

A benefit would be the return value remains fully usable.

### Thread Local Storage

Use a field in thread local storage (TLS) to store errors similar to `errno`.

This leaves the API design entirely open.

TLS was only standardized in C11, while Elektra currently only requires C99.

This has all the same issues as `errno`: needs to be cleared to detect errors, can only store one error (per thread), etc.

### Limit to single error value

Always limiting a function to a single error code can be very limiting and in some cases and makes it very hard to find out what the exact problem was.
Reducing a function to a single error value, makes sense if the errors are easy to distinguish.
But for more complex preconditions, this can be annoying and inefficient to use.
For example, a function `foo` that takes two keynames, both of which must be valid.
To find out which of the names was invalid, the caller would have to call something like `elektraCheckName(name)` on at least one name.
That's a non-trivial check that is not cheap in terms of runtime.
More importantly, it is a check that `foo` already did, and the result could just be propagated to the caller.

### Redesign API

Similar to the option above, we limit all functions to a single error value.
But we also redesign the API, so that every API function only has maximal one error case (apart from failed allocation and `NULL` pointers as arguments).

- `nameNew(name) -> name_object` only has the error case of invalid name
- `keyNew(name_object)` won't have error cases anymore
- `keySetName(name_object)` only has the error case of read-only names

The advantage is that an API like this is hard to be used incorrectly.
You can only call `keySetName` when you already have a valid key name, so there is only a single error case for `keySetName`.
If `keySetName` fails, we always know exactly what was wrong; the name was read-only.

However, always partitioning an API like this could create very annoying to use APIs.
It could also have a negative impact on memory use.
To make an API like this work properly, all the intermediate objects that allow us to have functions with a single error case, must be opaque structs.
That means they must be heap-allocated as separate objects.

### Require caller to check preconditions

If caller are interested in the specific errors, they need to do extra checks, e.g., before calling `keySetName`, they call `elektraCheckName(name)` or `keyIsLocked`.

This is not so intuitive and is against our principle to make it hard to use the API wrong.

It's also unclear, what the function would do, if the caller did not check the preconditions.

### Pragmatic solution

We assume the functions in question are side-effect-free and the only possible cause of errors are invalid arguments.
We also assume that functions _can_ always be written such that they fail before any permanent changes to the arguments are made.

If we assume that _all_ functions are indeed written that way, then we can conclude, that in theory a single error indicating return value is always enough.
The reason is simply, that under the previous assumptions the caller can always make further checks on the arguments to find the exact cause of the error.

However, because sometimes checking arguments can be complicated and expensive (in terms of runtime), we _do not_ reduce all functions to a single error indicating value.
Instead, we take pragmatic approach and try to give more detailed errors, when appropriate and while maintaining a sensible API design.

Below we explore a few different types of functions (in terms of their possible errors) and explore, how they can report errors.
In addition to these examples, some of the other options could be combined with this one.
Where appropriate, the API can be redesigned and partitioned into functions with a single error case.
In other cases, using an extra argument for errors, or using an output pointer argument, might be an option.

#### Simple preconditions

Many functions' only error cases are precondition violations.
If the preconditions are simple enough, they can be checked beforehand if it is not clear whether the precondition holds.

Take for example the `keyValue` function (after PR #4691).
Looking just at some of the error checks, it looks a bit like this:

```c
if (key == NULL) {
// error 1: precondition violation
}

if (key->keyData == NULL) {
// error 2: invalid Key, COW container missing
}

// success: return value
return key->keyData->data.v;
```

> **Note:** The above doesn't exactly match the real world.
> It is a hypothetical example, and we assume that `key->keyData` should always be set by `keyNew` and when changing the value.

If we want separate error indications for both error cases and a nice API we immediately hit a problem.
The obvious return type is `const void *`, because that's the type of the key value (and we want it const).
But that's a pointer, so only `NULL` could be used for errors.
The issue is, `NULL` may be a valid key value.
So what do we do?

Essentially, we do nothing, "everything is fine":

```c
const void * keyValue (Key * key) {
  if (key == NULL) {
    return NULL;
  }

  assert(key->keyData != NULL);
  if (key->keyData == NULL) {
    return NULL;
  }

  return key->keyData->data.v;
}
```

We return `NULL` for both error cases and even let the errors overlap with a legitimate success value.
In the first case, it's fine for multiple reasons:

- Many callers won't care, if the value inside the key is `NULL` or the key itself is `NULL`.
  For example, `keyValue (ksLookup (ks, name)) == NULL` is probably fine either way.
- If the caller does care, they can easily check `if (key == NULL)` before _or_ after the `keyValue` call.

The second case has a different reason.
The error is not the caller's fault.
It is an internal issue the caller can do nothing about, `key->value` should always be there.
That's why we add an `assert` before the check.
This will trigger in a debug build, where the internal error can be fixed.
For release builds, the `assert` won't be there.
Instead, we return `NULL`, so we aren't forced to `abort()` the whole process and the caller may even be able to gracefully recover from the internal bug.

#### Boolean functions

The overlapping of error and success values has especially positive effects, when we talk about boolean functions:

```c
// returns 1 if key is "foo", 0 if key is not "foo", and -1 if key == NULL
int keyIsFooInt (Key * key);

// returns true if key is "foo", false otherwise (including if key == NULL)
bool keyIsFooBool (Key * key);
```

The `keyIsFooInt` function is very annoying to use, but `keyIsFooBool` is much easier except for the rare case where you need to distinguish a `NULL` key:

```c
// "foo" and not NULL
if (keyIsFooInt (key) == 1)
if (key != NULL && keyIsFooBool (key))

// not "foo" or NULL
if (keyIsFooInt (key) != 1)
if (key == NULL || !keyIsFooBool (key))

// not "foo" and not NULL
if (keyIsFooInt (key) == 0)
if (key != NULL && !keyIsFooBool (key))

// "foo" or NULL
if (keyIsFooInt (key) != 0)
if (key == NULL || keyIsFooBool (key))
```

Especially, if you don't expect the `-1` return value (because `int` is also often boolean in C) bugs arise:

```c
// Expected: key is "foo"
// Actually: key is "foo" or key is NULL
if (keyIsFooInt (key))
// Equivalent: if (keyIsFooInt (key) != 0)

// Expected: key is not "foo"
// Actually: key is not "foo" and key is not NULL
if (!keyIsFooInt (key))
// Equivalent: if (keyIsFooInt (key) == 0)
```

#### Complex preconditions

The above cases assume very simple preconditions.
But sometimes the precondition checks are non-trivial.
Take for example searching for `Key` in a `KeySet` with an escaped name:

```c
Key * ksLookupByName (KeySet * ks, const char * escapedName)
```

Because we return a pointer, the only error option is `NULL`.
But that could mean multiple things:

- No such key found
- Error: `ks == NULL`
- Error: `escapedName == NULL`
- Error: `escapedName` not a valid escaped name

If the caller wants to know which of these was the case, they have to do additional checks.
For the first three cases that's easy, that's just two simple `NULL`-checks.
But detecting the last case is non-trivial.
Validating escaped names means checking a lot of rules.

However, there is once again a very simple pragmatic solution ("everything is fine").
We simply provide a public API function that allows checking, whether a `const char *` is a valid escaped name.
Then the caller can just check after the error occurred:

```c
// assume we already know ks != NULL && escapedName != NULL
Key * found = ksLookupByName (ks, escapedName);
if (found == NULL)
{
  if (!elektraIsValidEscapedName (escapedName)) {
    // invalid name
  } else {
    // key not found
  }
}
```

## Decision

Go with "Pragmatic solution".
We don't define any strict rules for the entire API.
Instead, we define a few basic guidelines.

### Guidelines

These guidelines shall be applied to each function of the public API separately to decide how the function treats errors.

#### Boolean functions

If a function is conceptually boolean, it should return a `bool`.
This means choosing `true` or `false` as the fallback value used in case of errors.

If choosing `true` or `false` is not feasible, redesigning the function may be necessary.
The function may be doing to many checks in one go and splitting it into multiple functions could make sense.

If splitting is also not possible, an `int` or `enum` should be returned.
The name of such a function must make clear that it is not actually boolean (avoid `is` and `has`, prefer `check`) to avoid accidental use in an `if`.

#### Check preconditions

Functions must check all preconditions.
A (bug-free) function should never segfault, abort the process or otherwise cause unexpected behavior on the caller side.
All conditions that could cause such behavior must be listed as preconditions and checked by the function as a safety measure.

#### `NULL` arguments

A `NULL` pointer argument should never be treated as a separate error case (unless it is the only error case).
If a `NULL` pointer constitutes an invalid argument, the function should return an existing error (might be used for other precondition checks) or a default fallback value.

#### Simple preconditions

Simple preconditions are those that can be checked easily by the caller and which are not computationally expensive to check.
All such preconditions should use the same error code, which may even be shared with other error cases.

#### Complex preconditions

Complex preconditions which

- cannot (easily) be checked by the caller (e.g., because there is no public API for it) OR
- are computationally expensive to check (i.e., duplicating the check would be bad)

should always use separate error codes for each error case.
The error code may not be shared with other complex preconditions, but can be shared with simple preconditions or `NULL` arguments, since those are easy to distinguish for the caller.

#### Restructure API

To avoid complex preconditions and the need for multiple error codes, the API can be restructured.
This should, however, be done with caution.
The API should still remain ergonomic to use and there should not be extra memory allocations creating opaque objects just to guard against possible errors.

#### Error arguments

If a function needs multiple error codes, return them as an `int` or enum value.
Do not use an error argument (like `Key * error`).
Instead, use an output pointer argument (e.g., `Foo * output`) for the data produced by the function.
This makes it much more obvious to the caller that errors can occur.

An exception is e.g., the high-level API, which maps the error concept of a `Key * error` onto a custom `ElektraError` type.
This is allowed, because it has the same expressiveness and use case as a `Key * error`.

#### Internal errors

Internal errors that are never the fault of the caller and always caused by a bug in Elektra, should be prevented with an `assert`.

#### Secondary return values

In the success case, only return data the caller explicitly asked for by calling the function.
Do not use secondary information as the return value.
If there is secondary information, the caller may care about, use an (optional) output pointer argument.

For example, `ksAppendKey` should not return the new size of the `KeySet`.
The caller did not ask for the size, they asked for a key to be inserted.
Apart from potential errors, the possible results the caller cares about are only:

- success: key newly added
- success: key existed, now replaced

Therefore, the return value should only encode this information, e.g., via an `int`.

Another example, could be a hypothetical `elektraEscapedNameIsValid` function.
Here the caller wants to know whether the given string is a valid escaped name.
They may still care about the canonical size of the escaped name or the offset of the first invalid character.
For those things, optional output pointer arguments should be used.
The function signature could look like this:

```c
/**
 * @param outSize If not `NULL` and @p name is valid, `*outSize` will be set to the canonical size of @p name
 * @param outErrorIndex If not `NULL` and @p name is not valid, `*outErrorIndex` will be set to the index of the first invalid character in @p name
 *
 * @returns `true` if @p name is valid escaped name and `false` otherwise (including `name == NULL`)
 */
bool elektraEscapedNameIsValid (const char * name, size_t * outSize, size_t * outErrorIndex);
```

## Rationale

- Under the current assumptions this is the obvious solution.
  Almost every other solution borders on over-engineering.
- Defining strict rules for error handling, could in future leave us cornered and forced to create bad API or introduce exceptions to the rules.
- Every function has its own use case, so it makes sense that errors are also treated differently on a case by case basis.

## Implications

- We will need to examine the current API and decide which functions need to be redesigned.

## Related Decisions

- [Error Code Implementation](../5_implemented/error_code_implementation.md)
- [Error codes](../5_implemented/error_codes.md)
- [Error message format](../5_implemented/error_message_format.md)

## Notes
