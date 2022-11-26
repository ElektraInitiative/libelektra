# Errors without `errorKey`

## Problem

We often use a `Key * errorKey` argument to return error information via metadata (see related decisions).
In `libelektra-core`, however, that would be inappropriate and sometimes even impossible (e.g., `keyNew`).

Therefore, we need a different concept for errors, when an `errorKey` cannot be used.

## Constraints

- Using special return values as errors may be limited.
  For example, only `NULL` is an invalid pointer and sometimes even that is a legitimate non-error result.
- It should be easy to detect error cases and distinguish between errors, once an error is detected.
- It should be hard to miss the fact that errors are possible.
  Note, however, this does not mean checking for an error should be enforced.
  There are many cases, where the caller knows an error is impossible, because of the previous code path.
  The requirement is analogous to the `Key * errorKey` argument.
  The presence of the argument makes it obvious that errors _can_ happen, but you can easily omit the error check when you want to.
- Adding error indications should not make the API less convenient to use.

## Assumptions

- In many cases, it is possible to avoid error cases through the preceding code path.
- When an error occurred, the kind error can be distinguished after the fact in many cases.
- Callers normally don't care about the exact error, unless they can do something about it.
  This could be:
  - reporting the error to a user
  - switching to a different code path
  - (during debugging) changing the code
- Functions can be written in a such a way that they detect and report all errors without permanent effects.

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

### Pragmatic solution

The pragmatic solution here is to simply assume "everything will be fine".

Many functions only return errors for simple precondition violations, like passing a `NULL` pointer or otherwise invalid argument.
Especially for `libelektra-core` (the only place where `Key * errorKey` truly cannot be used), all functions must be side effect free.
Therefore, the only possible cause of errors are invalid arguments.

If we now assume the function is written such that it fails before it changes any argument's data, then we can infer that only a single error indicating return value is needed.
Because the arguments are unchanged and the only possible cause of errors, we can distinguish between errors after the fact.

As stated, many functions' only error cases are precondition violations.
If the preconditions are simple enough, they can be checked beforehand if it is not clear whether the precondition holds.

Take for example some hypothetical `keyValue` function, in a world where we have a `key->value` container for COW that should always be present.
Looking just at the error checks, without even considering the function signature:

```c
if (key == NULL) {
// error 1: precondition violation
}

if (key->value == NULL) {
// error 2: invalid Key, COW container missing
}

// success: return value
return key->value->data;
```

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

  assert(key->value != NULL);
  if (key->value == NULL) {
    return NULL;
  }

  return key->value->data;
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
Instead, we return `NULL`, so the caller can do something and doesn't have to deal with the internal bug.

This overlapping of error and success values has especially positive effects, when we talk about boolean functions:

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
Then the caller can just check after the error occured:

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

**Suggestion:** Go with "Pragmatic solution"

## Rationale

Under the current assumptions this is the obvious solution.
Every other solution borders on over-engineering.

## Implications

- Some functions (e.g., `keyIsBelow`) should have simplified return values

## Related Decisions

- [Error Code Implementation](../5_implemented/error_code_implementation.md)
- [Error codes](../5_implemented/error_codes.md)
- [Error message format](../5_implemented/error_message_format.md)

## Notes
