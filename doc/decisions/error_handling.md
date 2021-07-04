# Error Handling

## Problem
Currently, a lot of functions return the error value, regardless of error. These poses some problems, for instance authors of bindings, where it is hard to tell which error exactly occured. A big improvement to this would be returning different error codes for different types of errors, in order to be able to provide better information in cases of errors. Some functions also make use of an `errorKey`. While this seems like a decent approach, to provide additional informations in cases of error, it is currently very badly documented. It is not really transparent for the users of the function, what kind of information is included in the `errorKey` and even which messages could possibly be returned.

## Constraints
We also have to consider that many parts of the API very constrained in what values they can use for error codes. If a function returns a pointer the only possible error value by definition is NULL.

## Assumptions

## Considered Alternatives
- Change return types for many functions across the API

## Decision
- Add documentation for errorKey to Doxygen
- Add thread-local error handling for getting information about errors

## Rationale

## Implications

## Related Decisions

## Notes