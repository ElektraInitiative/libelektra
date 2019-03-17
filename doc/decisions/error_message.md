# Error message & handling concept

## Problem

The current error concept has disadvantages in following regards:

* Too verbose error message
* A lot of redundant errors
* Hard to manage specification file
* No senseful way for application developers to use error codes from elektra

This concept should improve useability for end users and developers.

## Constraints

* Error numbers must stay because they are more reliable to check against than strings

## Assumptions

## Considered Alternatives

* Removing the specification file without requiring error numbers
* Possible variations on what message should be displayed
* Adding the key of the occurred error to the API which permits reading information from
additional metadata such as an error message provided by a specification author. 
Reason against: The description of the key should already provide such information.
Doing it in an extra key would imply redundant information.

## Decision

The error message has the current format:

```
The command kdb set failed while accessing the key database with the info:
Sorry, the error (#121) occurred ;(
Description: validation failed
Reason: Validation of key "<key>" with string "<value>" failed.
Ingroup: plugin
Module: enum
At: ....../src/plugins/enum/enum.c:218
Mountpoint: <parentKey>
Configfile: ...../<file>.25676:1549919217.284067.tmp
```

The new default error message will look like this:
```
Sorry, plugin <PLUGIN> issued error #<NR>:
Validation of key "<key>" with string "<value>" failed.
```
The error #<NR> will be the color red while <PLUGIN> will be the color blue.

Optionally a third line indicating a solution can be added. Eg. for a permission related error there would be a third line:
```
Possible Solution: Retry the command as sudo (sudo !!)
```

To avoid losing information, the user can use the command line argument `-v` (verbose) to show
`Mountpoint`, `Configfile` in addition to the current error message.
Furthermore a developer can use the command line argument `-d` (debug) 
to show `At` for debugging purposes.

Warnings will be removed from the specification file as they are not needed there.

All "fatal" errors will be converted to "errors" as the distinguishment is not relevant.

Unused marked errors will be removed from the specification.

The remaining ~130 errors will be categorizes into logical groups with subgroups.
Each group and subgroup will receive a range of numbers such as in the HTTP protocol.

* Permanent errors (1-500)
    * Resource (1-50)
    * Parsing (51 - 100)
    * Installation (101 - 150)
    * Logical (151 - 200)
* Temporary (501 - 1000)
    * Conflict (501 - 600)
    * Timeout (601 - 701)
* Validation (1001 - 1500)
    * Syntactic (1001 - 1100)
    * Semantic (1101 - 1200)

## Rationale

The new error message is much more succinct which gives end users more relevant information.
Furthermore the solution approach still holds all necessary information if requested by users.

A new metakey `error/solution` will be implemented and attached to the parentkey via the new API.
`Ingroup`, `Description` and `Module` will be removed from the error message as they provide no useful
information at all.

The grouping of errors will allow developers to filter for specific as well as more general errors to correctly
react to them programmatically. Even though there are currently just 8 categories they will reserver up to 1500 error numbers.
This will permit additional subgrouping of errors in case it might be needed in the future. Imagine the case where
"Recourse" errors is too general because developers saw a need for splitting the errors in "permission" and "existence" errors.
They can simply take the current 1-50 numbers and make "permission" range from 1-25 and "existence" to 26-50. This will also allow
backwards compatibility by applications just checking for recourse errors in general.
Splitting/merging/rearranging any category should only be done by a decision (such as this file here) because elektra developers
should not be able to generate a new category as they wish because it would lead to the same proliferation of errors as we have now.

Warnings will be removed from the specification file. Any current warning will use the function
```
elektraAddWarning(Key * parentKey, const char * message)
```
Note that no error number is present anymore as it is not needed.

The API for the errors will be extended as following:
```
// Already present
ELEKTRA_SET_ERROR (nr, parentKey, message);
ELEKTRA_SET_ERRORF(nr, parentKey, message, ...);

// Extended API
ELEKTRA_SET_ERROR_WITH_SOLUTION(nr, parentKey, solution, message)
ELEKTRA_SET_ERROR_WITH_SOLUTIONF(nr, parentKey, solution, message, ...)
```

The vararg will affect the message and not the solution such as before.


## Implications

The specification file will stay but should be untouched in most of the cases in the future. Also the C++ code generation
file which uses the specification will stay as it is easier to change categories. The elektra warning part will be removed though.

Current errors will be migrated. The migration of each and every error can be seen here: [google docs](https://docs.google.com/spreadsheets/d/1-vXNZ7pN9wlMFByIMLbFt_HieyJpop0UyksbgAvmGK4/edit?usp=sharing).

## Related decisions


## Notes


