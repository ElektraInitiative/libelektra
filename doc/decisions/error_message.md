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
additional metadata

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

Unused marked errors will be removed from the specification

The remaining ~130 errors will be categorizes as following:

* Permanent errors
    * Resource
    * Parsing
    * Installation
    * Logical
* Temporary
    * Conflict
    * Timeout
* Validation
    * Syntactic
    * Semantic

## Rationale

The new metakey `error/solution` will be implemented and attached to the parentkey via the new API.


## Implications


## Related decisions


## Notes


