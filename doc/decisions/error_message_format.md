# Error message & handling concept

## Problem

Too verbose error message
Currently for every error, 9 lines are shown in which most of them are not relevant to end users/administrators. One goal
is to reduce the verbosity of such messages and let users/administrators see only information they need.

## Constraints

- Supporting multiple programming languages
- Plugin System
- Error Code should be preserved

## Assumptions

## Considered Alternatives

Possible variations on what message should be displayed,
eg. to keep the mountpoint information or on how wordings should be (with or without
"Sorry, ...", coloring of certain parts of a message, etc.)

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

The new default message will look like this:

```
Sorry, plugin <PLUGIN> issued [error|warning] code <NR>:
Validation of key "<key>" with string "<value>" failed.
```

The <NR> will be the color red in case of an error or yellow in case of a warning
while <PLUGIN> will be the color blue.

Optionally a third line indicating a solution can be added. Eg. for a permission related error there would be a third line:

```
Possible Solution: Retry the command as sudo (sudo !!)
```

To avoid losing information, the user can use the command line argument `-v` (verbose) to show
`Mountpoint`, `Configfile` in addition to the current error message.
Furthermore a developer can use the command line argument `-d` (debug)
to show `At` for debugging purposes.

## Rationale

The new error message is much more succinct which gives end users more relevant information.
Furthermore the solution approach still holds all necessary information if requested by users.

`Ingroup`, `Description` and `Module` will be removed from the error message as they provide no useful
information at all.

## Implications

## Related Decisions

## Notes
