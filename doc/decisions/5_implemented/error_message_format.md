# Error message format

## Problem

Too verbose error message.
Currently for every error, 9 lines are shown in which most of them are not relevant to end users/administrators.
One goal is to reduce the verbosity of such messages and let users/administrators see only information they need.

## Constraints

- Supporting multiple programming languages
- Plugin System
- Error Code should be preserved

## Assumptions

## Considered Alternatives

Possible variations on what message should be displayed, e.g., to keep the mountpoint information or on how wordings should be (with or without "Sorry, ...", coloring of certain parts of a message, etc.)

Examples would be to

- Leave out the "Sorry" in the error message or leave the introduction sentence completely
- Drop `At`, `Mountpoint`, `Configfile`, `Module`. This information though yields useful information
  or was even added as a request
- Show mountpoint, configfile, module, etc in beneath the general introduction message. Eg.
  `The command kdb set failed while accessing the key database on mountpoint (...) with the info`
- Incorporating the description in another ways:
  `Reason: Validation of key "<key>" with string "<value>" failed. (validation failed)`
- Use one command line option to show all additional info which gets hidden per default from now on instead of two
- Color the main message differently compared to the general introduction message
- Do not color messages as it might confuse users with overwhelming many colors
- Do not print out the error code. It is useful though for googling

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
Sorry, module `MODULE` issued [error|warning] `NR`:
`ERROR_CODE_DESCRIPTION`: Validation of key "<key>" with string "<value>" failed.
```

The `NR` will be the color red in case of an error or yellow in case of a warning while `MODULE` will be the color blue.

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

## Implications

`Description` will be incorporated into `Reason` whereas the `Module` will be incorporated into
the general sentence starting the error message.

## Related Decisions

- [Error Codes](error_codes.md)
  Shows how the new error codes are meant to be

## Notes
