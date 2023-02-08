# High-level API Help Message

This decision _does not_ assume code-generation is used.
For the case of code-generation see the [Notes](#notes) section.

## Problem

We want to allow to print the help message no matter what errors happened in `kdbOpen` or `kdbGet`.

## Constraints

- `elektraOpen` should not return a broken `Elektra` instance.
- The help message can only be printed, if `elektraOpen` returns an `Elektra` instance and no `ElektraError`.

## Assumptions

- We assume that the application in question was correctly installed.
- We assume `gopts` was mounted. This is not the default right now, but the code-generator template `highlevel` contains code that will mount `gopts`, if it is missing.
- We assume the application was called in _help mode_, i.e. with `--help`.
  Otherwise printing the help message is not possible, anyway.

## Considered Alternatives

- Ignore all errors (in help mode):
  Not a feasible solution, because there may have been problems when reading the storage file and therefore, the help message may be broken or incomplete.
- Ignore all errors (in help mode), which occurred after the `gopts` plugin ran:
  Complicated to implement (we need to know about plugin order, etc.).
  Not actually necessary (see [Rationale](#rationale)).

## Decision

Ignore missing `require`d keys (in help mode), but fail for every other error.

## Rationale

Required keys **must** be provided by the user/admin and cannot come from another source (Elektra, app developer, etc.).
Therefore they will be missing until the user makes changes to the KDB. Before that, no other error should occur (we assumed a correct installation).
If a user runs `app` for the first time and receives an error about a missing required key, they will:

1. know what to do and add the key, thereby fixing the problem.
2. try `app -h` and see that it doesn't show a help message. They will probably continue with 3.
3. try `app --help` to find out more. The help message may or may not contain useful information. If not they may try 4.
4. read some other documentation to find out more. Ideally this leads them to 1.

In any case after this the user definitely know how to interact with the KDB.
Since we assumed that there won't be any errors before the KDB was changed, we can assume that the user caused other errors by changing the KDB.

## Notes

If code-generation is used, the situation is a little different.
If the parameter `embedHelpFallback` is set to `1`, a fallback help message will be created from the specification originally passed to the code-generator and embedded into the application.
The parameter also changes, how help mode is detected and ultimately allows the help message function (`printHelpMessage` by default) to always print a help message.
Although it may not reflect changes, the user made to the specification.
