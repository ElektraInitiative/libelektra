# Script Testing

## Issue

Writing portable shell code for testing commandline tools is difficult.

## Constraints

- Should be able to record input/output/exit codes of command-line tools
- Should be aware of configuration settings (KDB), for example, restore it on changes

## Assumptions

None.

## Considered Alternatives

* http://pythonpaste.org/scripttest/
 + easy to work with
 - can only trace a single directory (would not work with /etc + ~)
 - extra dependency not in any distro

* robotframework
 - additional (fat) dependency
 - integration with cmake?
 - does not allow one to capture stdout, stderr + return code

* expect
 + interactive testing (e.g. for kdb mount)
 - quite long for simple things (e.g. check /bin/true needs 4 lines)
 - new syntax for Elektra (TCL)
 - additional dependency


## Decision

Develop shell recorder and tutorial wrapper.

## Argument

## Implications

## Related decisions

## Notes
