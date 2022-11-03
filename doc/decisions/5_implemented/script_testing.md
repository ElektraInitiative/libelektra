# Script Testing

## Problem

Writing portable shell code for testing command-line tools is difficult.

## Constraints

- Should be able to record input/output/exit codes of command-line tools
- Should be aware of configuration settings (KDB), for example, restore it on changes

## Assumptions

None.

## Considered Alternatives

- pythonpaste

  - Pros:
    - easy to work with
  - Cons:
    - can only trace a single directory (would not work with /etc + ~)
    - extra dependency not in any distro

- robotframework

  - Cons:
    - additional (fat) dependency
    - integration with cmake?
    - does not allow one to capture stdout, stderr + return code

- expect
  - Pros:
    - interactive testing (e.g. for kdb mount)
  - Cons:
    - quite long for simple things (e.g. check /bin/true needs 4 lines)
    - new syntax for Elektra (TCL)
    - additional dependency

## Decision

Develop [shell recorder](/tests/shell/shell_recorder) and [tutorial wrapper](/tests/shell/shell_recorder/tutorial_wrapper).

## Rationale

## Implications

## Related Decisions

## Notes

- 12.11.2017: pythonpaste not maintained anymore, site is offline
