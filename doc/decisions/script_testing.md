# Script Testing

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
