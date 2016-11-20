A full java binding using JNA.

## Usage ##

For using the binding as standalone (to write applications using Elektra),
make sure that CLASSPATH includes jna.jar and libelektra.jar (or this directory
which contains the elektra subdirectory), e.g.:

    export CLASSPATH="/usr/share/java/libelektra.jar:/usr/share/java/jna.jar"
    export CLASSPATH="~e/src/bindings/jna:/usr/share/java/jna.jar"

to set it permanently for your user, you can use:

    kdb set user/env/override/CLASSPATH "/usr/share/java/libelektra.jar:/usr/share/java/jna.jar"

then you can compile and run [HelloElektra](HelloElektra.java):

    javac HelloElektra.java && java HelloElektra

For plugin development, see [plugins](elektra/plugin)
and also [here](/src/plugins/jni) for more information.

## Testing ##

Unit tests can be run after importing jUnit, JNA and the elektra java library into a project (eclipse, netbeans, intelliJ, ...).

Tested library versions are:
	JNA: 4.1
	jUnit: 4.11 [jUnit 3 is not supported]
	Both libraries should work on version 4.0 too, though.

It should also be possible to run the tests by command line:
	1) Compile the library and tests (run in root directory; make sure junit4 and jna are installed and/or path is correct):
		javac -cp src/elektra:/usr/share/java/jna.jar:/usr/share/java/junit4.jar src/elektra/*.java src/elektra/plugin/*.java src/elektra/test/*.java
	2) Run all jUnit tests:
		java -cp src:/usr/share/java/jna.jar:/usr/share/java/junit4.jar org.junit.runner.JUnitCore elektra.test.AllTests
	2.1) Or run all tests on their own:
		java -cp src:/usr/share/java/jna.jar:/usr/share/java/junit4.jar org.junit.runner.JUnitCore elektra.test.KeyTest
		java -cp src:/usr/share/java/jna.jar:/usr/share/java/junit4.jar org.junit.runner.JUnitCore elektra.test.KeySetTest


## Limitations ##

- no binary keys
- no full names
- error/warning fixed to a special number, no generator for any
  error/warning
- no java iterator for metadata
- no simple way to print warning/errors
- no junit tests, so its likely that there are some bugs
