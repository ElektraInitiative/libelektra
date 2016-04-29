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