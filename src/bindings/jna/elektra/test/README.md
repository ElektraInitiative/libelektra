Unit tests can be run after importing jUnit, JNA and the elektra java library into a project (eclipse, netbeans, intelliJ, ...).

It should also be possible to run the tests by command line:
1) Compile the library and tests (run in "elektra" directory):
	javac -cp .:/path/to/jna.jar:/path/to/junit.jar *.java plugin/*.java test/*.java
2) Run all jUnit tests:
	java -cp .:/path/to/jna.jar:/path/to/junit.jar org.junit.runner.JUnitCore elektra.test.AllTests
2.1) Or run all tests on their own:
	java -cp .:/path/to/jna.jar:/path/to/junit.jar org.junit.runner.JUnitCore elektra.test.KeyTest
	java -cp .:/path/to/jna.jar:/path/to/junit.jar org.junit.runner.JUnitCore elektra.test.KeySetTest