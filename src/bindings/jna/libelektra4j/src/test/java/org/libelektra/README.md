Unit tests can be run after importing jUnit, JNA and the libelektra4j java library into a project (eclipse, netbeans, intelliJ, ...). Alternatively, you can use maven to both build the project or just run the unit tests.

Tested library versions are:
	JNA: 4.2.2
	jUnit: 4.12 [jUnit 3 is not supported]
	hamcrest-core: 1.13 (required by newer jUnit versions)
	Both libraries should work on version 4.0 too, though.

It should also be possible to run the tests by command line:
	1) Compile the library and tests (run in root directory; make sure junit4 and jna are installed and/or path is correct):
		mkdir ./target if it does not exist yet
		javac -cp <path to junit and jna and hamcrest*> -d ./target src/main/java/org/libelektra/*.java src/main/java/org/libelektra/plugin/*.java src/test/java/org/libelektra/*.java
		* so if you copied the jna.jar, junit.jar and hamcrest-core.jar directly to the jna directory, the correct path would be ./jna.jar:./junit.jar:./hamcrest-core.jar (separated by :), otherwise specify the appropriate locations.
		for linux users they are usually in /usr/share/java/jna.jar:/usr/share/java/junit4.jar
	2) Run all jUnit tests (please note that the -cp parameter now also has to include the target directory we created in the first step, where the compiled classfiles are):
		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.AllTests
	2.1) Or run all tests on their own:
		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.KeyTest
		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.KeySetTest

Running the tests by command line using maven:
	1) simply execute mvn test, the build system takes care about the rest