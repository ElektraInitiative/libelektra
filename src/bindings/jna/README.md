A full java binding using JNA.

## Usage ##

### Command line ###

#### Linux ####
For using the binding as standalone (to write applications using Elektra),
make sure that CLASSPATH includes jna.jar and libelektra.jar (or this directory
which contains the libelektra4j subdirectory that corresponds to the libelektra.jar), e.g.:

    export CLASSPATH="/usr/share/java/libelektra.jar:/usr/share/java/jna.jar"
    export CLASSPATH="~e/src/bindings/jna:/usr/share/java/jna.jar"

to set it permanently for your user, you can use:

    kdb set user/env/override/CLASSPATH "/usr/share/java/libelektra.jar:/usr/share/java/jna.jar"

then you can compile and run [HelloElektra](HelloElektra.java):

    javac HelloElektra.java && java HelloElektra

Alternatively you can also specify the classpath directly, both during compilation and execution. Note that in this example, we have the elektra and jna libraries in different directories, as mac doesn't allow writing to /usr/share/java in never versions. Also note its important in that case to include this directory containing the compiled HelloElektra.class when executing it, otherwise it will not be found:

	javac -cp ./libelektra4j.jar:./libelektra4j/jna.jar HelloElektra.java
	java -cp .:./libelektra4j.jar:./libelektra4j/jna.jar HelloElektra

For plugin development, see [plugins](libelektra4j/plugin)
and also [here](/src/plugins/jni) for more information.

### Maven ###

In case you want to use maven as your build system, simply go into the libelektra4j subfolder and execute mvn install. This will install the plugin into your local maven repository. Afterwards you can include it as a dependency into your maven project, and given that libelektra is actually installed on your system you can use it by including the maven dependency:
	<groupId>org.libelektra</groupId>
	<artifactId>libelektra4j</artifactId>
	<version>0.0.1-SNAPSHOT</version>

## Testing ##

### Command Line ###

Unit tests can be run after importing jUnit, JNA and the libelektra4j java library into a project (eclipse, netbeans, intelliJ, ...). Alternatively, you can use maven to both build the project or just run the unit tests.

Tested library versions are:
	JNA: 4.2.2
	jUnit: 4.12 [jUnit 3 is not supported]
	hamcrest-core: 1.13 (required by newer jUnit versions)
	Both libraries should work on version 4.0 too, though.

It should also be possible to run the tests by command line:
	1) Compile the library and tests (run in root directory; make sure junit4 and jna are installed and/or path is correct). Execute the following commands inside the libelektra4j folder:
		mkdir ./target if it does not exist yet
		javac -cp <path to junit and jna and hamcrest*> -d ./target src/main/java/org/libelektra/*.java src/main/java/org/libelektra/plugin/*.java src/test/java/org/libelektra/*.java
		* so if you copied the jna.jar, junit.jar and hamcrest-core.jar directly to the jna directory, the correct path would be ./jna.jar:./junit.jar:./hamcrest-core.jar (separated by :), otherwise specify the appropriate locations.
		for linux users they are usually in /usr/share/java/jna.jar:/usr/share/java/junit4.jar
	2) Run all jUnit tests (please note that the -cp parameter now also has to include the target directory we created in the first step, where the compiled classfiles are):
		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.AllTests
	2.1) Or run all tests on their own:
		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.KeyTest
		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.KeySetTest

### Maven ###

simply go into the libelektra4j subfolder and execute mvn test to run just the tests.

## Limitations ##

- no binary keys
- error/warning fixed to a special number, no generator for any
  error/warning
- no java iterator for metadata
- no simple way to print warning/errors
