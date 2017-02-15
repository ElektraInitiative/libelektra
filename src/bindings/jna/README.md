A full java binding using JNA. This binding requires elektra to be installed on 
the system to work.

## Usage ##

To use the bindings in a java project, we have to include the jar file 
libelektra-0.8.19.jar in the project. This jar is created upon build, if you 
enable the jna bindings. You can also use maven to take care about the 
dependencies.

Please note that the jni plugin is a different thing than the jna bindings. We 
can use the jni plugin to develop plugins for elektra itself, whereas the jna 
bindings allow to use elektra to access configuration in java projects. The jni 
plugin is *not* required for the jna bindings to work.

### Command line ###

#### Linux ####
For using the binding as standalone (to write applications using Elektra),
make sure that CLASSPATH includes jna.jar and libelektra.jar (or this directory
which contains the libelektra4j subdirectory that corresponds to the 
libelektra.jar), e.g.:

    export CLASSPATH="/usr/share/java/libelektra-0.8.19.jar:/usr/share/java/jna.jar"
    export CLASSPATH="~e/src/bindings/jna:/usr/share/java/jna.jar"

to set it permanently for your user, you can use:

    kdb set user/env/override/CLASSPATH "/usr/share/java/libelektra-0.8.19.jar:/usr/share/java/jna.jar"

then you can compile and run [HelloElektra](HelloElektra.java):

    javac HelloElektra.java && java HelloElektra

You can also specify the classpath directly, both during compilation and execution.
Also note its important in that case to include this directory containing the 
compiled HelloElektra.class when executing it, otherwise it will not find it:

	javac -cp /usr/share/java/libelektra4j-0.8.19.jar:/usr/share/java/jna.jar HelloElektra.java
	java -cp .:/usr/share/java/libelektra4j-0.8.19.jar:/usr/share/java/jna.jar HelloElektra

For plugin development, see [plugins](libelektra4j/plugin)
and also [here](/src/plugins/jni) for more information.

#### macOS ####

Using the bindings via the command line works the same way as in Linux. The 
difference is that in macOS the jar file gets generally installed to 
`/usr/local/share/java`.

### Maven ###

To use the jna bindings via maven, first you have to install the jna bindings 
to your local maven repository. When you have built elektra with the jna 
bindings included, they should have been automatically installed to 
´/usr/share/java/´ along with a pom file for the library. To install it to your
local maven repository from that location, execute the following command:

	mvn org.apache.maven.plugins:maven-install-plugin:2.5.2:install-file -Dfile=/usr/local/share/java/libelektra4j-0.8.19.jar -DpomFile=/usr/local/share/java/libelektra4j-0.8.19.pom.xml

Given that libelektra is actually installed on your system you can use it by 
including the following maven dependency in your project afterwards:

	<groupId>org.libelektra</groupId>
	<artifactId>libelektra4j</artifactId>
	<version>0.8.19</version>

## Testing ##

### Command Line ###

You can run unit tests after importing jUnit, JNA and the libelektra4j java 
library into a project (eclipse, netbeans, intelliJ, ...).

Tested library versions are:

- JNA: 4.2.2
- jUnit: 4.12 [jUnit 3 is not supported]
- hamcrest-core: 1.13 (required by newer jUnit versions)

Tested JDK versions are:

- Oracle JDK 1.8.0_112 on macOS 10.12 Sierra
- OpenJDK 1.8.0_121 on Arch Linux

Both libraries should work on version 4.0 too, though.

It should also be possible to run the tests by command line:

1. Compile the library and tests (run in root directory; make sure junit4 and 
	jna are installed and/or path is correct). Execute the following commands inside 
	the libelektra4j folder:

		mkdir ./target (if it does not exist yet)
		javac -cp <path to junit and jna and hamcrest*> -d ./target src/main/java/org/libelektra/*.java src/main/java/org/libelektra/plugin/*.java src/test/java/org/libelektra/*.java

	If you copied the jna.jar, junit.jar and hamcrest-core.jar directly to the 
	jna directory, the correct path would be `./jna.jar:./junit.jar:./hamcrest-core.jar` 
	(separated by : on mac and linux, by ; on windows), otherwise specify the 
	appropriate locations.
	
	For linux users they are usually in `/usr/share/java/jna.jar:/usr/share/java/junit4.jar`

2. Run all jUnit tests (please note that the -cp parameter now also has to 
	include the target directory we created in the first step, where the compiled 
	classfiles are):

		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.AllTests

   Or run all tests on their own:

		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.KeyTest
		java -cp <path to target, junit, jna and hamcrest> org.junit.runner.JUnitCore org.libelektra.KeySetTest

### Maven ###

When using Maven, the unit tests of the bindings will be automatically executed 
if you run all tests on a build (the target is called testjna_maven).

## Limitations ##

- no binary keys
- error/warning fixed to a special number, no generator for any
  error/warning
- no java iterator for metadata
- no simple way to print warning/errors
