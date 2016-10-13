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


## Limitations ##

- no binary keys
- no full names
- error/warning fixed to a special number, no generator for any
  error/warning
- no java iterator for metadata
- no simple way to print warning/errors
- no junit tests, so its likely that there are some bugs
