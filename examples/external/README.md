This folder contains examples on how to write applications
using Elektra outside of the Elektra source tree.

[See how to build and run example](tests/shell/check_external.sh)

Note that in a real-world build you should be careful with using `-Wl,-rpath`. In most cases you should only use it for development
purposes and not in a release build. Therefore you should not use the Makefile provided in the pkg-config example for release builds.
