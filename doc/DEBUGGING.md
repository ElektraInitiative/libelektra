# DEBUGGING

Elektra can be debugged using gdb (either standalone or as part of an IDE like CLion).

To see the source files during debugging follow these steps:

1. Follow the [Get started instructions](/doc/GETSTARTED.md) to clone and install the repository.  
   Note: To disable compiler optimizations and add debug symbols, add these flags when calling `cmake`:  
   `cmake -DCMAKE_BUILD_TYPE=DEBUG -DENABLE_DEBUG=on -DCMAKE_C_FLAGS=-O0 -g`
2. Before starting gdb, set the environment variable `LD_LIBRARY_PATH` to the path where libelektra was installed during `make install` during step 1 (e.g. on Ubuntu 20.04.3 use `export LD_LIBRARY_PATH=/usr/local/lib/elektra`).  
   If you use CLion, set the environment variable in your Debug Configuration.
3. After any code changes, you need to execute `make install` again to apply your changes.
