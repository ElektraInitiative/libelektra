# Installation on ArchLinux #

* Create a build directory and compile the library

	mkdir build
	cd build
	cmake ..

* Install the files using the generated Makefile

	sudo make install 

## Troubleshooting ##

If you encounter the problem that the library can not be found (output like this)

	kdb: error while loading shared libraries: libelektratools.so: cannot open shared object file: No such file or directory

You need to create a configuration file under `/etc/ld.so.conf.d/` (e.g. `/etc/ld.so.conf.d/elektra.conf`).

Add the path where the library has been installed

	/usr/lib/local/

and run `ldconfig` as root.
