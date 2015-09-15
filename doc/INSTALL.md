# INSTALL #


## DEBIAN ##

You might want to use:

	dpkg-buildpackage -us -uc -sa

to build a debian package.
(In the Debian branch, see GIT)

## GENERIC ##

First follow the steps in [COMPILE](COMPILE.md).

To install Elektra use:

	sudo make install

To uninstall Elektra use (will not be very clean,
e.g. it will not remove directories and *.pyc files):

	sudo make uninstall

or in the build directory (will not honor DESTDIR!):

	xargs rm < install_manifest.txt


## CPACK ##

First follow the steps in [COMPILE](COMPILE.md)..

Then use:

	cpack

which should create a package for distributions where a Generator is
implemented. See [this cmake file](cmake/ElektraPackaging.cmake) for available Generators
and send a merge request for your system.

## TROUBLESHOOTING ##

If you encounter the problem that the library can not be found (output like this)

	kdb: error while loading shared libraries: libelektratools.so: cannot open shared object file: No such file or directory

You need to place a configuration file at `/etc/ld.so.conf.d/` (e.g. `/etc/ld.so.conf.d/elektra.conf`).

Add the path where the library has been installed

	/usr/lib/local/

and run `ldconfig` as root.
