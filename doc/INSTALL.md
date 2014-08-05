# INSTALL #


## DEBIAN ##

You might want to use:

	dpkg-buildpackage -us -uc -sa

to build a debian package.
(In the Debian branch, see GIT)


## GENERIC ##

First follow the steps in COMPILE.

To install elektra use:

	sudo make install


## CPACK ##

First follow the steps in COMPILE.

Then use:

	cpack

which should create a package for distributions where a Generator is
implemented. See cmake/ElektraPackaging.cmake for available Generators.