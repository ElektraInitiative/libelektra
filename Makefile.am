# $Id$

ACLOCAL_AMFLAGS = -I m4

SUBDIRS = libltdl src examples doc scripts xmldtd xmlschema tests rpmspecs
EXTRA_DIST=elektra.pc elektratools.pc elektra.spec elektra.xml 

pkgconfigdir = $(ulibdir)/pkgconfig
pkgconfig_DATA = elektra.pc elektratools.pc

RPMARCH=`rpmbuild --showrc | awk '/^build arch/ {print $$4}'`

rpm: dist
	mkdir -p rpmdir/BUILD \
	rpmdir/SPECS \
	rpmdir/SOURCES \
	rpmdir/SRPMS \
	rpmdir/RPMS/$(RPMARCH)
	rpmbuild --clean -ta $(PACKAGE)-$(VERSION).tar.gz --define "_topdir $$PWD/rpmdir"
	@echo "============================================================"
	@echo "Finished - the packages are in rpmdir/RPMS and rpmdir/SRPMS!"

