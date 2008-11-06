ACLOCAL_AMFLAGS = -I m4

SUBDIRS = libltdl src doc benchmarks examples scripts xmlschema tests
EXTRA_DIST=elektra.pc elektratools.pc elektracpp.pc elektra.spec elektra.mandriva.spec elektra.xml

pkgconfigdir = $(ulibdir)/pkgconfig
pkgconfig_DATA = elektra.pc elektratools.pc elektracpp.pc

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

MAINTAINERCLEANFILES = \
	configure \
	depcomp \
	compile \
	config.guess \
	config.sub \
	ltmain.sh \
	missing \
	aclocal.m4 \
	install-sh \
	Makefile.in \
	m4/ltsugar.m4 \
	m4/argz.m4 \
	m4/libtool.m4 \
	m4/ltversion.m4 \
	m4/lt~obsolete.m4 \
	m4/ltoptions.m4 \
	m4/ltdl.m4

maintainer-clean-local:
	-chmod -R a+rw $(distdir)
	-rm -fr $(distdir)
	-rm -fr libltdl

