
# $Id$
# $LastChangedBy$

NAME=elektra

DTDVERSION=0.1.1
SVNREP=http://germane-software.com/repositories/elektra

# List here the backends to be compiled
BACKENDS="filesys berkeleydb gconf fstab ini"
# The one single default backend
DEFAULT_BACKEND=filesys

# Default dirs we use for installation, that can be substituted by the
# command line. See the README file or the RPM spec file.
BINDIR=/bin
UDIR=/usr
UBINDIR=$(UDIR)/bin
LIBDIR=/lib
ULIBDIR=$(UDIR)/lib
CONFDIR=/etc
INCDIR=$(UDIR)/include
DOCDIR=$(UDIR)/share/doc
MANDIR=$(UDIR)/share/man
SGMLDIR=$(UDIR)/share/sgml


DIRS=src xmldtd xmlschema doc





all: elektra.pc
	for x in ${DIRS}; do (cd "$$x"; $(MAKE) \
		BACKENDS=${BACKENDS} \
		OPTIMIZATIONS="${OPTIMIZATIONS}" \
		DTDVERSION=${DTDVERSION} \
		DOCDIR=${DOCDIR} MANDIR=${MANDIR} SGMLDIR=${SGMLDIR} \
		BINDIR=${BINDIR} LIBDIR=${LIBDIR} \
		UBINDIR=${UBINDIR} ULIBDIR=${ULIBDIR} \
		CONFDIR=${CONFDIR} INCDIR=${INCDIR} $@); \
	done


cleanhere:
	-rm -f *~ elektra.spec elektra.pc svn-commit*
	-find . -name "*~" -o -name ".kdbg*" | xargs rm -f


clean: cleanhere
	for x in ${DIRS}; do (cd "$$x"; $(MAKE) BACKENDS=${BACKENDS} $@); done

distclean: cleanhere
	for x in ${DIRS}; do (cd "$$x"; $(MAKE) BACKENDS=${BACKENDS} $@); done


commit: clean
	cd ..; \
	svn ci elektra

vtag:
	PACK=`cat VERSION`;\
	svn cp ${SVNREP}/trunk ${SVNREP}/tags/$$PACK


dist: distclean elektra.spec
	# svn export wont work here...
	$(MAKE) -C doc docbookman   # leave mans already generated
	DIR=`basename \`pwd\``;\
	PACK=`cat VERSION`;\
	PACK=${NAME}-$$PACK;\
	cd ..;\
	ln -s $$DIR $$PACK;\
	tar --exclude .svn --exclude Homepage -czf $$PACK.tar.gz $$PACK/*;\
	rm -f $$PACK






rpmsig: dist
	VER=`cat VERSION`;\
	rpmbuild --sign -ta ../${NAME}-$$VER.tar.gz


rpm: dist
	VER=`cat VERSION`;\
	rpmbuild -ta ../${NAME}-$$VER.tar.gz


deb:
	dpkg-buildpackage -rfakeroot


elektra.spec: elektra.spec.in
	VERSION=`cat VERSION` ;\
	sed -e "s/_VERSION_/$$VERSION/g; s/_DTDVERSION_/${DTDVERSION}/g;" < $< > $@ ;\
	cat ChangeLog >> $@

elektra.pc: elektra.pc.in
	VERSION=`cat VERSION` ;\
	PCUDIR=`echo $(UDIR) | sed -e "s,/,\\\/,g"` ;\
	PCLIBDIR=`echo $(LIBDIR) | sed -e "s,/,\\\/,g"` ;\
	cat $< | sed -e "s/_VERSION_/$$VERSION/g" |\
	sed -e "s,_UDIR_,$$PCUDIR,g" |\
	sed -e "s,_LIBDIR_,$$PCLIBDIR,g" > $@ ;



install: all
	[ -d "${DESTDIR}${LIBDIR}" ] || mkdir -p ${DESTDIR}${LIBDIR}
	[ -d "${DESTDIR}${ULIBDIR}" ] || mkdir -p ${DESTDIR}${ULIBDIR}
	[ -d "${DESTDIR}${ULIBDIR}/pkgconfig" ] || mkdir -p ${DESTDIR}${ULIBDIR}/pkgconfig
	[ -d "${DESTDIR}${BINDIR}" ] || mkdir -p ${DESTDIR}${BINDIR}
	[ -d "${DESTDIR}${INCDIR}" ] || mkdir -p ${DESTDIR}${INCDIR}
	[ -d "${DESTDIR}${CONFDIR}/profile.d" ] || mkdir -p ${DESTDIR}${CONFDIR}/profile.d
	[ -d "${DESTDIR}${DOCDIR}/${NAME}" ] || mkdir -p ${DESTDIR}${DOCDIR}/${NAME}
	[ -d "${DESTDIR}${DOCDIR}/${NAME}-devel/examples" ] || mkdir -p ${DESTDIR}${DOCDIR}/${NAME}-devel/examples
	for x in ${DIRS}; do (cd "$$x"; $(MAKE) DESTDIR=${DESTDIR} \
		DTDVERSION=${DTDVERSION} BACKENDS=${BACKENDS} \
		DOCDIR=${DOCDIR} MANDIR=${MANDIR} SGMLDIR=${SGMLDIR} \
		BINDIR=${BINDIR} LIBDIR=${LIBDIR} \
		UBINDIR=${UBINDIR} ULIBDIR=${ULIBDIR} \
		CONFDIR=${CONFDIR} INCDIR=${INCDIR} $@); \
	done
	cp scripts/elektraenv ${DESTDIR}${CONFDIR}/profile.d/elektraenv.sh
	chmod a+x example/*-convert
	cp elektra.pc ${DESTDIR}${ULIBDIR}/pkgconfig/elektra.pc
	cp LICENSE ${DESTDIR}${DOCDIR}/${NAME}
	cp ChangeLog ${DESTDIR}${DOCDIR}/${NAME}
	cp example/*-convert example/*.xml ${DESTDIR}${DOCDIR}/${NAME}
	cp example/*.c ${DESTDIR}${DOCDIR}/${NAME}-devel/examples/
	-LD_LIBRARY_PATH='${DESTDIR}${LIBDIR}':"$$LD_LIBRARY_PATH" [ `id -u` -eq "0" ] && kdb set system/sw/kdb/current/schemapath '${DESTDIR}${SGMLDIR}/elektra-${DTDVERSION}/elektra.xsd'
