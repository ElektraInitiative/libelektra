
# $Id$
# $LastChangedBy$

NAME=elektra

DTDVERSION=0.1.0
SVNREP=http://germane-software.com/repositories/elektra


# Default dirs we use for installation, that can be substituted by the
# command line. See the README file or the RPM spec file.
BINDIR=/bin
UBINDIR=/usr/bin
LIBDIR=/lib
ULIBDIR=/usr/lib
CONFDIR=/etc
INCDIR=/usr/include
DOCDIR=/usr/share/doc
MANDIR=/usr/share/man
SGMLDIR=/usr/share/sgml


DIRS=src doc dtd





all:
	for x in ${DIRS}; do (cd "$$x"; make DTDVERSION=${DTDVERSION} \
		DOCDIR=${DOCDIR} MANDIR=${MANDIR} SGMLDIR=${SGMLDIR} \
		BINDIR=${BINDIR} LIBDIR=${LIBDIR} \
		UBINDIR=${UBINDIR} ULIBDIR=${ULIBDIR} \
		CONFDIR=${CONFDIR} INCDIR=${INCDIR} $@); done


cleanhere:
	-rm *~ elektra.spec svn-commit*
	-find . -name "*~" | xargs rm


clean: cleanhere
	for x in ${DIRS}; do (cd "$$x"; make $@); done

distclean: cleanhere
	for x in ${DIRS}; do (cd "$$x"; make $@); done


commit: clean
	cd ..; \
	svn ci elektra

vtag:
	PACK=`cat VERSION`;\
	svn cp ${SVNREP}/trunk ${SVNREP}/tags/$$PACK


dist: distclean elektra.spec
	# svn export wont work here...
	make -C doc docbookman   # leave mans already generated
	DIR=`basename \`pwd\``;\
	PACK=`cat VERSION`;\
	PACK=${NAME}-$$PACK;\
	cd ..;\
	ln -s $$DIR $$PACK;\
	tar --exclude .svn -czf $$PACK.tar.gz $$PACK/*;\
	rm $$PACK






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
	sed -e "s/_VERSION_/$$VERSION/g" < $< > $@ ;\
	cat ChangeLog >> $@




install: all
	[ -d "${DESTDIR}${LIBDIR}" ] || mkdir -p ${DESTDIR}${LIBDIR}
	[ -d "${DESTDIR}${ULIBDIR}" ] || mkdir -p ${DESTDIR}${ULIBDIR}
	[ -d "${DESTDIR}${BINDIR}" ] || mkdir -p ${DESTDIR}${BINDIR}
	[ -d "${DESTDIR}${INCDIR}" ] || mkdir -p ${DESTDIR}${INCDIR}
	[ -d "${DESTDIR}${CONFDIR}/profile.d" ] || mkdir -p ${DESTDIR}${CONFDIR}/profile.d
	[ -d "${DESTDIR}${DOCDIR}/${NAME}" ] || mkdir -p ${DESTDIR}${DOCDIR}/${NAME}
	[ -d "${DESTDIR}${DOCDIR}/${NAME}-devel" ] || mkdir -p ${DESTDIR}${DOCDIR}/${NAME}-devel
	for x in ${DIRS}; do (cd "$$x"; make DTDVERSION=${DTDVERSION} \
		DOCDIR=${DOCDIR} MANDIR=${MANDIR} SGMLDIR=${SGMLDIR} \
		BINDIR=${BINDIR} LIBDIR=${LIBDIR} \
		UBINDIR=${UBINDIR} ULIBDIR=${ULIBDIR} \
		CONFDIR=${CONFDIR} INCDIR=${INCDIR} $@); done
	cp scripts/elektraenv ${DESTDIR}${CONFDIR}/profile.d/elektraenv.sh
	chmod a+x example/*-convert
	cp LICENSE ${DESTDIR}${DOCDIR}/${NAME}
	cp ChangeLog ${DESTDIR}${DOCDIR}/${NAME}
	cp example/*-convert ${DESTDIR}${DOCDIR}/${NAME}
	cp example/*.c example/*.xml ${DESTDIR}${DOCDIR}/${NAME}-devel

