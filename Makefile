
# $Id$
# $LastChangedBy$

NAME=elektra
CC=gcc ${OPTIMIZATIONS} -Wcomment -Wformat -Wimplicit-int -Wimplicit-function-declaration -Wparentheses -Wreturn-type -Wunused -Wuninitialized
XMLINCLUDES=`xml2-config --cflags`
XMLLIBS=`xml2-config --libs`

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


DIRS=doc dtd



.c.o:
	${CC} -fpic -o $@ -c $<



all: libkdb.a libkdb.so libregistry.so kdb
	for x in ${DIRS}; do (cd "$$x"; make $@); done


cleanhere:
	-rm core* *~ *.o *.so *.a kdb elektra.spec localeinfo svn-commit*
	-find . -name "*~" | xargs rm


clean: cleanhere
	for x in ${DIRS}; do (cd "$$x"; make $@); done

distclean: cleanhere
	for x in ${DIRS}; do (cd "$$x"; make $@); done

libregistry.so: registrystub.o libkdb.so
	${CC} -shared -fpic -o $@ libkdb.so registrystub.o


libkdb.so: localkdb.o key.o
	${CC} -shared -fpic -o $@ localkdb.o key.o



libkdb.a: localkdb.o key.o
	ar q $@ localkdb.o key.o




key.o: key.c kdb.h kdbprivate.h
	${CC} -g -fpic -o $@ -c $<





localkdb.o: localkdb.c kdb.h kdbprivate.h
	${CC} -g -fpic -c $<




kdb.o: kdb.c kdb.h kdbprivate.h
	${CC} -g ${XMLINCLUDES} -fpic -c $<



kdb: kdb.o libkdb.so
	${CC} -g -L. ${XMLLIBS} -lkdb -o $@ $<
	# ld -shared -lc -lkdb -static -lxml2 -lz -o $@ $<


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
	tar --exclude .svn -czf $$PACK.tar.gz $$DIR/






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
	for x in ${DIRS}; do (cd "$$x"; make DTDVERSION=${DTDVERSION} \
		DOCDIR=${DOCDIR} MANDIR=${MANDIR} SGMLDIR=${SGMLDIR} $@); done
	# Strips will be done automatically by rpmbuild
	# strip libkdb.so
	# strip libregistry.so  # remove me in the future
	# strip kdb
	[ -d "${DESTDIR}${LIBDIR}" ] || mkdir -p ${DESTDIR}${LIBDIR}
	[ -d "${DESTDIR}${ULIBDIR}" ] || mkdir -p ${DESTDIR}${ULIBDIR}
	[ -d "${DESTDIR}${BINDIR}" ] || mkdir -p ${DESTDIR}${BINDIR}
	[ -d "${DESTDIR}${INCDIR}" ] || mkdir -p ${DESTDIR}${INCDIR}
	[ -d "${DESTDIR}${CONFDIR}/profile.d" ] || mkdir -p ${DESTDIR}${CONFDIR}/profile.d
	[ -d "${DESTDIR}${DOCDIR}/${NAME}" ] || mkdir -p ${DESTDIR}${DOCDIR}/${NAME}
	[ -d "${DESTDIR}${DOCDIR}/${NAME}-devel" ] || mkdir -p ${DESTDIR}${DOCDIR}/${NAME}-devel
	cp libkdb.so ${DESTDIR}${LIBDIR}
	cp libkdb.a ${DESTDIR}${ULIBDIR}
	cp libregistry.so ${DESTDIR}${LIBDIR} # remove me in the future
	cp kdb ${DESTDIR}${BINDIR}
	cp kdb.h ${DESTDIR}${INCDIR}
	cp scripts/elektraenv ${DESTDIR}${CONFDIR}/profile.d/elektraenv.sh
	chmod a+x example/*-convert
	cp LICENSE ${DESTDIR}${DOCDIR}/${NAME}
	cp ChangeLog ${DESTDIR}${DOCDIR}/${NAME}
	cp example/*-convert ${DESTDIR}${DOCDIR}/${NAME}
	cp kdb.c example/example.c ${DESTDIR}${DOCDIR}/${NAME}-devel

