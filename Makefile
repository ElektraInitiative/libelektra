
# $Id$
# $LastChangedBy$

NAME=elektra
CC=gcc -O3 -g -Wcomment -Wformat -Wimplicit-int -Wimplicit-function-declaration -Wparentheses -Wreturn-type -Wunused -Wuninitialized
XMLINCLUDES=`xml2-config --cflags`
XMLLIBS=`xml2-config --libs`

DTDVERSION=0.1.0
SVNREP=http://germane-software.com/repositories/elektra


DIRS=doc dtd



.c.o:
	${CC} -g -fpic -o $@ -c $<



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
	find $$DIR/ | grep -v .svn | sort | cpio -H tar -o | gzip --best -c > $$PACK.tar.gz






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
	for x in ${DIRS}; do (cd "$$x"; make DTDVERSION=${DTDVERSION} $@); done
	strip libkdb.so
	strip libregistry.so  # remove me in the future
	[ -d "${DESTDIR}/lib" ] || mkdir -p ${DESTDIR}/lib
	[ -d "${DESTDIR}/usr/lib" ] || mkdir -p ${DESTDIR}/usr/lib
	[ -d "${DESTDIR}/bin" ] || mkdir -p ${DESTDIR}/bin
	[ -d "${DESTDIR}/usr/include" ] || mkdir -p ${DESTDIR}/usr/include
	[ -d "${DESTDIR}/etc/profile.d" ] || mkdir -p ${DESTDIR}/etc/profile.d
	[ -d "${DESTDIR}/usr/share/doc/${NAME}" ] || mkdir -p ${DESTDIR}/usr/share/doc/${NAME}
	[ -d "${DESTDIR}/usr/share/doc/${NAME}-devel" ] || mkdir -p ${DESTDIR}/usr/share/doc/${NAME}-devel
	cp libkdb.so ${DESTDIR}/lib
	cp libkdb.a ${DESTDIR}/usr/lib
	cp libregistry.so ${DESTDIR}/lib # remove me in the future
	strip kdb
	cp kdb ${DESTDIR}/bin
	cp kdb.h ${DESTDIR}/usr/include
	cp scripts/elektraenv ${DESTDIR}/etc/profile.d/elektraenv.sh
	chmod a+x example/*-convert
	cp LICENSE ${DESTDIR}/usr/share/doc/${NAME}
	cp example/*-convert ${DESTDIR}/usr/share/doc/${NAME}
	cp kdb.c example/example.c ${DESTDIR}/usr/share/doc/${NAME}-devel

