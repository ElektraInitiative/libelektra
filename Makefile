
# $Id$
# $LastChangedBy$


CC=gcc -O3 -g -Wcomment -Wformat -Wimplicit-int -Wimplicit-function-declaration -Wparentheses -Wreturn-type -Wunused -Wuninitialized
XMLINCLUDES=`xml2-config --cflags`
XMLLIBS=`xml2-config --libs`

DTDVERSION=0.1.0


DIRS=doc dtd



.c.o:
	${CC} -g -fpic -o $@ -c $<



all: libregistry.a libregistry.so rg
	for x in ${DIRS}; do (cd "$$x"; make $@); done

	
	
	
clean:
	-rm *~ *.o core.* libregistry.so libregistry.a rg registry.spec
	-find . -name "*~" | xargs rm
	for x in ${DIRS}; do (cd "$$x"; make $@); done

	
	



libregistry.so: localregistry.o key.o
	${CC} -shared -fpic -o $@ localregistry.o key.o



libregistry.a: localregistry.o key.o
	ar q $@ localregistry.o key.o




key.o: key.c registry.h registryprivate.h
	${CC} -g -fpic -o $@ -c $<




	
localregistry.o: localregistry.c registry.h registryprivate.h
	${CC} -g -fpic -c $<


	
	
rg.o: rg.c registry.h registryprivate.h
	${CC} -g ${XMLINCLUDES} -fpic -c $<



rg: rg.o
	${CC} -g -L. ${XMLLIBS} -lregistry -o $@ $<
	# ld -shared -lc -lregistry -static -lxml2 -lz -o $@ $<

	
	
dist: clean registry.spec
	DIR=`basename \`pwd\``;\
	PACK=`cat VERSION`;\
	PACK=registry-$$PACK;\
	cd ..;\
	find $$DIR/ | grep -v .svn | cpio -H tar -o | gzip --best -c > $$PACK.tar.gz

	
	
rpm: dist
	VER=`cat VERSION`;\
	rpmbuild -ta ../registry-$$VER.tar.gz

	
	
	
registry.spec: registry.spec.in
	VERSION=`cat VERSION` ;\
	sed -e "s/_VERSION_/$$VERSION/g" < registry.spec.in > registry.spec ;\
	cat ChangeLog >> registry.spec


	
	
install: all
	for x in ${DIRS}; do (cd "$$x"; make DTDVERSION=${DTDVERSION} $@); done
	strip libregistry.so
	cp libregistry.so ${DESTDIR}/lib
	cp libregistry.a ${DESTDIR}/usr/lib
	strip rg
	cp rg ${DESTDIR}/bin
	cp registry.h ${DESTDIR}/usr/include
	cp scripts/rgsetenv ${DESTDIR}/etc/profile.d/rgsetenv.sh
	cp example/*-convert ${DESTDIR}/usr/share/doc/registry
	cp rg.c example/example.c ${DESTDIR}/usr/share/doc/registry-devel
	
