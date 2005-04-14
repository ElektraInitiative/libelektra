# $Id$
# $LastChangedBy$

# The root dir of your DocBook XSL Stylesheets distribution
DBROOT=/usr/share/sgml/docbook/xsl-stylesheets

MANSTYLE=${DBROOT}/manpages/docbook.xsl

# Layout and confiruations to generate HTML for the web site
HTML_TITLE_LAYOUT=html-titlepage-layout.tpl
HTML_PARAMS=html-params.xsl

APIDIR=elektra-api
DOXYSOURCES=../src/libkdb.c ../src/kdb.c ../src/kdbtools.c ../src/kdb.h ../src/kdbbackend.h ../src/kdbprivate.h ../src/key.c ../src/backends/template/template.c ../src/Doxyfile
PATCHSOURCES=patches/xorg/README.xorg.Elektra.txt patches/sysvinit/README.sysvinit.Elektra.txt

# Default dirs we use for installation, that can be substituted by the command line.
DOCDIR=/usr/share/doc
MANDIR=/usr/share/man


# Workhorses
DOXYGEN=doxygen
XSLTPROC=xsltproc

.SUFFIXES: .xml .html .pdf .ps .rtf .xsl .fo .tpl
#.SILENT:





.xml.html:
	#if [ ! -L docbook ]; then ln -s ../../doc-common/docbook .; fi
	${XSLTPROC} -o ${HTML_TITLE_LAYOUT:.tpl=.xsl} ${DBROOT}/template/titlepage.xsl ${HTML_TITLE_LAYOUT}
	${XSLTPROC} -o $@ ${HTML_TITLE_LAYOUT:.tpl=.xsl} $<


all: man

doxygen: ${APIDIR}.tar.gz

docbookman: kdb.1 elektra.7 elektra.5

man: doxygen docbookman




elektra-api.tar.gz: $(DOXYSOURCES)
	-rm -rf ${APIDIR}
	-rm -rf ${APIDIR}.tar.gz
	echo "Making Doxygen..."
	cd ../src; ${DOXYGEN}
	tar -czf ${APIDIR}.tar.gz ${APIDIR}/html
	# find ${APIDIR}/html | cpio -H tar -o | gzip -c --best > ${APIDIR}.tar.gz



patchdoc: $(PATCHSOURCES)
	for f in $(PATCHSOURCES); do \
		nf=`echo $$f | sed -e 's/\.txt$$/\.html/g'`; \
		echo "<html><body><pre>" > $$nf; \
		cat $$f >> $$nf; \
		echo "</pre></body></html>" >> $$nf; \
	done
	

#	find patches/ -name "*.txt" | grep -v .svn | while read f; do \

example.c:
	ln -s ../example/example.c .




csource.xml: example.c
	(echo "<programlisting><![CDATA["; cat  example.c; echo "]]></programlisting>") > csource.xml
	#(echo "<programlisting><![CDATA["; sed -e 's/\&/\&amp\;/g' < example.c; echo "]]></programlisting>") > csource.xml




kdb.1: kdb.1.xml author.xml bestpract.xml rgexample.xml rgcmd.xml
	${XSLTPROC} ${MANSTYLE} $<




elektra.7: elektra.7.xml author.xml overview.xml bestpract.xml rgexample.xml
	${XSLTPROC} ${MANSTYLE} $<




elektra.5: elektra.5.xml author.xml storage.xml
	${XSLTPROC} ${MANSTYLE} $<




howto.html: howto.xml elektra.7.xml author.xml garbage.xml overview.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml csource.xml api.xml society.xml






html: man howto.html doxygen patchdoc
	man2html -r < kdb.1 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.1.html
	man2html -r < elektra.7 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.7.html
	man2html -r < elektra.5 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.5.html
	man2html -r < ${APIDIR}/man/man3/kdb.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.3.html
	man2html -r < ${APIDIR}/man/man3/key.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > key.3.html
	man2html -r < ${APIDIR}/man/man3/keyset.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > keyset.3.html
	mv howto.html index.html



# 'make distclean' will not remove docbook- and doxygen-generated man pages
distclean:
	-find patches/ -name "README*html" | grep -v .svn | xargs rm
	-rm *titlepage-layout.xsl
	-rm *.html
	-rm *~
	-rm csource.xml
	-rm docbook
	-rm example.c
	-rm -rf ${APIDIR}
	#-find ${APIDIR}/man | egrep -v "key.*3|ks.*3" | xargs rm
	#-rm -rf ${APIDIR}/html
	-rm -rf ${APIDIR}.tar.gz



clean: distclean
	-rm *.[1357]



install: all
	[ -d ${DESTDIR}${MANDIR}/man1 ] || mkdir -p ${DESTDIR}${MANDIR}/man1
	[ -d ${DESTDIR}${MANDIR}/man3 ] || mkdir -p ${DESTDIR}${MANDIR}/man3
	[ -d ${DESTDIR}${MANDIR}/man5 ] || mkdir -p ${DESTDIR}${MANDIR}/man5
	[ -d ${DESTDIR}${MANDIR}/man7 ] || mkdir -p ${DESTDIR}${MANDIR}/man7
	find ${APIDIR}/html | cpio -pdm ${DESTDIR}${DOCDIR}/elektra-devel/
	for m in `ls *.1`; do gzip -c --best $$m > ${DESTDIR}${MANDIR}/man1/$$m.gz; done
#	for m in `ls *.3`; do gzip -c --best $$m > ${DESTDIR}${MANDIR}/man3/$$m.gz; done
	gzip ${APIDIR}/man/man3/{kdb,key,ks}*.3
	cp ${APIDIR}/man/man3/{kdb,key,ks}*.3.gz ${DESTDIR}${MANDIR}/man3/
	for m in `ls *.5`; do gzip -c --best $$m > ${DESTDIR}${MANDIR}/man5/$$m.gz; done
	for m in `ls *.7`; do gzip -c --best $$m > ${DESTDIR}${MANDIR}/man7/$$m.gz; done
#	cat kdb.3.xml | perl -e '{while (<>) {m/<function>(\w*)<\/function>/; print $$1 . "\n" if ($$1); }}' | \
#		grep kdb | sort -u | while read f; do ln -s kdb.3.gz ${DESTDIR}${MANDIR}/man3/$$f.3.gz; done
#	cat key.3.xml | perl -e '{while (<>) {m/<function>(\w*)<\/function>/; print $$1 . "\n" if ($$1); }}' | \
#		egrep "key|ks" | sort -u | while read f; do ln -s key.3.gz ${DESTDIR}${MANDIR}/man3/$$f.3.gz; done



pub: html doxygen
	find elektra-api.tar.gz *html docbook.css patches/ | grep -v .svn | cpio -o | ssh -l aviram shell.sf.net "cd /home/groups/e/el/elektra/htdocs; rm -rf patches; cpio -idvm; rm -rf api; tar -zxvf elektra-api.tar.gz"



pubh: html
	scp *html docbook.css aviram@shell.sf.net:/home/groups/e/el/elektra/htdocs



pubd: doxygen
	find elektra-api.tar.gz | cpio -o | ssh -l aviram shell.sf.net "cd /home/groups/e/el/elektra/htdocs; cpio -idvm; rm -rf api; tar -zxvf elektra-api.tar.gz"



pubi:
	find images | grep -v .svn | cpio -o | ssh -l aviram shell.sf.net "cd /home/groups/e/el/elektra/htdocs; cpio -idvm"
	
