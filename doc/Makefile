# $Id$
# $LastChangedBy$

MANSTYLE=manstyle.xsl
HTMLSTYLE=htmltitlepage.xsl

.SUFFIXES: .xml .html .pdf .ps .rtf .xsl .fo .tpl
.SILENT:

.xml.html:
	if [ ! -L docbook ]; then ln -s ../../doc-common/docbook .; fi
	xsltproc -o $@ ${HTMLSTYLE} $<


all: man doxygen

doxygen:
	cd ..; doxygen
	find api/html | cpio -H tar -o | gzip -c --best > elektra-api.tar.gz

docbookman: kdb.1 elektra.7 elektra.5

man: doxygen docbookman

example.c:
	ln -s ../example/example.c .

csource.xml: example.c
	(echo "<programlisting><![CDATA["; sed -e 's/\&/\&amp\;/g' < example.c; echo "]]></programlisting>") > csource.xml

kdb.1: kdb.1.xml author.xml bestpract.xml rgexample.xml rgcmd.xml
	xsltproc ${MANSTYLE} $<

elektra.7: elektra.7.xml author.xml overview.xml bestpract.xml rgexample.xml
	xsltproc ${MANSTYLE} $<

elektra.5: elektra.5.xml author.xml storage.xml
	xsltproc ${MANSTYLE} $<

#kdb.3: kdb.3.xml author.xml csource.xml api.xml
#	xsltproc ${MANSTYLE} $<

#key.3: key.3.xml author.xml
#	xsltproc ${MANSTYLE} $<

howto.html: howto.xml elektra.7.xml author.xml garbage.xml overview.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml csource.xml api.xml society.xml

html: man howto.html doxygen
	man2html -r < kdb.1 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.1.html
	man2html -r < elektra.7 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.7.html
	man2html -r < elektra.5 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.5.html
	man2html -r < api/man/man3/kdb.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.3.html
	man2html -r < api/man/man3/key.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > key.3.html
	mv howto.html index.html






clean:
	-rm *.[1357]
	-rm *.html
	-rm *~
	-rm csource.xml
	-rm docbook
	-rm example.c
	-rm -rf api
	-rm -rf *-api.tar.gz



install: all
	[ -d ${DESTDIR}/usr/share/man/man1 ] || mkdir -p ${DESTDIR}/usr/share/man/man1
	[ -d ${DESTDIR}/usr/share/man/man3 ] || mkdir -p ${DESTDIR}/usr/share/man/man3
	[ -d ${DESTDIR}/usr/share/man/man5 ] || mkdir -p ${DESTDIR}/usr/share/man/man5
	[ -d ${DESTDIR}/usr/share/man/man7 ] || mkdir -p ${DESTDIR}/usr/share/man/man7
	find api/html | cpio -pdm ${DESTDIR}/usr/share/doc/elektra-devel/
	for m in `ls *.1`; do gzip -c --best $$m > ${DESTDIR}/usr/share/man/man1/$$m.gz; done
#	for m in `ls *.3`; do gzip -c --best $$m > ${DESTDIR}/usr/share/man/man3/$$m.gz; done
	cp api/man/man3/kdb*.3 api/man/man3/key*.3 api/man/man3/ks*.3 ${DESTDIR}/usr/share/man/man3/
	gzip ${DESTDIR}/usr/share/man/man3/*
	for m in `ls *.5`; do gzip -c --best $$m > ${DESTDIR}/usr/share/man/man5/$$m.gz; done
	for m in `ls *.7`; do gzip -c --best $$m > ${DESTDIR}/usr/share/man/man7/$$m.gz; done
#	cat kdb.3.xml | perl -e '{while (<>) {m/<function>(\w*)<\/function>/; print $$1 . "\n" if ($$1); }}' | \
#		grep kdb | sort -u | while read f; do ln -s kdb.3.gz ${DESTDIR}/usr/share/man/man3/$$f.3.gz; done
#	cat key.3.xml | perl -e '{while (<>) {m/<function>(\w*)<\/function>/; print $$1 . "\n" if ($$1); }}' | \
#		egrep "key|ks" | sort -u | while read f; do ln -s key.3.gz ${DESTDIR}/usr/share/man/man3/$$f.3.gz; done



pub: html doxygen
	find elektra-api.tar.gz *html docbook.css | cpio -o | ssh -l aviram shell.sf.net "cd /home/groups/e/el/elektra/htdocs; cpio -idvm; rm -rf api; tar -zxvf elektra-api.tar.gz"

pubi:
	find images | grep -v .svn | cpio -o | ssh -l aviram shell.sf.net "cd /home/groups/e/el/elektra/htdocs; cpio -idvm"
	
