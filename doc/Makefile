

MANSTYLE=manstyle.xsl
HTMLSTYLE=htmltitlepage.xsl

.SUFFIXES: .xml .html .pdf .ps .rtf .xsl .fo .tpl

.xml.html:
	xsltproc -o $@ ${HTMLSTYLE} $<


all: man


man: rg.1 registry.7 registry.5 registry.3 key.3


csource.xml: example.c
	(echo "<programlisting><![CDATA["; sed -e 's/\&/\&amp\;/g' < example.c; echo "]]></programlisting>") > csource.xml

rg.1: rg.1.xml author.xml bestpract.xml rgexample.xml rgcmd.xml
	xsltproc ${MANSTYLE} $<

registry.7: registry.7.xml author.xml generics.xml bestpract.xml rgexample.xml
	xsltproc ${MANSTYLE} $<

registry.5: registry.5.xml author.xml storage.xml
	xsltproc ${MANSTYLE} $<

registry.3: registry.3.xml author.xml csource.xml api.xml
	xsltproc ${MANSTYLE} $<

key.3: key.3.xml author.xml
	xsltproc ${MANSTYLE} $<

howto.html: howto.xml registry.7.xml author.xml generics.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml csource.xml api.xml society.xml

html: man howto.html
	man2html -r < rg.1 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > rg.1.html
	man2html -r < registry.7 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > registry.7.html
	man2html -r < registry.5 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > registry.5.html
	man2html -r < registry.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > registry.3.html
	man2html -r < key.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > key.3.html
	mv howto.html index.html






clean:
	-rm *.[1357]
	-rm *.html
	-rm *~
	-rm csource.xml

install: man
	for m in `ls *.1`; do gzip -c --best $$m > ${DESTDIR}/usr/share/man/man1/$$m.gz; done
	for m in `ls *.7`; do gzip -c --best $$m > ${DESTDIR}/usr/share/man/man7/$$m.gz; done
	for m in `ls *.5`; do gzip -c --best $$m > ${DESTDIR}/usr/share/man/man5/$$m.gz; done
	for m in `ls *.3`; do gzip -c --best $$m > ${DESTDIR}/usr/share/man/man3/$$m.gz; done
	cat registry.3.xml | perl -e '{while (<>) {m/<function>(\w*)<\/function>/; print $$1 . "\n" if ($$1); }}' | \
		grep registry | sort -u | while read f; do ln -s registry.3.gz ${DESTDIR}/usr/share/man/man3/$$f.3.gz; done
	cat key.3.xml | perl -e '{while (<>) {m/<function>(\w*)<\/function>/; print $$1 . "\n" if ($$1); }}' | \
		egrep "key|ks" | sort -u | while read f; do ln -s key.3.gz ${DESTDIR}/usr/share/man/man3/$$f.3.gz; done
	
	
	