# $Id$
# $LastChangedBy: aviram $

EXTRA_DIST=kdb.1.xml elektra.7.xml elektra.5.xml author.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml overview.xml $(man1_MANS) $(man5_MANS) $(man7_MANS)

if HAVE_XSLTPROC
if HAVE_STYLESHEET

man1_MANS = kdb.1
man5_MANS = elektra.5
man7_MANS = elektra.7

# Layout and confiruations to generate HTML for the web site
# HTML_TITLE_LAYOUT=html-titlepage-layout.tpl
# HTML_PARAMS=html-params.xsl

# Dependencies for Doxygen documentation
# DOXYSOURCES=../src/libelektra/libkdb.c ../src/kdb/kdb.c ../src/libelektratools/kdbtools.c ../src/include/kdb.h ../src/include/kdbbackend.h ../src/include/kdbprivate.h ../src/libelektra/key.c ../src/backends/template/template.c ../src/Doxyfile

# PATCHSOURCES=patches/xorg/README.xorg.Elektra.txt patches/sysvinit/README.sysvinit.Elektra.txt

#SUFFIXES: .xml .html .pdf .ps .rtf .xsl .fo .tpl

#
# TARGETS
#
#.xml.html:
#	$(xsltproc) -o ${HTML_TITLE_LAYOUT:.tpl=.xsl} $(dbroot)/template/titlepage.xsl ${HTML_TITLE_LAYOUT}
#	$(xsltproc) -o $@ ${HTML_TITLE_LAYOUT:.tpl=.xsl} $<


all: documentation

# html doxygen
documentation: docbookman 

docbookman: kdb.1 elektra.7 elektra.5
	

#
# Generate doxygen documentation
# This generate man for API too (see src/Doxyfile)
#
#doxygen: $(DOXYSOURCES)
#	@echo "Making Doxygen..."
#	cd ../src/; $(doxygen)

#
# Build man documentation
#
kdb.1: kdb.1.xml author.xml bestpract.xml rgexample.xml rgcmd.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<

elektra.7: elektra.7.xml author.xml overview.xml bestpract.xml rgexample.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<

elektra.5: elektra.5.xml author.xml storage.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<

#
# Build HTML documentation
#

# example.c:
#	LN_S ../example/example.c .

# csource.xml: example.c
#	(echo "<programlisting><![CDATA["; cat  example.c; echo "]]></programlisting>") > csource.xml

# howto.html: howto.xml elektra.7.xml author.xml garbage.xml overview.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml csource.xml api.xml society.xml

# html: howto.html doxygen patchdoc
#	$(man2html) -r < kdb.1 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.1.html
#	$(man2html) -r < elektra.7 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.7.html
#	$(man2html) -r < elektra.5 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.5.html
#	$(man2html) -r < elektra-api/man/man3/kdb.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.3.html
#	$(man2html) -r < elektra-api/man/man3/key.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > key.3.html
#	$(man2html) -r < elektra-api/man/man3/keyset.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > keyset.3.html
#	mv howto.html index.html

endif
endif
