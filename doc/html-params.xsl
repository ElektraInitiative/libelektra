<?xml version='1.0'?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		exclude-result-prefixes="#default">

	<xsl:import href="/usr/share/sgml/docbook/xsl-stylesheets/xhtml/chunk.xsl"/>
	<!--xsl:import href="_DBSTYLESHEET_"/-->

	<xsl:output method="html" indent="no" encoding="UTF-8"/>

	<xsl:param name="chunk.section.depth" select="'1'"/>
	<xsl:param name="chunker.output.method" select="'html'"/>

	<xsl:param name="html.stylesheet" select="'docbook.css'"/>
	<xsl:param name="html.stylesheet.type">text/css</xsl:param>

	<xsl:param name="admon.graphics.extension" select="'.gif'"/>
	<xsl:param name="admon.graphics" select="1"/>
	<xsl:param name="callout.graphics.extension" select="'.gif'"/>
	<xsl:param name="callouts.extension" select="'0'"/>

	<xsl:param name="section.autolabel" select="'0'"/>
	<xsl:param name="appendix.autolabel" select="'0'"/>

	<xsl:param name="suppress.navigation">1</xsl:param>

	<xsl:param name="shade.verbatim" select="'0'"/>

	<xsl:param name="toc.max.depth">8</xsl:param>
	<xsl:param name="toc.section.depth">2</xsl:param>

	<xsl:param name="admon.style">
		<xsl:text>margin-left: 0.5in; margin-right: 1in;</xsl:text>
	</xsl:param>

	<xsl:param name="use.id.as.filename" select="'1'"/>

	<xsl:param name="formal.title.placement">
figure after
example before
equation after
table before
procedure before
	</xsl:param>

	<xsl:param name="generate.toc">
appendix  toc
/article   toc
/book      toc,figure,table,example,equation
chapter   toc
part      toc
preface   toc
qandadiv  toc
qandaset  toc
reference toc
section   toc
set       toc
	</xsl:param>

</xsl:stylesheet>
