<?xml version='1.0'?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		exclude-result-prefixes="#default">

	<xsl:import href="docbook/html/docbook.xsl"/>

	<xsl:param name="html.stylesheet" select="'docbook.css'"/>
	<xsl:param name="html.stylesheet.type">text/css</xsl:param>

	<xsl:param name="admon.graphics.extension" select="'.gif'"/>
	<xsl:param name="admon.graphics" select="1"/>
	<xsl:param name="callout.graphics.extension" select="'.gif'"/>
	<xsl:param name="callouts.extension" select="'0'"/>

	<xsl:param name="shade.verbatim" select="'0'"/>

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
/article   toc,figure,table,example,equation
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
