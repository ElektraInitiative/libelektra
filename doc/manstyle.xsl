<?xml version='1.0'?>
<!-- vim:set sts=2 shiftwidth=2 syntax=sgml: -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- Hacked version to let use the same XML for man page and howto.
     It only defines a template for <section>, the rest is imported from the original XSL.
     
     Avi Alkalay
     03/2004
 -->

<xsl:import href="/usr/share/sgml/docbook/xsl-stylesheets/manpages/docbook.xsl"/>

<xsl:template match="section">
  <xsl:choose>
    <xsl:when test="ancestor::section">
      <xsl:text>&#10;.SS "</xsl:text>
      <xsl:value-of select="title[1]"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>&#10;.SH "</xsl:text>
      <xsl:value-of select="translate(title[1],'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>"&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>



</xsl:stylesheet>
