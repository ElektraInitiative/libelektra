<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- This stylesheet was created by template/titlepage.xsl; do not edit it by hand. -->

<xsl:import xmlns:xsl="http://www.w3.org/1999/XSL/Transform" href="htmlmystyle.xsl"/>

	
		

<xsl:template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" name="article.titlepage.recto">
			
  <xsl:choose xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:when xmlns:xsl="http://www.w3.org/1999/XSL/Transform" test="articleinfo/title">
      <xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" mode="article.titlepage.recto.auto.mode" select="articleinfo/title[1]"/>
    </xsl:when>
    <xsl:when xmlns:xsl="http://www.w3.org/1999/XSL/Transform" test="artheader/title">
      <xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" mode="article.titlepage.recto.auto.mode" select="artheader/title[1]"/>
    </xsl:when>
    <xsl:when xmlns:xsl="http://www.w3.org/1999/XSL/Transform" test="title">
      <xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" mode="article.titlepage.recto.auto.mode" select="title[1]"/>
    </xsl:when>
  </xsl:choose>

			
  <xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" mode="article.titlepage.recto.auto.mode" select="articleinfo/pubdate"/>
  <xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" mode="article.titlepage.recto.auto.mode" select="artheader/pubdate"/>

			
  <xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" mode="article.titlepage.recto.auto.mode" select="articleinfo/authorgroup"/>
  <xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" mode="article.titlepage.recto.auto.mode" select="artheader/authorgroup"/>

		
</xsl:template>
			
			

			

<xsl:template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" match="authorgroup" mode="article.titlepage.recto.auto.mode">
  <xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" select="author" mode="article.titlepage.recto.auto.mode"/>
</xsl:template>

		
	

<xsl:template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" name="article.titlepage">
  <div class="titlepage">
    <xsl:call-template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" name="article.titlepage.before.recto"/>
    <xsl:call-template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" name="article.titlepage.recto"/>
    <xsl:call-template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" name="article.titlepage.before.verso"/>
    <xsl:call-template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" name="article.titlepage.verso"/>
    <xsl:call-template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" name="article.titlepage.separator"/>
  </div>
</xsl:template>

<xsl:template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" match="title" mode="article.titlepage.recto.auto.mode">
<div use-attribute-sets="article.titlepage.recto.style">
<xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" select="." mode="article.titlepage.recto.mode"/>
</div>
</xsl:template>

<xsl:template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" match="pubdate" mode="article.titlepage.recto.auto.mode">
<div use-attribute-sets="article.titlepage.recto.style">
<xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" select="." mode="article.titlepage.recto.mode"/>
</div>
</xsl:template>

<xsl:template xmlns:xsl="http://www.w3.org/1999/XSL/Transform" match="authorgroup" mode="article.titlepage.recto.auto.mode">
<div use-attribute-sets="article.titlepage.recto.style">
<xsl:apply-templates xmlns:xsl="http://www.w3.org/1999/XSL/Transform" select="." mode="article.titlepage.recto.mode"/>
</div>
</xsl:template>


</xsl:stylesheet>
