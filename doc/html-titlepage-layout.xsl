<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- This stylesheet was created by template/titlepage.xsl; do not edit it by hand. -->

<xsl:import href="html-params.xsl"/>

<xsl:template name="article.titlepage.recto">
  <xsl:choose>
    <xsl:when test="articleinfo/title">
      <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="articleinfo/title[1]"/>
    </xsl:when>
    <xsl:when test="artheader/title">
      <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="artheader/title[1]"/>
    </xsl:when>
    <xsl:when test="info/title">
      <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="info/title[1]"/>
    </xsl:when>
    <xsl:when test="title">
      <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="title[1]"/>
    </xsl:when>
  </xsl:choose>

  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="articleinfo/pubdate"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="artheader/pubdate"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="info/pubdate"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="articleinfo/authorgroup"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="artheader/authorgroup"/>
  <xsl:apply-templates mode="article.titlepage.recto.auto.mode" select="info/authorgroup"/>
</xsl:template>

<xsl:template match="authorgroup" mode="article.titlepage.recto.auto.mode">
  <xsl:apply-templates select="author" mode="article.titlepage.recto.auto.mode"/>
</xsl:template>

<xsl:template name="article.titlepage.verso">
</xsl:template>

<xsl:template name="article.titlepage">
  <div element="article" wrapper="div" class="titlepage">
    <div side="recto">
    <xsl:call-template name="article.titlepage.before.recto"/>
    <xsl:call-template name="article.titlepage.recto"/>
    </div>
    <div side="verso">
    <xsl:call-template name="article.titlepage.before.verso"/>
    <xsl:call-template name="article.titlepage.verso"/>
    </div>
    <xsl:call-template name="article.titlepage.separator"/>
  </div>
</xsl:template>

<xsl:template match="title" mode="article.titlepage.recto.auto.mode">
<div xsl:use-attribute-sets="article.titlepage.recto.style" predicate="[1]">
<xsl:apply-templates select="." mode="article.titlepage.recto.mode"/>
</div>
</xsl:template>

<xsl:template match="pubdate" mode="article.titlepage.recto.auto.mode">
<div xsl:use-attribute-sets="article.titlepage.recto.style">
<xsl:apply-templates select="." mode="article.titlepage.recto.mode"/>
</div>
</xsl:template>

<xsl:template match="authorgroup" mode="article.titlepage.recto.auto.mode">
<div xsl:use-attribute-sets="article.titlepage.recto.style">
<xsl:apply-templates select="." mode="article.titlepage.recto.mode"/>
</div>
</xsl:template>

</xsl:stylesheet>
