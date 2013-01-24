<?xml version="1.0" encoding="utf-8"?>

<!-- Magic: -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                version="1.0">

<xsl:import href="/usr/share/xml/docbook/stylesheet/nwalsh/html/chunk.xsl"/>

<xsl:import href="aptitude-common.xsl"/>

<xsl:output method="html" encoding="UTF-8"/>
<xsl:param name="chunker.output.encoding" select="'utf-8'"/>
<xsl:param name="chunker.output.doctype-public"
    select="'-//W3C//DTD HTML 4.01 Transitional//EN'"/>
<xsl:param name="chunker.output.doctype-system"
    select="'http://www.w3.org/TR/html4/loose.dtd'"/>

<xsl:param name="chunk.section.depth" select="2"/>
<xsl:param name="chunk.quietly" select="1"/>
<xsl:param name="chunk.first.sections" select="1"/>
<xsl:param name="generate.section.toc.level" select="3"/>

<xsl:param name="html.stylesheet">aptitude.css</xsl:param>

<!-- Borrowed from the DocBook XSL reference -->
<xsl:template name="tr.attributes">
  <xsl:param name="row" select="."/>
  <xsl:param name="rownum" select="0"/>

  <xsl:if test="not(ancestor::table/@tabstyle = 'unstriped')">
    <xsl:if test="$rownum mod 2 = 0">
      <xsl:attribute name="class">oddrow</xsl:attribute>
    </xsl:if>
  </xsl:if>

</xsl:template>


</xsl:stylesheet>
