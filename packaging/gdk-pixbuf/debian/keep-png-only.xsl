<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:smi="http://www.freedesktop.org/standards/shared-mime-info">

<!--
Since a namespace is declared in the XML document, this stylesheet
needs to declare it as well to be able to match the various elements,
that's why there's xmlns:smi above, and smi:mime-* below.
-->

<!-- Identity template. -->
<xsl:template match="@*|node()">
 <xsl:copy>
  <xsl:apply-templates select="@*|node()"/>
 </xsl:copy>
</xsl:template>

<!-- Remove unneeded elements. -->
<xsl:template match='smi:mime-type[@type!="image/png"]' />

<!-- Avoid plenty of empty lines. -->
<xsl:strip-space elements="smi:mime-info" />

</xsl:stylesheet>
