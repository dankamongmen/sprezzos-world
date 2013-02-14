<?xml version="1.0" encoding="UTF-8" ?><!--*-coding: utf-8; -*-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!--

Sort *-device.xml using xslt.  Use it like this:

   xsltproc devsort.xsl bus-device.xml > bus-device.xml.new &&
     bus-device.xml.new bus-device.xml

-->

  <xsl:output method="xml" indent="yes" encoding="UTF-8"/>

  <xsl:attribute-set name="bus">
    <xsl:attribute name="bus"><xsl:value-of select="@bus"/></xsl:attribute>
  </xsl:attribute-set>

  <xsl:template match="/device_list">
    <xsl:copy use-attribute-sets="bus">
     <xsl:for-each select="/device_list/device">
       <xsl:sort select="@vendor"/>
       <xsl:sort select="@model"/>
       <xsl:sort select="@subvendor"/>
       <xsl:sort select="@subdevice"/>
       <xsl:sort select="@busclass"/>
       <xsl:copy-of select="."/>
     </xsl:for-each>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
