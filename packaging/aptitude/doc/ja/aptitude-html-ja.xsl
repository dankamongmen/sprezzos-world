<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                version="1.0">
<xsl:import href="../aptitude-html.xsl"/>
<xsl:param name="chunker.output.encoding" select="'utf-8'"/>
<xsl:output method="text"
            encoding="utf-8"
	    indent="no"/>
<xsl:param name="menuchoice.menu.separator" select="' → '"/>
</xsl:stylesheet>
