<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
	<head>
<style type="text/css" media="screen">
html { background: #000; margin: auto;}
.disclose { font-weight: bold; font-size: .75em; margin: auto; max-width: 30em; border: 1px solid #ccc;}
table { background-color: #ddd; margin: auto; border: 1px solid #000; overflow: hidden;}
caption { color: #fff; }
a:link { color: #D8D8D8; }
a:visited { color: #D8D8D8; }
th { border: 1px solid #000; }
td { overflow: hidden; }
.pn { max-width: 15em; font-weight: bold; border: 1px solid #000; background-color: lightgreen; }
.psprezz { max-width: 10em; font-weight: bold; background-color: #9999FF; }
.psprezzh { font-weight: bold; background-color: #9999FF; }
.pdebu { max-width: 10em; background-color: lightblue; }
.pdebt { max-width: 10em; background-color: #FFFFCC; }
.pur { max-width: 10em; font-weight: bold; background-color: #996600; }
.purh { font-weight: bold; background-color: #996600; }
.puq { max-width: 10em; font-weight: bold; background-color: salmon; }
.puqold { max-width: 10em; background-color: salmon; }
</style>
<script type="text/javascript">
<!--
var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-28599323-1']);
_gaq.push(['_setDomainName', 'sprezzos.com']);
_gaq.push(['_trackPageview']);
(function() {
	var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
	ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
	var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
})();
//-->
</script>
	</head>
  <body>

	  <center><a href="https://www.sprezzatech.com/wiki/index.php/SprezzOS_1"><img src="http://sprezzos.com/sprezzos-banner.png" alt="Download SprezzOS 1 today!" /></a></center>
<br /><table>
<caption>
	<h2>
		<xsl:value-of select="name[1]"/>
	</h2>
<center>
	<br />
	<br />
	<a href="https://www.sprezzatech.com/wiki/index.php/SprezzOS_1"><img src="http://sprezzos.com/sprezzos-banner.png" alt="Download SprezzOS 1 today!"/></a>
</center>
  </body>
  </html>
</xsl:template>

</xsl:stylesheet> 
