<?php
/* RSSReader 0.2.6 - a parser hook for MediaWiki
 * Copyright © 2008  Artem Kaznatcheev
 * Copyright © 2012  Thorsten Glaser
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

# Not a valid entry point, skip unless MEDIAWIKI is defined
if ( !defined('MEDIAWIKI') ) {
	exit( 1 );
}

$wgExtensionFunctions[] = 'efRSSReader';
$wgRSSReaderExtVersion = '0.2.6';

$wgExtensionCredits['parserhook'][] = array(
	'name' => 'RSS Reader',
	'version' => $wgRSSReaderExtVersion,
	'author' => 'Artem Kaznatcheev',
	'description' => 'Adds <tt>&lt;rss&gt;</tt> tag',
	'url' => 'http://www.mediawiki.org/wiki/Extension:RSS_Reader'
);

### Global Variables ###
//path to follow for server scripts
$egRSSReaderPath  = $wgScriptPath."/extensions/RSS_Reader";
$egCacheTime      = 3600; //default cache time in seconds
$egCacheTimeMin   = 1800; //minimum cache time in seconds
$egCacheTimeMax   = 7200; //maximum cache time in seconds
$egCache          = true; //boolean to determine if caching should be done
$egCacheDir       = dirname( __FILE__ ).'/cache/'; //directory of cache
//boolean to determine if links created should have rel="nofollow"
$egNoFollow       = false;
$egWidthMin       = 200;  //minimim width in pixels
$egWidthMax       = 800;  //maximum width in pixels

/**
 * select if cURLRSS or wikiRSS or lastRSS should be loaded
 * set rssType to the proper object type
 * each object of rssType must have the same interface
 */
if (file_exists(dirname(__FILE__)."/cURLRSS.php")){
  require_once(dirname(__FILE__)."/cURLRSS.php"); //loads cURLRSS.php
  $rssType = new cURLRSS; //set rssType to cURLRSS
} else if (file_exists(dirname( __FILE__ )."/wikiRSS.php")) {
  require_once(dirname( __FILE__ )."/wikiRSS.php"); //loads wikiRSS.php
  $rssType = new wikiRSS; //set rssType to wikiRSS
} else if (file_exists(dirname( __FILE__ )."/lastRSS.php")) {
  require_once(dirname( __FILE__ )."/lastRSS.php"); //loads lastRSS.php
  $rssType = new lastRSS; //set rssType to lastRSS
} else {
  trigger_error("RSSReader is not properly set-up:".
    "need cURLRSS, wikiRSS or lastRSS");
}

function efRSSReader() {
  global $wgParser;
  $wgParser->setHook( 'rss', 'efCreateRSSReader' );
}

function efCreateRSSReader($input, $argv, $parser){
  global $wgOut, $egRSSReaderPath, $egCacheTime, $egCacheTimeMin,
    $egCacheDir, $wgRSSReaderExtVersion,
    $egCacheTimeMax, $egCache, $rssType, $egNoFollow, $egWidthMin,
    $egWidthMax;

  // initialise args incase PHP set to strict
  foreach (array('number', 'width', 'title', 'time', 'desc') as $k) if (!isset($argv[$k])) $argv[$k] = false;

   //disable cache so feed is fetched everytime page is visited
  $parser->disableCache();
  if (!$input){ //if no input do nothing
  } else {
    $fields = explode("|",$input); //get the urls between the tags

    /*
     * Check if a "number=n" argument has been provided
     * if it has and is an int, then set the $number field to the proper
     * value else set $number field to zero (which means "all available")
     */
    if (!$argv["number"]){ //check if argument has been provided
      $number=0; //set default if no argument has been provided
    } else {
      //check if argument is an integer
      if ((int)$argv["number"]."" == $argv["number"]) {
        $number = $argv["number"]; //set $number field
      } else {
        $number=0; //if not an integer, then set default
      }
    }

    /*
     * Check if a "width=n" argument has been provided
     * if it has and is an int, then set the $width field to the proper
     * value else set $width field to zero (which means "no width")
     */
    if (!$argv["width"]){ //check if argument has been provided
      $width=0; //set default if no argument has been provided
    } else {
      //check if argument is an integer
      if ((int)$argv["width"]."" == $argv["width"]) {
        if (($argv["width"]>=$egWidthMin)&&($argv["width"]<=$egWidthMax)) {
          $width = $argv["width"]; //set $width field
        } else $width = 0; //if out of range, then set default
      } else {
        $width=0; //if not an integer, then set default
      }
    }

    /*
     * Check if a "time=n" argument has been provided
     * if it has and is between $egCacheTimeMin and $egCacheTimeMax
     * then set $cacheTime to the value
     * else set $cacheTime to $egCacheTime (the default CacheTime)
     */
    if (!$argv["time"]){ //check if argument has been provided
      //set default if no argument has been provided
      $cacheTime = $egCacheTime;
    } else {
      //check if argument is an integer
      if ((int)$argv["time"]."" == $argv["time"]) {
        //check if argument is in range
        if (($argv["time"]>=$egCacheTimeMin)
          &&($argv["time"]<=$egCacheTimeMax)) {
          $cacheTime = $argv["time"]; //set $cacheTime field
        } else {
          //set default if argument is outside range
          $cacheTime = $egCacheTime;
        }
      } else {
        $cacheTime=$egCacheTime; //set default if not an integer
      }
    }

    /* Check if a "desc=off" parameter has been provided and set desc */
    $desc = true; //set the default
    if ($argv["desc"]){ //check if argument has been provided
      if ($argv["desc"]=="off") $desc = false;
    }

    /* Check if "title=off" parameter was provided and set dispTitle */
    $dispTitle = true; //set the default
    if ($argv["title"]){ //check if argument has been provided
      if ($argv["title"]=="off") $dispTitle = false;
    }

    $wgOut->addLink(array(
	'rel' => 'stylesheet',
	'type' => 'text/css',
	'href' => "$egRSSReaderPath/RSSReader.css?$wgRSSReaderExtVersion",
    ));

    if (!$width) {
      $output = '
        <table class="RSSMainBody" style="background:inherit;">
        <tr>
      ';
    } else {
      $output = '
        <table class="RSSMainBody" style="background:inherit; float:right; width:'.$width.'">
        <tr>
      ';
    }

    //calculates the desired width for each feed and makes sure it is int
    $width = intval(100/sizeof($fields) - 5);

    // Create cURLRSS or wikiRSS or lastRSS object
    $rss = new $rssType; //initialize an object of rssType
    // Set public variables
    if (($rssType instanceof lastRSS) && $egCache) {
      $rss->cache_dir = $egCacheDir;
    }
    $rss->cache = $egCache; //cache attribute
    $rss->cache_time = $cacheTime; //refresh time in seconds
    $rss->date_format = 'l';

    foreach ($fields as $field) {
      //table cell that contains a single RSS feed
      $output .= '<td valign="top" style="width: '.$width.'%;">';
      if (($rssArray = $rss->get($field)) && (isset($rssArray['link']) || isset($rssArray['title']) || isset($rssArray['description']))) {
        if ($dispTitle) { //check if title should be displayed
          $output .=
            '<div class="RSSReader-head">'.
              '<h3><a href="'.
              $rssArray['link'].
              '"';
          //decide if nofollow is needed
          if ($egNoFollow) $output .= 'rel="nofollow"';
          $output .= '>'.
            $rssArray['title'].
            '</a></h3>';
          //decide if description is required
          if ($desc) $output .= $rssArray['description'];
          $output .= '</div>';
        }

        /* Outputs the items */
        $text = isset($argv["text"]);
        if (!$text) $output .= "<ul class='rss'>";
        $i = 0; //counter for number of items already displayed
        foreach ($rssArray['items'] as $item){
          $output .= $text ? "<div class='rss'><h3>" : "<li>";
          $output .= '<a href="'.$item['link'].'" ';
          //decide if nofollow is needed
          if ($egNoFollow) $output .= 'rel="nofollow"';
          $item_title=preg_replace("|\[rsslist:.+?\]|", "",
            htmlspecialchars(html_entity_decode(html_entity_decode($item['title'],
            ENT_QUOTES, "UTF-8"), ENT_QUOTES, "UTF-8"), ENT_QUOTES, "UTF-8"));
          $output .= '>'.$item_title.'</a>';
          if ($text) {
            $desc = preg_replace("|\[rsslist:.+?\]|", "",
              Sanitizer::removeHTMLtags(html_entity_decode($item['description'],
              ENT_QUOTES, "UTF-8"), null, array(),
              array('a', /* does not work */ 'img')));
            $output .= "</h3>\n$desc</div>\n";
          } else $output .= "</li>\n";
          /*if reached the number of desired display items stop working on
           *displaying more items*/
          if (++$i == $number) break;
          } //close foreach items
        if (!$text) $output .= "</ul>\n";
      } else { //output error if not possible to fetch RSS
        $output .= "Error: It's not possible to get $field...";
      }
      $output .= '</td>';
    } //close foreach fields
    $output .= "</tr></table>";
  } //close main "else"
  return $output;
}
?>
