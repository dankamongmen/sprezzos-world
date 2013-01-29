<?php

// Include base functions
include( '/usr/share/mediawiki-extensions/base/ExtensionFunctions.php' ) ;

$mw_debian_extensions_dir = "/etc/mediawiki-extensions/extensions-enabled";

// Including all enabled extensions.
if ( is_dir( $mw_debian_extensions_dir )
   &&
     $dh = opendir( $mw_debian_extensions_dir ) ) {
        while ( ( $file = readdir( $dh ) ) !== false ) {
            $absolute_file = $mw_debian_extensions_dir . DIRECTORY_SEPARATOR . $file ;
	    if ( preg_match( "/.php$/",$file ) && file_exists( $absolute_file ) &&
                 is_readable( $absolute_file ) ) {
                include_once( $absolute_file );
            }
        }
        closedir( $dh );
     }

?>
