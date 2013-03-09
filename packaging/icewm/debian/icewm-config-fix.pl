#!/usr/bin/perl
# © Eduard Bloch <blade@debian.org>, 2003
#

$menufile = $ENV{"HOME"}."/.icewm/menu";
$updateMenuMsg = "updating... ";

$menufile = $ARGV[0] if( -f $ARGV[0] );

if(! -f $menufile) 
{
   print "No personal menu file, fine...\n";
}
else {
   print "Testing $menufile... ";
}
open(menu, "+<$menufile");
while(<menu>) {
   if($_ =~ /folder.*icewm-menu-gnome/) {
      if(defined $updateMenuMsg) {
         push(menuContents, "
menuprog \"Gnome\" folder icewm-menu-gnome2 --list /usr/share/gnome/vfolders
menuprog \"KDE\" folder icewm-menu-gnome2 --list /usr/share/applnk
");

         print $updateMenuMsg;
         undef $updateMenuMsg;
      }
   }
   else {
      push(menuContents, $_);
   }
}

# fix very old config without gnome menu entry
if(defined $updateMenuMsg) {
   push(menuContents, "
menufile Toolbar folder toolbar
separator
menuprog \"Gnome\" folder icewm-menu-gnome2 --list /usr/share/gnome/vfolders
menuprog \"KDE\" folder icewm-menu-gnome2 --list /usr/share/applnk
");

   print $updateMenuMsg;
   undef $updateMenuMsg;
}
print "done.\n";

seek(menu, 0, 0);
print menu @menuContents;
