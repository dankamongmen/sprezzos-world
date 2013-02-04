About the S/390 installation CD
===============================

It is possible to "boot" the installation system off this CD using
the files provided in the /boot directory.

Note that the /boot/d390oco.* files are only provided as an example
as Debian cannot ship the object-code-only-modules-ramdisk (oco.bin).

Although you can boot the installer from this CD, the installation
itself is *not* actually done from the CD. Once the initrd is loaded,
the installer will ask you to configure your network connection and
uses the network-console component to allow you to continue the
installation over SSH. The rest of the installation is done over the
network: all installer components and Debian packages are retrieved
from a mirror.


Tip for users of the Hercules emulator
--------------------------------------
If you want to ipl the installer off this CD, mount it on the host
system (e.g. on /media/cdrom) and enter the following in the Hercules
management console:
   ipl /media/cdrom/boot/d390.ins
