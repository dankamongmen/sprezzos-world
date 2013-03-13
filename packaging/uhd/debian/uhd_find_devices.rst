=================
 uhd_find_devices
=================

---------------------------------------------
Universal Hardware Driver Discovery Utility
---------------------------------------------

DESCRIPTION
===========

Find UHD supporting Software Radio Peripherals attached by USB,
network or embedded configuration.

The UHD is the universal hardware driver for Ettus Research
products. The goal of the UHD is to provide a host driver and api for
current and future Ettus Research products. Users will be able to use
the UHD driver standalone or with 3rd party applications.

Hardware supporting UHD drivers includes the Universal Software Radio
Peripheral, or USRP, available in several models.

SYNOPSIS
========

  uhd_find_devices [OPTIONS]

OPTIONS
=======

--args arg	Device Address Arguments
--help 		This help information

IDENTIFYING USRPS
=================

Devices are addressed through key/value string pairs.
These string pairs can be used to narrow down the search for a specific device or group of devices.
Most UHD utility applications and examples have a \-\-args parameter that takes a device address;
where the device address is expressed as a delimited string.
See the documentation in types/device_addr.hpp for reference.

^^^^^^^^^^^^^^^^^^^^^^^^^
Common device identifiers
^^^^^^^^^^^^^^^^^^^^^^^^^

Every device has several ways of identifying it on the host system:

Identifier	Key	Notes

Serial		serial	globally unique identifier

Address		addr	unique identifier on a network

Name		name	optional user\-set identifier

Type		type	hardware series identifier


EXAMPLES
========

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Device discovery via command line
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Devices attached to your system can be discovered using the "uhd_find_devices" program.
The find devices program scans your system for supported devices and prints
out an enumerated list of discovered devices and their addresses.
The list of discovered devices can be narrowed down by specifying
device address args.

Device address arguments can be supplied to narrow the scope of the search.

 uhd_find_devices \-\-args="type=usrp1"

 \-\- OR \-\-

 uhd_find_devices \-\-args="serial=12345678"

Find all devices available to this system:

  uhd_find_devices

SEE ALSO
========

The UHD html documentation

file:///usr/share/doc/uhd-host/manual/html/index.html

or the GNU Radio project web site: http://gnuradio.org/

The programs uhd_usrp_probe(1) and the other UHD utilities
usrp2_card_burner.py, usrp_n2xx_net_burner.py found in
the /usr/lib/uhd/utils directory.

AUTHOR
======

This manual page was written by Maitland Bottoms for the Debian
project (but may be used by others).

COPYRIGHT
=========
Copyright (c) 2010 Ettus Research LLC

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
