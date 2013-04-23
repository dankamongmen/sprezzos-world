#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Version 20090321
# Eckhard M. Jäger <Bart@neeneenee.de>
# Copyright @ 2009 area42 - Agentur & Systempartner
# http://www.area42.de
#   
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License V2 as published by the Free Software
# Foundation;

from gimpfu import *
import os, string, sys
import os.path

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)

def python_fu_cmyk_tiff_2_pdf(active_image, active_layer, this_file1, this_file2, this_compress, this_dpi, delete_files, start_viewer):
	if this_file1 == None and this_file2 == None:
		return
	do_compress = "-j -q 95"
	do_multi = 0
	do_delete = 0
	do_win = ""
	do_winpath = ""
	title1 = ""
	title2 = ""
	this_cmd = "rm"
	
	# print gimp.locale_directory
	
	# Determine the OS
	if os.sep == "\\":
		do_win = ".exe"
		do_winpath = "\%programfiles\%\\bin\\"
		this_cmd = "del"
	
 	if this_file1 != None:
 		do_multi += 1
		if os.path.isfile(this_file1):
			output_name1 = os.path.splitext(this_file1)
			title1 = os.path.basename(output_name1[0])
			dir1 = os.path.dirname(this_file1)
 	
 	if this_file2 != None:
 		do_multi += 1
		if os.path.isfile(this_file2):
			output_name2 = os.path.splitext(this_file2)
			title2 = " - %s" % os.path.basename(output_name2[0])

 	title = "%s%s" % (title1, title2)
 	
	if do_multi > 1:
		all_files = "%s%s%s-%s" % (dir1, os.sep, os.path.basename(output_name1[0]), os.path.basename(output_name2[0]))
		command = "%stiffcp%s \"%s\" \"%s\" \"%s.tif\"" % (do_winpath, do_win, this_file1, this_file2, all_files)
		# Multi Tiff
		# print "**********************************************"
		# print "Multi Tiff"
		# print command
		os.system(command)
	else:
		all_files = str(output_name1[0])
		
	if this_compress == "2":
		do_compress = "-z"
	elif this_compress == "4":
		do_compress = "-d"
		
	# Tiff 2 PDF	
	# print "**********************************************"
	# print "Tiff 2 PDF"
	command = "%stiff2pdf%s %s -c \"GIMP CMYK Tiff 2 PDF\" -f -x %s -y %s -t \"%s\" -o \"%s.pdf\" \"%s.tif\"" % (do_winpath, do_win, do_compress, str(this_dpi), str(this_dpi), title, all_files, all_files)
	# print command		
	os.system(command)
	do_delete = 1
	
	# PDF Viewer
	if start_viewer:
		if os.sep == "\\":
			try:
				command = "acrobat.exe \"%s.pdf\"" % all_files
				os.system(command)
			except OSError:
				command = "acrord32.exe \"%s.pdf\" &" % all_files
				os.system(command)	
		else:
			try:
				command = "acroread \"%s.pdf\" &" % all_files
				os.system(command)
			except OSError:
				command = "evince \"%s.pdf\" &" % all_files
				os.system(command)
	
	# Multi Tiff Clean
	if do_multi > 1:
		# print "Multi Tiff Clean"
		command = "%s \"%s.tif\" &" % (this_cmd, all_files)
		os.system(command)
		
	# Image Tiff Clean	
	if delete_files and do_delete:
		# print " Image Tiff Clean	"
		if os.path.isfile(this_file1):
			command = "%s \"%s\" &" % (this_cmd, this_file1)
			os.system(command)
				
		if os.path.isfile(this_file2):
			command = "%s \"%s\" &" % (this_cmd, this_file2)
			os.system(command)		

register(
	"python-fu-cmyk-tiff-2-pdf",
	"CMYK Tiff 2 PDF 20090321\nCreating a CMYK PDF for prepress from a CMYK Tiff image. \nPresented by area42 - Agentur & Systempartner: \nwww.area42.de",
	"CMYK Tiff 2 PDF 20090321\nCreating a CMYK PDF for prepress from a CMYK Tiff image. \nPresented by area42 - Agentur & Systempartner: \nwww.area42.de",
	"Eckhard M. Jaeger, http://my.opera.com/area42/blog/",
	"GPL V2 License",
	"2009",
	"<Image>/Image/Separate/CMYK Tiff 2 PDF...",
	"",
	[
		(PF_FILENAME,	"this_file1", _("CMYK Tiff Images"), ""),
		(PF_FILENAME,	"this_file2", _(" "), ""),
		(PF_RADIO, 		"this_compress",  _("Compression"), "0", (("JPEG 95% Quality","0"), ("Zip Full Quality","2"), ("No Compression","4"))),
		(PF_SPINNER,	"this_dpi", _("Resolution (dpi)"), 300, (0,2400,50)), 
		(PF_TOGGLE,		"delete_files", _("Delete Images\nafter PDF creation"), False),
		(PF_TOGGLE,		"start_viewer", _("Start PDF Viewer\nafter export"), True),
	],
	[],
	python_fu_cmyk_tiff_2_pdf,
	domain=("gimp20-python", gimp.locale_directory))

main()
