;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Infrared Channelswitch for GIMP 2.6
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2008, Bern, Switzerland
;
; Tags: photo, infrared
;
; Author statement:
;
; Autolevel colors (aka trivial Auto Whitebalance) and switch Red and Blue channel in a color IR image
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.


;
; Define the function
;
(define (script-fu-Eg-InfraredChannelswitch InImage InLayer InAuto InFlatten)
;
; Save history
;
	(gimp-image-undo-group-start InImage)
;
	(let*	(
			(SwitchLayer (car (gimp-layer-copy InLayer TRUE)))
		)
		(gimp-image-add-layer InImage SwitchLayer -1)
;
; Autolevel the colors, if we need to
;
		(cond
			((= InAuto TRUE) (gimp-levels-stretch SwitchLayer))
		)
;
		(let*	(
				(RGBImage (car (plug-in-decompose TRUE InImage SwitchLayer "RGB" TRUE)))
				(RGBLayer (cadr (gimp-image-get-layers RGBImage)))
				(CompImage (car (plug-in-drawable-compose TRUE RGBImage (aref RGBLayer 2) (aref RGBLayer 1) (aref RGBLayer 0) -1 "RGB")))
				(CompLayer (cadr (gimp-image-get-layers CompImage)))
			)
			(gimp-selection-all CompImage)
			(gimp-edit-copy (aref CompLayer 0))
			(gimp-floating-sel-anchor (car (gimp-edit-paste SwitchLayer FALSE)))
			(gimp-image-delete CompImage)
			(gimp-image-delete RGBImage)
;
; Flatten the image, if we need to
;
			(cond
				((= InFlatten TRUE) (gimp-image-merge-down InImage SwitchLayer CLIP-TO-IMAGE))
				((= InFlatten FALSE)
					(begin
						(gimp-drawable-set-name SwitchLayer _"Switched Channels")
						(gimp-image-set-active-layer InImage InLayer)
					)
				)
			)
		)
	)
;
; Finish work
;
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
;
)
;
(script-fu-register
	"script-fu-Eg-InfraredChannelswitch"
	_"Infrared _Channelswitch"
	_"Switch Red and Blue channels in color IR images, autolevel colors"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"05.03.2008"
	"RGB*"
	SF-IMAGE	"The Image"		0
	SF-DRAWABLE	"The Layer"		0
	SF-TOGGLE	_"Autolevel Image First"	TRUE
	SF-TOGGLE	_"Flatten Image"		FALSE
)
;
(script-fu-menu-register "script-fu-Eg-InfraredChannelswitch"
			 "<Image>/Filters/Eg")
;
