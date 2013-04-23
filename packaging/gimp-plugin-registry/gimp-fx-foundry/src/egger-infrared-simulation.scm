;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Infrared, V2.1 for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: photo, infrared
;
; Author statement:
;
; You can find more about simulating IR at
; http://www.gimpguru.org/Tutorials/SimulatedInfrared/
; http://epaperpress.com/psphoto/
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

; Define the function
;
(define (script-fu-Eg-Infrared InImage InLayer InOpacity InContrast InBlur InFlatten)
;
; Save history			
;
	(gimp-image-undo-group-start InImage)
;	
	(let*	(
		(CopyLayer (car (gimp-layer-copy InLayer TRUE)))
		(IRLayer (car (gimp-layer-copy InLayer TRUE)))
		(IRLayer2 (car (gimp-layer-copy InLayer TRUE)))
		(IRLayer3 (car (gimp-layer-copy InLayer TRUE)))
		)
		(gimp-image-add-layer InImage CopyLayer -1)
;
		(gimp-image-add-layer InImage IRLayer3 -1)
		(gimp-layer-set-mode IRLayer3 SCREEN-MODE)
		(gimp-layer-set-opacity IRLayer3 InContrast)
		(gimp-image-add-layer InImage IRLayer2 -1)
		(gimp-layer-set-mode IRLayer2 BURN-MODE)
		(gimp-layer-set-opacity IRLayer2 InContrast)
		(gimp-image-add-layer InImage IRLayer -1)
		(gimp-layer-set-mode IRLayer SCREEN-MODE)
		(gimp-layer-set-opacity IRLayer InOpacity)
;
; Dreamy IR
;
		(gimp-desaturate CopyLayer)
		(gimp-desaturate IRLayer3)
		(plug-in-colors-channel-mixer TRUE InImage IRLayer2 TRUE 1.0 1.0 -1.0 0 0 0 0 0 0)
		(plug-in-colors-channel-mixer TRUE InImage IRLayer TRUE -.3 2.0 -.7 0 0 0 0 0 0)
		(gimp-brightness-contrast IRLayer -25 70)
;
; Blur the IR layer, if we need to
;
		(if (= InBlur TRUE) (plug-in-gauss TRUE InImage IRLayer 4.0 4.0 TRUE))
;
; Flatten the image, if we need to
;
		(cond
			((= InFlatten TRUE) 
				(begin
					(gimp-image-merge-down InImage CopyLayer CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage IRLayer3 CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage IRLayer2 CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage IRLayer CLIP-TO-IMAGE)
				)
			)
;
			((= InFlatten FALSE) 
				(begin
					(gimp-drawable-set-name IRLayer _"Infrared (Green)")
					(gimp-drawable-set-name IRLayer2 _"Infrared (Sky Contrast)")
					(gimp-drawable-set-name IRLayer3 _"Infrared (Green Glow)")
					(gimp-drawable-set-name CopyLayer _"BW-Copy")
					(gimp-image-set-active-layer InImage InLayer)
				)
			)
		)
	)
;
; Finish work
;
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
)
;
(script-fu-register
	"script-fu-Eg-Infrared"
	_"_Infrared Simulation"
	_"Simulate a B&W Infra-red image"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"07.10.2007"
	"RGB*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-ADJUSTMENT	_"IR Layer Opacity" '(80.0 1.0 100.0 1.0 0 2 0)
	SF-ADJUSTMENT	_"Contrast/Glow Layer Opacity" '(50.0 1.0 100.0 1.0 0 2 0)
	SF-TOGGLE	_"Blur Image"	FALSE
	SF-TOGGLE	_"Flatten Image"	FALSE
)
;
(script-fu-menu-register "script-fu-Eg-Infrared"
			 "<Image>/Filters/Eg")
;

