;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Black & White script(V2.3) for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: photo, b&w, monochrome
;
; Author statement:
;
; You can find more about simulating BW at
; http://epaperpress.com/psphoto/
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; 18.11.2007 -  ; Added avaliabillity check for rgb scater plug-in
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
(define (script-fu-Eg-Black&White InImage InLayer InType InFlatten)
;	
; Save history			
;
	(gimp-image-undo-group-start InImage)
;
	(let*	(
		(BWLayer (car (gimp-layer-copy InLayer TRUE)))
		)
		(gimp-image-add-layer InImage BWLayer -1)
;
; Select conversion type
;
		(cond
;
; B&W (No Filter)
;
			((= InType 0) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.45 0.35 0.3 0 0 0 0 0 0))
;
; B&W with Red filter
;
			((= InType 1) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 1.0 0.15 -0.15 0 0 0 0 0 0))
;
; B&W with Green Filter
;
			((= InType 2) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.35 0.55 0.2 0 0 0 0 0 0))
;
; B&W with Blue Filter
;
			((= InType 3) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.15 0.15 0.8 0 0 0 0 0 0))
;
; B&W with Yellow filter
;
			((= InType 4) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.6 0.3 0.10 0 0 0 0 0 0))
;
; B&W with Orange filter
;
			((= InType 5) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.75 0.2 0.2 0 0 0 0 0 0))
;
; B&W with Yellow-Green filter
;
			((= InType 6) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.25 0.65 0.15 0 0 0 0 0 0))
;
; B&W (Lithographic film)
;
			((= InType 7)
				(begin
					(plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.4 0.3 0.3 0 0 0 0 0 0)
					(gimp-brightness-contrast BWLayer 0 110)
				)
			)
;
; B&W (Orthochromatic film)
;
			((= InType 8) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE -1.1 1.05 1.05 0 0 0 0 0 0))
;
; B&W (High Contrast)
;
			((= InType 9) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 1.4 -0.1 -0.3 0 0 0 0 0 0))
;
; B&W (Landscape)
;
			((= InType 10) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.1 1.3 -0.35 0 0 0 0 0 0))
;
; B&W (Portrait)
;
			((= InType 11) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.55 0.3 0.25 0 0 0 0 0 0))
;
; B&W (Gimp Desaturate)
;
			((= InType 12) (gimp-desaturate BWLayer))
		)
;
; Flatten the image, if we need to
;
		(cond
			((= InFlatten TRUE) (gimp-image-merge-down InImage BWLayer CLIP-TO-IMAGE))
			((= InFlatten FALSE)
				(begin
					(gimp-drawable-set-name BWLayer _"BlackWhite")
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
;
)
;
; Register the function with the GIMP
;
(script-fu-register
	"script-fu-Eg-Black&White"
	_"Black and White"
	_"Black and White conversions"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"25.10.2007"
	"RGB*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-OPTION 	_"Which B&W conversion" 
			'( 		
					_"B&W (No Filter)"
					_"B&W + RED filter"
					_"B&W + GREEN filter"
					_"B&W + BLUE filter"
					_"B&W + YELLOW filter"
					_"B&W + ORANGE filter"
					_"B&W + YELLOW-GREEN filter"
					_"B&W (Lithographic film)"
					_"B&W (Orthochromatic film)"
					_"B&W (High Contrast)"
					_"B&W (Landscape)"
					_"B&W (Portrait)"
					_"B&W (GIMP Desaturate)" 
			)
	SF-TOGGLE	_"Flatten Image"	FALSE
)
;
(script-fu-menu-register "script-fu-Eg-Black&White"
			 "<Image>/Filters/Eg")
;

