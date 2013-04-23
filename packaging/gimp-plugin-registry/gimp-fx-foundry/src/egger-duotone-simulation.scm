;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Duotone/Tritone Simulation, V2.2 for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: photo, duotone
;
; Author statement:
;
; You can find more about toning at
; http://www.gimp.org/tutorials/Sepia_Toning/
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
(define (script-fu-Eg-DuotoneSimulation InImage InLayer InType InColor InFlatten)
;	
; Save history			
;
	(gimp-image-undo-group-start InImage)
	(if (= (car (gimp-drawable-is-rgb InLayer)) FALSE ) (gimp-image-convert-rgb InImage))
;
	(let*	(
		(TintLayer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage)) RGBA-IMAGE _"Tint" 70.0 OVERLAY-MODE)))
		(Old-FG-Color (car (gimp-context-get-foreground)))
		)
;
; Select the tint color
;
		(cond
;
; Cyano
;
			((= InType 0) (gimp-context-set-foreground '(20 120 180)))
;
; Gold
;
			((= InType 1) 
				(begin	
					(gimp-context-set-foreground '(250 145 55))
					(gimp-layer-set-opacity TintLayer 55)
				)
			)
;
; Palladium (Yellow)
;
			((= InType 2) (gimp-context-set-foreground '(143 153 69)))
;
; Selenium (Magenta)
;
			((= InType 3) 
				(begin				
					(gimp-context-set-foreground '(158 79 104))
					(gimp-layer-set-opacity TintLayer 60)
				)
			)
;
; Sepia
;
			((= InType 4) (gimp-context-set-foreground '(215 175 110)))
;
; Silver
;
			((= InType 5) 
				(begin
					(gimp-context-set-foreground '(92 153 154))
					(gimp-layer-set-opacity TintLayer 55)
				)
			)
;
; Cooler
;
			((= InType 6) (gimp-context-set-foreground '(150 150 175)))
;
; Warmer
;
			((= InType 7) (gimp-context-set-foreground '(180 170 150)))
;
; Selection
;
			((= InType 8) (gimp-context-set-foreground InColor))
		)
;
; Fill the layer with the tint
;
		(gimp-drawable-fill TintLayer FOREGROUND-FILL)
;
; Add the layer to the image
;
		(gimp-image-add-layer InImage TintLayer -1)
;
; Create a mask for the new layer
;
		(let*	(
			(TintMask (car (gimp-layer-create-mask TintLayer ADD-WHITE-MASK)))
			)
			(gimp-layer-add-mask TintLayer TintMask)
			(gimp-selection-all InImage)
			(gimp-edit-copy InLayer)
			(gimp-floating-sel-anchor (car (gimp-edit-paste TintMask TRUE)))
			(gimp-invert TintMask)
			(gimp-layer-set-edit-mask TintLayer FALSE)
		)
;
; Flatten the image, if we need to
;
		(cond
			((= InFlatten TRUE) (gimp-image-merge-down InImage TintLayer CLIP-TO-IMAGE))
			((= InFlatten FALSE)
				(begin
					(gimp-image-set-active-layer InImage InLayer)
				)
			)
		)
		(gimp-context-set-foreground Old-FG-Color)
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
	"script-fu-Eg-DuotoneSimulation"
	_"_Duotone Simulation"
	_"Simulate Duotones in GIMP"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"20.10.2007"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-OPTION	_"Select Tone"
			'( 
					_"Cyano"
					_"Gold"
					_"Palladium (Yellow)"
					_"Selenium (Magenta)"
					_"Sepia"
					_"Silver"
					_"Cooler look"
					_"Warmer look"
					_"Color from selection"
			)
	SF-COLOR	_"Select Color"	'(215  175 110)
	SF-TOGGLE	_"Flatten Image"	FALSE
)
;
(script-fu-menu-register "script-fu-Eg-DuotoneSimulation"
			 "<Image>/Filters/Eg")
;

