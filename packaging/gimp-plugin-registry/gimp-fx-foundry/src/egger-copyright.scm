;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Copyright script(v1.0a) for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: copyright, signature
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
(define (script-fu-Eg-Copyright InImage InLayer InText InFont InPercent InReserve InOpacity InColorPre InColor InPosition InBlur InFlatten)
;	
; Save history
;
	(gimp-image-undo-group-start InImage)
	(if (= (car (gimp-drawable-is-rgb InLayer)) FALSE ) (gimp-image-convert-rgb InImage))
;
	(let*	(
		(TheWidth (car (gimp-image-width InImage)))
		(TheHeight (car (gimp-image-height InImage)))
		(Old-FG-Color (car (gimp-context-get-foreground)))
		(FontSize (/ (* TheHeight InPercent) 100))
		(BlurSize (* FontSize 0.07))
		(text-size (gimp-text-get-extents-fontname InText FontSize PIXELS InFont))
		(text-width (car text-size))
		(text-height (cadr text-size))
		(reserve-width (/ (* TheWidth InReserve) 100))
		(reserve-height (/ (* TheHeight InReserve) 100))
		(text-x 0)
		(text-y 0)
		)
;
; Generate copyright text on the image
;
; Select the text color
;
		(cond
;
; Gray
;
			((= InColorPre 0) (gimp-context-set-foreground '(127 127 127)))
;
; Black
;
			((= InColorPre 1) (gimp-context-set-foreground '(15 15 15)))
;
; White
;
			((= InColorPre 2) (gimp-context-set-foreground '(240 240 240)))
;
; Selection
;
			((= InColorPre 3) (gimp-context-set-foreground InColor))
		)
;
;	Select position
;
		(cond 
;
;	Bottom right
;
			((= InPosition 0) 
				(begin
					(set! text-x (- TheWidth (+ text-width reserve-width)))
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Bottom left
;
			((= InPosition 1) 
				(begin
					(set! text-x reserve-width)
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Bottom center
;
			((= InPosition 2)
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Top right
;
			((= InPosition 3) 
				(begin
					(set! text-x (- TheWidth (+ text-width reserve-width)))
					(set! text-y reserve-height)
				)
			)
;
;	Top left
;
			((= InPosition 4) 
				(begin
					(set! text-x reserve-width)
					(set! text-y reserve-height)
				)
			)
;
;	Top center
;
			((= InPosition 5) 
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y reserve-height)
				)
			)
;
;	Image center
;
			((= InPosition 6) 
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y (/ (- TheHeight text-height) 2))
				)
			)
		)
;
		(let*	(
			(TextLayer (car (gimp-text-fontname InImage -1 text-x text-y InText -1 TRUE FontSize PIXELS InFont)))
			)
			(gimp-layer-set-opacity TextLayer InOpacity)
;
; Blur the text, if we need to
;
			(if (= InBlur TRUE) (plug-in-gauss TRUE InImage TextLayer BlurSize BlurSize TRUE))
;
; Flatten the image, if we need to
;
			(cond
				((= InFlatten TRUE) (gimp-image-merge-down InImage TextLayer CLIP-TO-IMAGE))
				((= InFlatten FALSE) 
					(begin
						(gimp-drawable-set-name TextLayer _"Copyright")
						(gimp-image-set-active-layer InImage InLayer)
					)
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
	"script-fu-Eg-Copyright"
	_"Copyright placer"
	_"Generate a copyright mark on an image"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"07.10.2007"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-STRING 	_"Copyright" "\302\251 M. Egger, 2009"
	SF-FONT 	_"Font" "Arial Bold"
	SF-ADJUSTMENT 	_"Text Height (Percent of image's height)" '(10 1.0 100 1.0 0 2 0)
	SF-ADJUSTMENT	_"Distance from border (Percent of image's height)" '(3 0.0 10 1.0 0 2 0)
	SF-ADJUSTMENT	_"Layer Opacity" '(60.0 1.0 100.0 1.0 0 2 0)
	SF-OPTION	_"Copyright color (preset)" '(_"Gray"
				_"Black"
				_"White"
				_"Color from selection")
	SF-COLOR 	_"Copyright color (selection)" '(220 220 220)
	SF-OPTION 	_"Copyright position" '(_"Bottom right"
				_"Bottom left"
				_"Bottom center"
				_"Top right"
				_"Top left"
				_"Top center"
				_"Image center")
	SF-TOGGLE 	_"Blur copyright" TRUE
	SF-TOGGLE	_"Flatten Image"	FALSE
)
;
(script-fu-menu-register "script-fu-Eg-Copyright"
			 "<Image>/Filters/Eg")
;
