;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Shadow Recovery script for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
;
; Tags: photo, exposure
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; august 2007 - fixed for gimp 2.4
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
(define (script-fu-Eg-ShadowRecovery InImage InLayer InMethod InOpacity InFlatten)
;	
; Save history			
;
	(gimp-image-undo-group-start InImage)
;
	(let*	(
		(CopyLayer (car (gimp-layer-copy InLayer TRUE)))
		(ShadowLayer (car (gimp-layer-copy InLayer TRUE)))
		)
;
; Create new layer and add it to the image
;
		(gimp-image-add-layer InImage CopyLayer -1)
		(gimp-layer-set-mode CopyLayer ADDITION-MODE)
		(gimp-layer-set-opacity CopyLayer InOpacity)
		(gimp-image-add-layer InImage ShadowLayer -1)
;
		(if (= (car (gimp-drawable-is-gray ShadowLayer)) FALSE ) (gimp-desaturate ShadowLayer))
		(gimp-invert ShadowLayer)
		(let*	(
			(CopyMask (car (gimp-layer-create-mask CopyLayer ADD-WHITE-MASK)))
			(ShadowMask (car (gimp-layer-create-mask ShadowLayer ADD-WHITE-MASK)))
			)
			(gimp-layer-add-mask CopyLayer CopyMask)
			(gimp-layer-add-mask ShadowLayer ShadowMask)
			(gimp-selection-all InImage)
			(gimp-edit-copy ShadowLayer)
			(gimp-floating-sel-anchor (car (gimp-edit-paste CopyMask TRUE)))
			(gimp-floating-sel-anchor (car (gimp-edit-paste ShadowMask TRUE)))
			(gimp-layer-set-edit-mask CopyLayer FALSE)
			(gimp-layer-set-edit-mask ShadowLayer FALSE)
		)
		(gimp-layer-set-mode ShadowLayer OVERLAY-MODE)
		(gimp-layer-set-opacity ShadowLayer InOpacity)
		(if (= InMethod 0) (gimp-image-remove-layer InImage CopyLayer))
;
; Flatten the image, if we need to
;
		(cond
			((= InFlatten TRUE)
				(begin
					(if (= InMethod 1) (gimp-image-merge-down InImage CopyLayer CLIP-TO-IMAGE))
					(gimp-image-merge-down InImage ShadowLayer CLIP-TO-IMAGE)
				)
			)
			((= InFlatten FALSE) 
				(begin
					(if (= InMethod 1) (gimp-drawable-set-name CopyLayer _"Shadowfree strong"))
					(gimp-drawable-set-name ShadowLayer _"Shadowfree normal")
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
(script-fu-register
	"script-fu-Eg-ShadowRecovery"
	_"Shadow _Recovery"
	_"Lighten-up Shadows"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"20.10.2007"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-OPTION 	_"Shadow Recovery Method" 
			'( 
						_"Normal"
						_"Strong"
			)
	SF-ADJUSTMENT	_"Layer Opacity"	'(60.0 1.0 100.0 1.0 0 2 0)
	SF-TOGGLE	_"Flatten Image"	FALSE
)
;
(script-fu-menu-register "script-fu-Eg-ShadowRecovery"
			 "<Image>/Filters/Eg")
;


