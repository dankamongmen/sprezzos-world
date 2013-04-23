;
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Autocrop all layers for GIMP 2.6
; Original author: TheUncle2k
;
; Tags: autocrop, all
;
; Author statement:
;
; A simple script that applies 'autocrop layer' to
; all the layers available on the image.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(script-fu-register
     "script-fu-autocrop-all-layers"
    _"Autocrop all layers"
    _"Apply autocrop layer to all available layers"
     "TheUncle2k"
     "copyright 2009, TheUncle2k / http://theuncle2k.deviantart.com"
     "21st December 2009"
     ""
     SF-IMAGE       "Input Image"    0
     SF-DRAWABLE    "Input Layer"    0
     )
    (script-fu-menu-register "script-fu-autocrop-all-layers" _"<Image>/FX-Foundry/Multi-Layer Tools")


(define (script-fu-autocrop-all-layers inImg inDrawable)

    (let*    (
        (theLayerNum
        (car (gimp-image-get-layers inImg))
        )
        (theWorkingLayer 0)
        (theIterations 0)
        (theOriginalActive inDrawable)
        )

    ;begin undo group

    (gimp-image-undo-group-start inImg)

    ;cycle through all layers and apply "autocrop layer"

    (while (< theIterations theLayerNum)
        (set! theWorkingLayer (aref (cadr (gimp-image-get-layers inImg)) theIterations))
        (gimp-image-set-active-layer inImg theWorkingLayer)
        (plug-in-autocrop-layer RUN-NONINTERACTIVE inImg theWorkingLayer)
        (set! theIterations (+ theIterations 1))
    )

    (gimp-image-set-active-layer inImg theOriginalActive)

    ;end undo group

    (gimp-image-undo-group-end inImg)

    (gimp-displays-flush)

    (list inImg inDrawable)

    );end let

);end define