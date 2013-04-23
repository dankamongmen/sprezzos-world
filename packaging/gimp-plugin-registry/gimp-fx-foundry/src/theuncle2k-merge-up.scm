;
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Merge up for GIMP 2.6
; Original author: TheUncle2k
;
; Tags: merge
;
; Author statement:
;
; Merges the active layer with the
; one above. The resulting layer name will be the name of the layer
; that was above.
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

(script-fu-register "script-fu-merge-up"
    _"Merge up"
    _"Merge the active layer with the one above it"
     "TheUncle2k"
     "copyright 2009, TheUncle2k / http://theuncle2k.deviantart.com"
     "21st December 2009"
     ""
     SF-IMAGE       "Input Image"    0
     SF-DRAWABLE    "Input Layer"    0
    )
    (script-fu-menu-register "script-fu-merge-up" _"<Image>/FX-Foundry/Layer Effects")


(define (script-fu-merge-up inImg inDrawable)

    (let* (
        (theLayerNum
            (car (gimp-image-get-layers inImg))
        )
        (theWorkingLayer 0)
        (theIterations 0)
        (theLayerName 0)
        (theAboveLayerPos 0)
    );end var declaration


    ;save the ID of the layer above of the active one

    (set! theAboveLayerPos (- (car (gimp-image-get-layer-position inImg inDrawable)) 1)  )


    ;check if working on the top of the stack. If true, do nothing

    (if (not(negative? theAboveLayerPos))
        (begin

            ;begin undo group

            (gimp-image-undo-group-start inImg)

            ;set as active the layer right above

            (set! theWorkingLayer (aref (cadr (gimp-image-get-layers inImg)) theAboveLayerPos))
            (gimp-image-set-active-layer inImg theWorkingLayer)

            ;save the name of the actual active layer

            (set! theLayerName (car(gimp-drawable-get-name theWorkingLayer)))

            ;merge down

            (set! theWorkingLayer (car(gimp-image-merge-down inImg theWorkingLayer 0)))

            ;set the name of the merged layer as the name saved before

            (gimp-drawable-set-name theWorkingLayer theLayerName)

            ;end undo group

            (gimp-image-undo-group-end inImg)

        );end begin

    );end if

(gimp-displays-flush)

(list inImg theWorkingLayer)

    );end let

);end define