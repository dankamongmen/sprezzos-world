;
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Resize image to layers for GIMP 2.6
; Original author: TheUncle2k
;
; Tags: resize, image, fit
;
; Author statement:
;
; Unlike "Fit canvas to layers" it resizes only the sides
; of the image that does not have any layer out of
; boundaries, keeping outside of the image what wasn't
; already visible.
; There is also the possibility to add a border to the
; resulting image. That border can be cleaned.
; It requires "Cut layers out of boundaries" to be fully
; functional otherwise the clean operation fails
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
     "script-fu-resize-image-to-layers"
    _"Resize image to layers..."
    _"Resize the image to fit to the layers, leaving outside of the image boundaries anything that was already out of boundaries. It allows to add a border."
     "TheUncle2k"
     "copyright 2009, TheUncle2k / http://theuncle2k.deviantart.com"
     "23rd December 2009"
     ""
     SF-IMAGE        "Input Image"            0
     SF-DRAWABLE     "Input Layer"            0
     SF-ADJUSTMENT  _"Border size (pixels)"   '(0 0 1000000 1 10 0 1)
     SF-TOGGLE      _"Clean the border"       FALSE
     )
     (script-fu-menu-register "script-fu-resize-image-to-layers" _"<Image>/FX-Foundry/Image Effects")


(define (script-fu-resize-image-to-layers inImg inDrawable inBufferSize inClean)

    (let* (
        (theImgWidth (car(gimp-image-width inImg)))
        (theImgHeight (car(gimp-image-height inImg)))
        (theIterations 0)
        (theLayerNum (car(gimp-image-get-layers inImg)))
        (theWorkingLayer 0)
        (theLayerWidth 0)
        (theLayerHeight 0)
        (theLayerOffsetX 0)
        (theLayerOffsetY 0)
        (theBorderLeft FALSE)
        (theBorderRight FALSE)
        (theBorderUp FALSE)
        (theBorderDown FALSE)
        (theMinPosX theImgWidth)
        (theMaxPosX 0)
        (theMinPosY theImgHeight)
        (theMaxPosY 0)
        (theNewImgWidth theImgWidth)
        (theNewImgHeight theImgHeight)
;        (theOriginalActive inDrawable)
        )

    ;begin undo group

    (gimp-image-undo-group-start inImg)

    ;check if it is necessary to remove the portions out of boundaries of the layers

    (if (equal? inClean TRUE)
        (if (defined? 'script-fu-cut-layers-out-of-boundaries)
            (begin

                ;call "Cut layers out of boundaries" to perform the removal

                (script-fu-cut-layers-out-of-boundaries inImg inDrawable TRUE)

                ;set the number of available layers to manage possible layers removal made by the
                ;script called above

                (set! theLayerNum (car(gimp-image-get-layers inImg)))

            );end begin

            (gimp-message "'Resize image to layers' requires 'Cut layers out of boundaries' script to perform the border cleaning. Please visit http://gimpfx-foundry.sourceforge.net/ or http://theuncle2k.deviantart.com/art/Cut-layers-out-of-boundaries-150425298")
        );end if
    );end if

    (begin

    (while (< theIterations theLayerNum)
        (set! theWorkingLayer (aref (cadr (gimp-image-get-layers inImg)) theIterations))
        (gimp-image-set-active-layer inImg theWorkingLayer)

        ;save the offsets and dimensions of the actual active layer

        (set! theLayerOffsetX (car(gimp-drawable-offsets theWorkingLayer)))
        (set! theLayerOffsetY (cadr(gimp-drawable-offsets theWorkingLayer)))
        (set! theLayerWidth (car(gimp-drawable-width theWorkingLayer)))
        (set! theLayerHeight (car(gimp-drawable-height theWorkingLayer)))

        ;check if the layer is out of the right boundary. If false, check if the
        ;right side of the actual active layer is the nearest to the image
        ;right boundary

        (cond ((> (+ theLayerWidth theLayerOffsetX) theImgWidth) (set! theBorderRight TRUE))
              ((> (+ theLayerWidth theLayerOffsetX) theMaxPosX) (set! theMaxPosX (+ theLayerWidth theLayerOffsetX)))
        );end cond

        ;check if the layer is out of the bottom boundary. If false, check if the
        ;bottom side of the actual active layer is the nearest to the image
        ;bottom boundary

        (cond ((> (+ theLayerHeight theLayerOffsetY) theImgHeight) (set! theBorderDown TRUE))
              ((> (+ theLayerHeight theLayerOffsetY) theMaxPosY) (set! theMaxPosY (+ theLayerHeight theLayerOffsetY)))
        );end cond

        ;check if the layer is out of the left boundary. If false, check if the
        ;left side of the actual active layer is the nearest to the image
        ;left boundary

        (cond ((< theLayerOffsetX 0) (set! theBorderLeft TRUE))
              ((< theLayerOffsetX theMinPosX) (set! theMinPosX theLayerOffsetX))
        );end cond

        ;check if the layer is out of the top boundary. If false, check if the
        ;top side of the actual active layer is the nearest to the image
        ;top boundary

        (cond ((< theLayerOffsetY 0) (set! theBorderUp TRUE))
              ((< theLayerOffsetY theMinPosY) (set! theMinPosY theLayerOffsetY))
        );end cond

        (set! theIterations (+ theIterations 1))
    );end while

    );end begin

    ;if a layer goes out of a side of the image, set the variables in order not to
    ;resize that side of the image

    (if (equal? theBorderLeft TRUE) (set! theMinPosX 0) (set! theMaxPosX (- theMaxPosX theMinPosX)))
    (if (equal? theBorderUp TRUE) (set! theMinPosY 0) (set! theMaxPosY (- theMaxPosY theMinPosY)))
    (if (equal? theBorderRight TRUE) (set! theMaxPosX (- theImgWidth theMinPosX)))
    (if (equal? theBorderDown TRUE) (set! theMaxPosY (- theImgHeight theMinPosY)))

    ;resize the image

    (gimp-image-resize inImg theMaxPosX theMaxPosY (- theMinPosX) (- theMinPosY))

    ;add the border. Default is 0, only useful if set by the user

    (set! theImgWidth (+ (car(gimp-image-width inImg)) inBufferSize inBufferSize))
    (set! theImgHeight (+ (car(gimp-image-height inImg)) inBufferSize inBufferSize))

    ;resize the image to add the border

    (gimp-image-resize inImg theImgWidth theImgHeight inBufferSize inBufferSize)


    (gimp-selection-none inImg)

;    (gimp-image-set-active-layer inImg theOriginalActive)

    ;end undo group

    (gimp-image-undo-group-end inImg)

    (gimp-displays-flush)

    (list inImg inDrawable)

    );end let

);end define
