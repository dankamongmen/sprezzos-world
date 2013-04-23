;
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Cut layers out of boundaries for GIMP 2.6
; Original author: TheUncle2k
;
; Tags: cut, crop
;
; Author statement:
;
; Cuts any portion of the active
; layer placed out of the image boundaries. The action
; can also be performed on all the available layers at once.
; If the layer is completely out of the image, it will be deleted.
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
     "script-fu-cut-layers-out-of-boundaries"
    _"Cut layers out of boundaries..."
    _"Cut the portion of the active layer or of all layers that are outside of the image boundaries"
     "TheUncle2k"
     "copyright 2009, TheUncle2k / http://theuncle2k.deviantart.com"
     "21st December 2009"
     ""
     SF-IMAGE     "Input Image"    0
     SF-DRAWABLE  "Input Layer"    0
     SF-TOGGLE   _"Cut all"        FALSE
    )
    (script-fu-menu-register "script-fu-cut-layers-out-of-boundaries" _"<Image>/FX-Foundry/Layer Effects")


(define (script-fu-cut-layers-out-of-boundaries inImg inDrawable inCutAll)

    (let* (
        (theImgWidth (car (gimp-image-width inImg)))
        (theImgHeight (car (gimp-image-height inImg)))
        (theIterations 0)
        (theLayerNum 1)
        (theWorkingLayer 0)
        (theLayerWidth 0)
        (theLayerHeight 0)
        (theLayerOffsetX 0)
        (theLayerOffsetY 0)
        (theOutRight 0)
        (theOutDown 0)
        (theDeleted 0)
    )

    ;begin undo group

    (gimp-image-undo-group-start inImg)

    (set! theWorkingLayer inDrawable)

    ;verify if cycling through all layers is needed

    (if (equal? inCutAll TRUE)

        ;save how many layers are available

        (set! theLayerNum (car(gimp-image-get-layers inImg)))
    )


    (while (< theIterations theLayerNum)


        ;if working on all layers, set the next layer available, starting from the first
        ;in the stack

        (if (equal? inCutAll TRUE)
            (begin
                (set! theWorkingLayer (aref (cadr (gimp-image-get-layers inImg)) theIterations))
                (gimp-image-set-active-layer inImg theWorkingLayer)
            )
        )


        ;save the position and dimensions of the layer

        (set! theLayerOffsetX (car(gimp-drawable-offsets theWorkingLayer)))
        (set! theLayerOffsetY (cadr(gimp-drawable-offsets theWorkingLayer)))
        (set! theLayerWidth (car(gimp-drawable-width theWorkingLayer)))
        (set! theLayerHeight (car(gimp-drawable-height theWorkingLayer)))


        ;check if the layer is out of the right boundary and save the amount

        (if (> (+ theLayerWidth theLayerOffsetX) theImgWidth)
                (set! theOutRight (- (+ theLayerWidth theLayerOffsetX) theImgWidth))
        )


        ;check if the layer is out of the bottom boundary and save the amount

        (if (> (+ theLayerHeight theLayerOffsetY) theImgHeight)
            (set! theOutDown (- (+ theLayerHeight theLayerOffsetY) theImgHeight))
        )


        ;if the layer is completely out of the left boundary delete it
        
        (cond ((< theLayerOffsetX (- (- theLayerWidth 1)))
              (begin
                (gimp-image-remove-layer inImg theWorkingLayer)
                (set! theDeleted 1)
              );end begin
              )
              ((< theLayerOffsetX 0)
              (begin

                ;resize the layer to fit the left boundary of the image.

                (gimp-layer-resize theWorkingLayer
                           (- theLayerWidth (- theLayerOffsetX))
                           theLayerHeight
                           theLayerOffsetX
                           0)

                ;update the dimensions of the layer

                (set! theLayerWidth (car (gimp-drawable-width theWorkingLayer)))
                (set! theLayerHeight (car (gimp-drawable-height theWorkingLayer)))
            );end begin
            )
        );end cond

        ;work on the layer if it has not been already deleted

        (if (= theDeleted 0)

        ;if the layer is completely out of the top boundary delete it

        (cond ((< theLayerOffsetY (- (- theLayerHeight 1)))
            (begin
                (gimp-image-remove-layer inImg theWorkingLayer)
                (set! theDeleted 1)
            );end begin
              )
              ((< theLayerOffsetY 0)
            (begin

                ;resize the layer to fit the top boundary of the image.

                (gimp-layer-resize theWorkingLayer
                           theLayerWidth
                           (- theLayerHeight (- theLayerOffsetY))
                           0
                           theLayerOffsetY)

                ;update the dimensions of the layer

                (set! theLayerWidth (car (gimp-drawable-width theWorkingLayer)))
                (set! theLayerHeight (car (gimp-drawable-height theWorkingLayer)))
            );end begin
            )
        );end cond
        );end if

        ;work on the layer if it has not been already deleted

        (if (= theDeleted 0)

        ;if the layer is completely out of the right boundary delete it

        (cond ((> theOutRight (- theLayerWidth 1))
            (begin
                (gimp-image-remove-layer inImg theWorkingLayer)
                (set! theDeleted 1)
            );end begin
              )
              ((> theOutRight 0)
            (begin

                ;resize the layer to fit the right boundary of the image.

                (gimp-layer-resize theWorkingLayer
                           (- theLayerWidth theOutRight)
                           theLayerHeight
                           0
                           0)

                ;update the dimensions of the layer

                (set! theLayerWidth (car(gimp-drawable-width theWorkingLayer)))
                (set! theLayerHeight (car(gimp-drawable-height theWorkingLayer)))
            );end begin
            )
        );end cond
        );end if

        ;work on the layer if it has not been already deleted

        (if (= theDeleted 0)

        ;if the layer is completely out of the bottom boundary delete it

        (cond ((> theOutDown (- theLayerHeight 1)) 
            (begin
                (gimp-image-remove-layer inImg theWorkingLayer)
                (set! theDeleted 1)
            );end begin
              )
              ((> theOutDown 0)
            (begin

                ;resize the layer to fit the bottom boundary of the image.

                (gimp-layer-resize theWorkingLayer
                           theLayerWidth
                           (- theLayerHeight theOutDown)
                           0
                           0)

                ;update the dimensions of the layer

                (set! theLayerWidth (car(gimp-drawable-width theWorkingLayer)))
                (set! theLayerHeight (car(gimp-drawable-height theWorkingLayer)))
            );end begin
            )
        );end cond
        );end if

        (set! theOutRight 0)
        (set! theOutDown 0)

        ;do not increase the counter if the layer has been deleted to consider the layer number variation
        ;caused by the removal. For the same reason reduce the number of layers by one.

        (if (= theDeleted 0)
        (set! theIterations (+ theIterations 1))
	(set! theLayerNum (- theLayerNum 1))
        )

        (set! theDeleted 0)

    );end while

    ;end undo group

    (gimp-image-undo-group-end inImg)

    (gimp-displays-flush)

    (list inImg inDrawable theLayerNum)

    );end let

);end define

